server <- function(input, output, session){
  
  # Prevent "greying out" when running in Civis Platform
  session$allowReconnect("force")
  
  # handle for screen dimension to enable plot sizing
  screen_dim <- reactive({
    return(as.numeric(input$dimension))
  })
  
  # filter dataset based on the spatial and time aggregation,
  # country and admin level 1
  dat_sp <- reactive({
    if (input$selector_sp_aggr %in% "admin1") {
      if (input$selector_time_aggr %in% "year") {
        dat <- xadm1_yr
      } else {
        dat <- xadm1_mo
      }
      dat <- dat %>% filter(country %in% as.character(input$selector_country))
      # Filter to user-selected province
      if(!is.null(input$selector_area)) {
        dat <- dat %>% filter(admin_level_1 %in% input$selector_area)
      }
    } else {
      dat <- NULL
    }
    return(dat)
  })
  
  
  # Populate subnational data field based on country selection
  observe({
    if (input$selector_sp_aggr %in% "admin1") {
      dat <- xadm1_mo %>% filter(country %in% input$selector_country)
        output$ui_area_selection <- renderUI({
          pickerInput(inputId = "selector_area", label = "Province/State",
                      choices = as.character(unique(dat$admin_level_1)),
                      multiple = TRUE, options = list(`actions-box` = TRUE))
        })
    # Note that for admin level 2, it would probably be more user-friendly
    # if we can include the admin level 1 in the list with collapsible feature
    } else if (input$selector_sp_aggr %in% "admin2") {
      dat <- xadm1_mo %>%      # NEED TO CHANGE THIS TO ADM2 DATA WHEN AVAILABLE !!!
        filter(country %in% input$selector_country) 
      output$ui_area_selection <- renderUI({
        pickerInput(inputId = "selector_area", label = "District",
                    choices = as.character(unique(dat$admin_level_2)),
                    multiple = TRUE, options = list(`actions-box` = TRUE))
      })
    } else {
      output$ui_area_selection <- renderUI({NULL})
    }
  })
  
  # Place plot description below the input selection. Ideally this should be 
  # a tooltip in the above input selection
  observeEvent(input$selector_plot_type, {
    output$ui_plotdescr <- renderUI({plot_descr[[input$selector_plot_type]]})
  })
  
  listen4indi <- reactive({
    list(input$selector_country, input$selector_sp_aggr, input$selector_area, input$selector_time_aggr, input$selector_plot_type)
  })
  
  # Populate indicator and time range field based on the plot type
  observeEvent(listen4indi(), {
    dat <- dat_sp()
    
    if(!is.null(dat)) {
      
      var_list <- unique(dat[, c("label", "variable_order")]) %>%
        arrange(variable_order) %>% pull(label) 
      
      if (input$selector_time_aggr %in% "month") {
        cal_view <- "months"
        cal_min <- as.character(min(dat$date, na.rm = TRUE))
        cal_max <- as.character(max(dat$date, na.rm = TRUE)) 
      } else if (input$selector_time_aggr %in% "year") {
        cal_view <- "years"
        cal_min <- (paste(min(dat$year, na.rm = TRUE), "-01-01", sep = ""))
        cal_max <- (paste(max(dat$year, na.rm = TRUE), "-01-01", sep = ""))
      } else {
        cal_view <- cal_min <- cal_max <- NULL
      }
      
    } else {
      cal_view <- cal_min <- cal_max <- NULL
    }
    
    if (input$selector_plot_type %in% "scatter" & !is.null(cal_view)) {
      output$ui_add1 <- renderUI({selectInput(inputId = "selector_indi1",label = "First Indicator (x-axis)",
                                              choices = var_list)})
      output$ui_add3 <- renderUI({selectInput(inputId = "selector_indi2",label = "Second Indicator (y-axis)",
                                              choices = var_list)})
      output$ui_add2 <- renderUI({
        airDatepickerInput(inputId = "selector_indi1_time", label = "Date range for 1st indicator",
                           range = TRUE, view = cal_view, clearButton = TRUE,
                           minView = cal_view,
                           dateFormat = "MM yyyy", 
                           monthsField =  "monthsShort",
                           minDate = cal_min, maxDate = cal_max) })
      
      output$ui_add4 <- renderUI({
        airDatepickerInput(inputId = "selector_indi2_time", label = "Date range for 2nd indicator",
                           range = TRUE, view = cal_view, clearButton = TRUE,
                           minView = cal_view,
                           dateFormat = "MM yyyy", 
                           monthsField =  "monthsShort",
                           minDate = cal_min, maxDate = cal_max) })
      
    } else if (input$selector_plot_type %in% "ts" & !is.null(cal_view)) {
      output$ui_add1 <- renderUI({selectInput(inputId = "selector_indi1",label = "First Indicator (primary (left) axis)",
                                              choices = var_list)})
      output$ui_add2 <- renderUI({selectInput(inputId = "selector_indi2",label = "Second Indicator (secondary (right) axis)",
                                              choices = var_list)})
      output$ui_add3 <- renderUI({airDatepickerInput(inputId = "selector_indi1_time", label = "Date range for both indicators",
                                                     range = TRUE, view = cal_view, minDate = cal_min, maxDate = cal_max)})
      output$ui_add4 <- renderUI({NULL})
    } else {
      output$ui_add1 <- output$ui_add2 <- output$ui_add3 <- output$ui_add4 <- renderUI({NULL})
    }
    
  })
  
  
  # When the get visualization button is clicked, populate the main panel
  observeEvent(input$bttn_plotme, {
    
    if ((input$selector_sp_aggr %in% c("admin1", "admin2")) & (length(input$selector_area) > 1)) {
        drp_bttn <- pickerInput(inputId = "area2color", label = HTML("<b>Select admin. area to color (up to 10)</b>"),
                                choices = input$selector_area,
                                multiple = TRUE, options = list("max-options" = 10,
                                                                "max-options-text" = "No more!"))
        swtch_bttn <- switchInput(inputId = "selector_panel",
                                  label = "Panel plot", labelWidth = "80px",
                                  value = FALSE,  size = "small")
        radio_bttn <- radioButtons(inputId = "selector_trendline",
                                   label = HTML("<b>Add trendline</b>"),
                                   choices = c("Linear", "Splines", "None"), selected = "None")
    } else {
        drp_bttn <- "TBD"
        swtch_bttn <- NULL
    }
    
    output$ui_mainpanel <- renderUI({
      fluidPage(style = "margin-left:20px;",
                fluidRow(column(10, swtch_bttn), 
                         column(2, style = "padding-right:30px;",
                                dropdown(HTML("<h6 style='color:#005ea2'>PLOT SETTINGS</h6>"),   
                                         drp_bttn,
                                         br(),
                                         radio_bttn,
                                         circle = TRUE, size = "xs", status = "default",
                                         icon = icon("gear"), right = TRUE,
                                         tooltip = tooltipOptions(title = "Plot settings"))
                         )
                 ),
                fluidRow(
                  column(12, plotlyOutput("myplot")),
                ))
    })
    
  })

  
  # Filter data based on user inputs
  get_data <- eventReactive(input$bttn_plotme, {
    dat0 <- dat_sp() 
    validate(need(!is.null(dat0), "Please choose province/state"))
    
    if (input$selector_plot_type %in% "scatter") {
      
      tt1 <- get_tt(input$selector_indi1_time, input$selector_time_aggr)
      tt2 <- get_tt(input$selector_indi2_time, input$selector_time_aggr)
      
      tt1_df <- data.frame(date = tt1) %>% arrange(date) %>% 
        mutate(date_order = paste("dd_", 1:n(), sep = ""))
      tt2_df <- data.frame(date = tt2) %>% arrange(date) %>%
        mutate(date_order = paste("dd_", 1:n(), sep = ""))
      
      dat1 <- filter(dat0, (date %in% tt1) & (label %in% input$selector_indi1))
      dat2 <- filter(dat0, (date %in% tt2) & (label %in% input$selector_indi2)) 
      
      validate(need(length(dat1) > 0, "Selected combination of first indicator and time range is unavailable"))
      validate(need(length(dat2) > 0, "Selected combination of second indicator and time range is unavailable"))
      
      dat1 <- merge(dat1, tt1_df, by = "date")
      dat2 <- merge(dat2, tt2_df, by = "date")
      
      admin_cols <- grep(c("country|admin_level_1|admin_level_2"), names(dat1), value = TRUE)
      
      ddstr_format <- if_else(input$selector_time_aggr %in% "month", "%b %Y", "%Y")
      
      uu <- merge(dat1, dat2, by = c('date_order', admin_cols)) %>%
        rowwise() %>%
        mutate(location = ifelse("admin_level_2" %in% colnames(.), paste(admin_level_2, admin_level_1, country, sep = ", "),
                                 ifelse("admin_level_1" %in% colnames(.), paste(admin_level_1, country, sep = ", "),
                                        country)))
      
      df4plot <- merge(dat1, dat2, by = c("date_order", admin_cols)) %>%
        rowwise() %>%
        mutate(loc = ifelse("admin_level_2" %in% colnames(.), paste(admin_level_2, admin_level_1, country, sep = ", "),
                            ifelse("admin_level_1" %in% colnames(.), paste(admin_level_1, country, sep = ", "),
                                   country))) %>%
        mutate(txt = paste(loc, "<br><br>", 
                           "x-axis: ", format(value.x), "<br>", label.x, "<br>", format(date.x, ddstr_format),
                           "<br><br>", "y-axis: ", format(value.y), "<br>", label.y, "<br>", 
                           format(date.y, ddstr_format), sep = "")) %>%
        ungroup()
      
      
    } else {
      
      df4plot <- NULL
    }
    
    return(df4plot)
    
  })
  
  
  
  output$myplot <- renderPlotly({
    
    df <- get_data()
    
    plot_type <- isolate(input$selector_plot_type)
    time_aggr <- isolate(input$selector_time_aggr)
    sp_aggr <- isolate(input$selector_sp_aggr)
    
    if ((plot_type %in% "scatter") & (!is.null(df))) {
      
      tt1 <- isolate(get_tt(input$selector_indi1_time, input$selector_time_aggr))
      tt2 <- isolate(get_tt(input$selector_indi2_time, input$selector_time_aggr))
      
      ddstr_format <- if_else(isolate(input$selector_time_aggr) %in% "month", "%b %Y", "%Y")
      
      x_label <- ifelse(length(tt1 > 1), paste(unique(df$label.x), " (", format(tt1[1], ddstr_format), " - ",
                                               format(tt1[length(tt1)], ddstr_format), ")", sep = ""),
                        paste(unique(df$label.x), " (", format(tt1[1], ddstr_format), ")"))
      y_label <- ifelse(length(tt2 > 1), paste(unique(df$label.y), " (", format(tt2[1], ddstr_format), " - ",
                                               format(tt2[length(tt2)], ddstr_format), ")", sep = ""),
                        paste(unique(df$label.y), " (", format(tt2[1], ddstr_format), ")"))
      
      gg <- ggplot(df, aes(x = value.x, y = value.y)) + 
        xlab(x_label) + ylab(y_label) +
        theme_minimal()
      
      if(length(input$area2color) > 0) {
        if ( (sp_aggr %in% "admin1") & (input$area2color[1] != "")) {
          df_area <- df %>%
            mutate(area_color = if_else(admin_level_1 %in% input$area2color, admin_level_1, "Others")) %>%
            mutate(area_color = factor(area_color, levels = c(input$area2color, "Others")))
          nin <- length(input$area2color)
          # nout <- length(isolate(input$selector_area)) - nin
          clr_area <- c(clr10[1:nin], "#4d4d4d")
          gg <- gg + geom_point(data = df_area, aes(colour = area_color, text = txt), size = 2.5) +
            scale_colour_manual("", values = clr_area)
        } else {
          gg <- gg + geom_point(aes(text = txt), colour = "#4d4d4d", size = 2.5)
        }

      } else {
        gg <- gg + geom_point(aes(text = txt), colour = "#4d4d4d", size = 2.5)
      }
      
      if ((length(input$selector_trendline) > 0)) {
        if (input$selector_trendline %in% c("Linear", "Splines")) {
              smooth_method <- ifelse(input$selector_trendline %in% "Linear", "lm", "loess")
              gg <- gg + geom_smooth(method = smooth_method)
        }
      }
  
  
      
      
      if(isolate(input$selector_indi1) == isolate(input$selector_indi2)){
        gg <- gg + 
          geom_abline(slope=1, intercept=0, linetype = "dotted", alpha = 0.5, aes(text = "Slope of 1 (x-value = y-value)"))
      }
      
      
      
    } else {
      gg <- NULL
    }
    
    scrn_sz <- screen_dim()
    ggplotly(gg, tooltip = "text", width = (0.6 * scrn_sz[1]), height = 0.75 * scrn_sz[2])
    
  })
  
  
  
} # End of server
