server <- function(input, output, session){
  
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
  
  
  listen4area <- reactive({list(input$selector_sp_aggr, input$selector_country)})
  
  observeEvent(listen4area(), {
    dat <- dat_sp()
    if (input$selector_sp_aggr %in% "admin1") {
      output$ui_area_selection <- renderUI({
        pickerInput(inputId = "selector_area", label = "Province/State",
                    choices = as.character(unique(dat$admin_level_1)),
                    multiple = TRUE, options = list(`actions-box` = TRUE))
      })
    } else if (input$selector_sp_aggr %in% "admin2") {
      output$ui_area_selection <- renderUI({
        pickerInput(inputId = "selector_area", label = "District",
                    choices = as.character(unique(dat$admin_level_2)),
                    multiple = TRUE, options = list(`actions-box` = TRUE))
      })
    } else {
      output$ui_area_selection <- renderUI({NULL})
    }
    
  })
  
  
  observeEvent(input$selector_plot_type, {
    output$ui_plotdescr <- renderUI({plot_descr[[input$selector_plot_type]]})
  })
  
  listen4indi <- reactive({
    list(input$selector_country, input$selector_sp_aggr, input$selector_area, input$selector_time_aggr, input$selector_plot_type)
  })
  
  
  observeEvent(listen4indi(), {
    dat <- dat_sp()
    
    if(!is.null(dat)) {
      
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
                                              choices = unique(dat$label))})
      output$ui_add3 <- renderUI({selectInput(inputId = "selector_indi2",label = "Second Indicator (y-axis)",
                                              choices = unique(dat$label))})
      output$ui_add2 <- renderUI({
        airDatepickerInput(inputId = "selector_indi1_time", label = "Date range for 1st indicator",
                           range = TRUE, view = cal_view, clearButton = TRUE,
                           minView = cal_view,
                           dateFormat = "MM yyyy", 
                           monthsField =  "monthsShort",
                           minDate = cal_min, maxDate = cal_max) })
      
      output$ui_add4 <- renderUI({
        airDatepickerInput(inputId = "selector_indi1_time", label = "Date range for 2nd indicator",
                           range = TRUE, view = cal_view, minDate = cal_min, maxDate = cal_max)})
      
    } else if (input$selector_plot_type %in% "ts" & !is.null(cal_view)) {
      output$ui_add1 <- renderUI({selectInput(inputId = "selector_indi1",label = "First Indicator (primary (left) axis)",
                                              choices = unique(dat$label))})
      output$ui_add2 <- renderUI({selectInput(inputId = "selector_indi2",label = "Second Indicator (secondary (right) axis)",
                                              choices = unique(dat$label))})
      output$ui_add3 <- renderUI({airDatepickerInput(inputId = "selector_indi1_time", label = "Date range for both indicators",
                                                     range = TRUE, view = cal_view, minDate = cal_min, maxDate = cal_max)})
      output$ui_add4 <- renderUI({NULL})
    } else {
      output$ui_add1 <- output$ui_add2 <- output$ui_add3 <- output$ui_add4 <- renderUI({NULL})
    }
    
  })
  
  
  observeEvent(input$bttn_plotme, {
    
    dat0 <- dat_sp()
    
    if (input$selector_plot_type %in% "scatter") {
      
      if (input$selector_time_aggr %in% "year") {
        tt1 <- paste()
      }
      
      tt1 <- as.Date(input$selector_indi1_time)
      dat1 <- dat0 %>%
        filter((label %in% input$selector_indi1) & (date >= tt1[1]) & (date <= tt1[2]))
      
    }

    
  })

  

  

  
  
  
  
} # End of server
