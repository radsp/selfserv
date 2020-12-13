
ui <- fluidPage(id = "main_page",
                 theme = bs_theme(bootswatch = "cerulean", version = "4", base_font = c("Arial", "sans-serif"),
                                  heading_font = "'Helvetica Neue', Helvetica, sans-serif",),
                inverse = TRUE,
                titlePanel(HTML("<br><h5 style='color:#005ea2;font-weight: bold;'>DATA VISUALIZER</h5>")),
                br(),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               selectInput(inputId = "selector_country", label = "Country",
                                           choices = ctry_list),
                               selectInput(inputId = "selector_sp_aggr", 
                                           label = "Administrative Level Aggregation",
                                           choices = c("Country" = "admin0", "Administrative Level 1" = "admin1", "Administrative Level 2" = "admin2")),
                               uiOutput("ui_area_selection"),
                               selectInput(inputId = "selector_time_aggr",
                                           label = "Temporal Aggregation",
                                           choices = c("Year" = "year", "Month" = "month")),
                               selectInput(inputId = "selector_plot_type", 
                                           label = "Plot type",
                                           choices = c("Scatter" = "scatter", "Time-series" = "ts", 
                                                       "Bar/(Histogram?)" = "bar", "Map" = "map")),
                               uiOutput("ui_plotdescr"),
                               br(),
                               uiOutput("ui_add1"),
                               uiOutput("ui_add2"),
                               uiOutput("ui_add3"),
                               uiOutput("ui_add4"),
                               actionBttn(inputId = "bttn_plotme",
                                 label = HTML("Get visualization"), 
                                 style = "pill", color = "primary", size = "sm"
                               )
                  ),
                  mainPanel(plotOutput("myplot"))
                )
                
        

                
                           # tabPanel("Plots"),
                           # tabPanel("About/Info")
  
)