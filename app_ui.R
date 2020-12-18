
ui <- fluidPage(id = "main_page",
                 theme = bs_theme(bootswatch = "cerulean", version = "4", base_font = c("Arial", "sans-serif"),
                                  heading_font = "'Helvetica Neue', Helvetica, sans-serif",),
                inverse = TRUE,
                tags$head(tags$script('var dimension = [0, 0];$(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth; dimension[1] = window.innerHeight; Shiny.onInputChange("dimension", dimension);});
                                      $(window).resize(function(e) {dimension[0] = window.innerWidth; dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension); });')),
                titlePanel(HTML("<br><h5 style='color:#005ea2;font-weight: bold;'>DATA VISUALIZER</h5>"), windowTitle = "Data Visualizer"),
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
                  # mainPanel(plotlyOutput("myplot"))
                  mainPanel(width = 9, uiOutput(outputId = "ui_mainpanel"))
                )
                
        

                
                           # tabPanel("Plots"),
                           # tabPanel("About/Info")
  
)