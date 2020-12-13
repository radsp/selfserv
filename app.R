

# Packages -------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(civis)
library(ggplot2)
library(tidyverse)



# remotes::install_github("rstudio/bslib")
library(bslib)


# Load data ------------------------------------------------------------------------------



# Sources --------------------------------------------------------------------------------

source("app_global.R")
source("app_ui.R")
source("app_server.R")

# Call the app ---------------------------------------------------------------------------

shinyApp(ui = ui, server = server)