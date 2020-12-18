

# Packages -------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(civis)
library(ggplot2)
library(tidyverse)
library(plotly)

if(!require(bslib)){
  install.packages("remotes")
  remotes::install_github("rstudio/htmltools")
  remotes::install_github("rstudio/bslib")
  library(bslib)
}

if(!require(ggthemes)){
  install.packages("ggthemes")
  library(ggthemes)
}

# remotes::install_github("rstudio/bslib")
# library(bslib)


# Load data ------------------------------------------------------------------------------



# Sources --------------------------------------------------------------------------------

source("app_util.R")
source("app_global.R")
source("app_ui.R")
source("app_server_v2.R")

# Call the app ---------------------------------------------------------------------------

shinyApp(ui = ui, server = server)