# app.R - Main Application File (Refactored)
# Cascade-Siskiyou National Monument Soil Explorer

# Load required libraries ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(sf)
  library(leaflet)
  library(raster)
  library(htmlwidgets)
  library(magrittr)
  library(DT)
  library(plotly)
  library(dplyr)  # Load dplyr AFTER raster to avoid select() conflicts
})

# Source modular components ----
source("config.R")
source("data_module.R")
source("ui_module.R")
source("map_utils.R")
source("plot_utils.R")
source("server_module.R")

# Define UI ----
ui <- create_dashboard_ui()

# Define Server ----
server <- function(input, output, session) {
  create_server(input, output, session)
}

# Run the application ----
shinyApp(ui = ui, server = server)