library(shiny)
library(leaflet)
library(sf)
library(raster)
library(httr)
library(jsonlite)
library(DT)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Cascade-Siskiyou Soil Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map"),
      menuItem("Soil Properties", tabName = "properties"),
      menuItem("Data Export", tabName = "export")
    ),
    
    # Data source selection
    checkboxGroupInput("data_sources",
                       "Data Sources:",
                       choices = list(
                         "SSURGO (SoilWeb)" = "ssurgo",
                         "SoilGrids 250m" = "soilgrids",
                         "Soil Explorer" = "soil_explorer"
                       ),
                       selected = c("ssurgo", "soilgrids")
    ),
    
    # Property selection
    selectInput("soil_property",
                "Soil Property:",
                choices = list(
                  "Soil Classification" = "classification",
                  "pH" = "ph",
                  "Organic Carbon" = "oc",
                  "Bulk Density" = "bd",
                  "Clay Content" = "clay",
                  "Sand Content" = "sand"
                )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, status = "primary",
                    leafletOutput("soil_map", height = "600px")
                )
              ),
              fluidRow(
                box(width = 6, title = "Point Information",
                    verbatimTextOutput("point_info")
                ),
                box(width = 6, title = "Soil Profile",
                    plotlyOutput("soil_profile")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize map
  output$soil_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -122.5, lat = 42.0, zoom = 11) %>%
      
      # Add monument boundary
      addPolygons(
        data = monument_boundary,
        fillOpacity = 0.1,
        weight = 2,
        color = "red"
      ) %>%
      
      # Add click event
      htmlwidgets::onRender("
        function(el, x) {
          this.on('click', function(e) {
            Shiny.setInputValue('map_click', {
              lat: e.latlng.lat,
              lng: e.latlng.lng
            });
          });
        }
      ")
  })
  
  # Add WMS layers based on selection
  observe({
    leafletProxy("soil_map") %>%
      clearGroup("soil_layers")
    
    if("ssurgo" %in% input$data_sources) {
      leafletProxy("soil_map") %>%
        addWMSTiles(
          baseUrl = "https://casoilresource.lawr.ucdavis.edu/cgi-bin/mapserv",
          layers = "soilmu_a",
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            map = "/var/www/html/soilweb/mapfile/soilweb.map"
          ),
          group = "soil_layers"
        )
    }
    
    if("soilgrids" %in% input$data_sources) {
      # Add SoilGrids WMS layer
      leafletProxy("soil_map") %>%
        addWMSTiles(
          baseUrl = "https://maps.isric.org/mapserv",
          layers = paste0("phh2o_0-5cm_mean"),
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            map = "/map/phh2o.map"
          ),
          group = "soil_layers"
        )
    }
  })
  
  # Handle map clicks
  observeEvent(input$map_click, {
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    
    # Query all selected data sources
    soil_data <- query_soil_data(lat, lng, input$data_sources)
    
    output$point_info <- renderText({
      paste0(
        "Coordinates: ", round(lat, 4), ", ", round(lng, 4), "\n",
        "SSURGO Map Unit: ", soil_data$ssurgo$muname, "\n",
        "Soil Series: ", soil_data$ssurgo$compname, "\n",
        "pH (0-5cm): ", soil_data$soilgrids$ph_0_5, "\n",
        "Organic Carbon: ", soil_data$soilgrids$oc_0_5, " g/kg"
      )
    })
  })
}

query_soil_data <- function(lat, lng, sources) {
  results <- list()
  
  if("ssurgo" %in% sources) {
    results$ssurgo <- query_ssurgo(lat, lng)
  }
  
  if("soilgrids" %in% sources) {
    results$soilgrids <- query_soilgrids(lat, lng)
  }
  
  return(results)
}