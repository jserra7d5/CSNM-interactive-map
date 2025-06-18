# ui_module.R - User Interface Components

#' Create the main dashboard UI
#' @return dashboardPage object
create_dashboard_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Cascade-Siskiyou Soil Explorer"),
    create_sidebar(),
    create_main_body()
  )
}

#' Create the dashboard sidebar
#' @return dashboardSidebar object
create_sidebar <- function() {
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Data Info", tabName = "info", icon = icon("info-circle"))
    ),
    
    hr(),
    
    create_layer_controls(),
    create_boundary_controls(),
    create_selection_info_panel()
  )
}

#' Create layer control inputs
#' @return List of UI elements for layer controls
create_layer_controls <- function() {
  list(
    radioButtons(
      "map_type",
      "Map Layer:",
      choices = list(
        "Soil Orders" = "soil",
        "Organic Carbon" = "oc", 
        "Soil pH" = "ph",
        "Satellite" = "satellite"
      ),
      selected = "soil"
    ),
    
    # Depth selector for OC
    conditionalPanel(
      condition = "input.map_type == 'oc'",
      selectInput(
        "oc_depth",
        "Organic Carbon Depth:",
        choices = setNames(1:6, DEPTH_LEVELS$labels),
        selected = 1
      )
    ),
    
    # Depth selector for pH
    conditionalPanel(
      condition = "input.map_type == 'ph'",
      selectInput(
        "ph_depth", 
        "pH Depth:",
        choices = setNames(1:6, DEPTH_LEVELS$labels),
        selected = 1
      )
    )
  )
}

#' Create boundary control inputs
#' @return UI element for boundary controls
create_boundary_controls <- function() {
  list(
    hr(),
    checkboxInput(
      "show_boundaries",
      "Show Map Unit Boundaries",
      value = FALSE
    )
  )
}

#' Create selection information panel
#' @return List of UI elements for selection info
create_selection_info_panel <- function() {
  list(
    conditionalPanel(
      condition = "output.has_polygon_data && output.has_click_data",
      hr(),
      h5("Selected Point Info:"),
      verbatimTextOutput("selection_info", placeholder = TRUE),
      
      hr(),
      h5("Soil Profile:"),
      plotlyOutput("soil_profile", height = "300px")
    )
  )
}

#' Create the main dashboard body
#' @return dashboardBody object
create_main_body <- function() {
  dashboardBody(
    create_custom_css(),
    create_tab_items()
  )
}

#' Create custom CSS styles
#' @return tags$head object with CSS
create_custom_css <- function() {
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f4f4;
      }
      .leaflet-container {
        background: #f8f8f8;
      }
      #soil-order-legend {
        position: absolute;
        top: 10px;
        left: 10px;
        z-index: 1000;
      }
      #mouse-coords {
        position: absolute;
        bottom: 30px;
        right: 10px;
        background: rgba(255,255,255,0.8);
        padding: 5px 10px;
        border-radius: 4px;
        font-size: 12px;
        z-index: 1000;
      }
      .shiny-text-output {
        white-space: pre-wrap !important;
        word-wrap: break-word !important;
        overflow-wrap: break-word !important;
        hyphens: auto !important;
        word-break: keep-all !important;
      }
      .shiny-text-output::before {
        content: '';
      }
    "))
  )
}

#' Create tab items for the dashboard
#' @return tabItems object
create_tab_items <- function() {
  tabItems(
    create_map_tab(),
    create_info_tab()
  )
}

#' Create the map tab content
#' @return tabItem object for the map
create_map_tab <- function() {
  tabItem(
    tabName = "map",
    fluidRow(
      box(
        width = 12,
        status = "primary", 
        title = "Interactive Soil Map",
        create_map_container()
      )
    )
  )
}

#' Create the map container with overlays
#' @return div object containing the map and overlays
create_map_container <- function() {
  div(
    style = "position: relative; height: 70vh;",
    leafletOutput("main_map", width = "100%", height = "100%"),
    
    conditionalPanel(
      condition = "input.map_type == 'soil'",
      create_legend_html()
    ),
    
    div(
      id = "mouse-coords",
      textOutput("mouse_coordinates")
    )
  )
}

#' Create the info tab content
#' @return tabItem object for the info page
create_info_tab <- function() {
  tabItem(
    tabName = "info",
    fluidRow(
      box(
        width = 12,
        title = "Dataset Information",
        status = "info",
        create_info_content()
      )
    )
  )
}

#' Create the content for the info tab
#' @return List of HTML elements with dataset information
create_info_content <- function() {
  list(
    h4("Data Sources"),
    tags$ul(
      tags$li("Soil Survey Geographic Database (SSURGO)"),
      tags$li("SoilGrids 250m Resolution (ISRIC) - Multi-depth profiles"),
      tags$li("USDA Soil Taxonomy Classification")
    ),
    
    h4("Available Depths"),
    tags$ul(
      lapply(DEPTH_LEVELS$labels, function(depth) tags$li(depth))
    ),
    
    h4("Map Layers"),
    tags$ul(
      tags$li(strong("Soil Orders:"), "USDA Soil Taxonomy at the order level"),
      tags$li(strong("Organic Carbon:"), "Multiple depths, g/kg"),
      tags$li(strong("Soil pH:"), "Multiple depths, decimal units")
    ),
    
    h4("Interactive Features"),
    tags$ul(
      tags$li("Click any location to see soil profiles"),
      tags$li("Select different depths for raster visualization"),
      tags$li("Toggle map unit boundaries"),
      tags$li("View detailed soil component information")
    )
  )
}

#' Create soil order legend HTML
#' @return tags$div object with the legend
create_legend_html <- function() {
  # Order the legend items - put Unknown at the end
  ordered_soil_orders <- c(
    "Alfisols", "Andisols", "Aridisols", "Entisols", "Gelisols", 
    "Histosols", "Inceptisols", "Mollisols", "Oxisols", 
    "Spodosols", "Ultisols", "Vertisols", "Unknown"
  )
  
  # Only include soil orders that are actually in our data
  available_orders <- names(SOIL_ORDER_COLORS)
  display_orders <- ordered_soil_orders[ordered_soil_orders %in% available_orders]
  
  legend_items <- lapply(display_orders, function(order) {
    color <- SOIL_ORDER_COLORS[order]
    tags$div(
      style = "display: flex; align-items: center; margin-bottom: 4px;",
      tags$div(
        style = sprintf(
          "width: 16px; height: 16px; background: %s; margin-right: 6px; border: 1px solid #666;",
          color
        )
      ),
      tags$span(order, style = "font-size: 12px;")
    )
  })
  
  tags$div(
    id = "soil-order-legend",
    style = "background: rgba(255,255,255,0.9); padding: 10px; border-radius: 8px; width: 140px;",
    tags$h4("Soil Orders", style = "margin: 0 0 6px; font-size: 14px;"),
    legend_items
  )
}