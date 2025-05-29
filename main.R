# app.R - Refactored CSNM Interactive Soil Map with Multi-Depth Support
# Cascade-Siskiyou National Monument Soil Explorer

# Load libraries ----
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

# Configuration ----
PROJECTION_CRS <- CRS("+init=EPSG:3857")
MAP_CENTER <- list(lat = 42.1, lng = -122.466, zoom = 11)

# Depth configuration with improved color schemes
DEPTH_LEVELS <- list(
  labels = c("0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm", "100-200 cm"),
  values = c("0_5", "5_15", "15_30", "30_60", "60_100", "100_200"),
  band_names = list(
    oc = c("soc_0.5cm_mean", "soc_5.15cm_mean", "soc_15.30cm_mean", 
           "soc_30.60cm_mean", "soc_60.100cm_mean", "soc_100.200cm_mean"),
    ph = c("phh2o_0.5cm_mean", "phh2o_5.15cm_mean", "phh2o_15.30cm_mean",
           "phh2o_30.60cm_mean", "phh2o_60.100cm_mean", "phh2o_100.200cm_mean")
  ),
  depths_cm = c(2.5, 10, 22.5, 45, 80, 150),  # Mid-points for profile plotting
  # Depth-specific color palettes for better visualization
  color_palettes = list(
    oc = list(
      surface = c("#FFF8DC", "#DEB887", "#D2691E", "#8B4513", "#654321"),  # Surface: light to dark brown
      shallow = c("#F5F5DC", "#DDD7AA", "#C19A6B", "#8B7355", "#5D4E37"),  # Shallow: beige to brown
      deep = c("#F0F8FF", "#B0C4DE", "#4682B4", "#2F4F4F", "#1C1C1C")     # Deep: light blue to dark (for low values)
    ),
    ph = c("#0000FF", "#4169E1", "#00BFFF", "#32CD32", "#FFFF00", "#FFA500", "#FF4500", "#FF0000")  # Blue to red
  )
)

# Data loading and preprocessing ----
load_and_prepare_data <- function() {
  
  # Load multi-band raster stacks
  cat("Loading raster data...\n")
  
  # Organic Carbon stack
  oc_stack <- tryCatch({
    stack("CSNM_OC_AllDepths.tif")
  }, error = function(e) {
    warning("Could not load OC multi-band raster: ", e$message)
    NULL
  })
  
  # pH stack  
  ph_stack <- tryCatch({
    stack("CSNM_pH_AllDepths.tif")
  }, error = function(e) {
    warning("Could not load pH multi-band raster: ", e$message)
    NULL
  })
  
  # Process raster stacks if available
  raster_data <- list()
  
  if (!is.null(oc_stack)) {
    cat("Processing OC stack with", nlayers(oc_stack), "bands\n")
    
    # Reproject stack to Web Mercator
    oc_proj <- projectRaster(oc_stack, crs = PROJECTION_CRS, method = "ngb")
    
    # Create individual processed layers for each depth
    raster_data$oc <- list()
    for (i in 1:nlayers(oc_proj)) {
      layer <- oc_proj[[i]]
      values <- na.omit(getValues(layer))
      unique_vals <- sort(unique(values))
      
      domain <- if(length(unique_vals) > 1) c(unique_vals[2], max(unique_vals)) else range(unique_vals)
      
      # Choose color palette based on depth
      if (i <= 2) {
        # Surface layers (0-5, 5-15cm): traditional brown palette
        color_palette <- DEPTH_LEVELS$color_palettes$oc$surface
      } else if (i <= 4) {
        # Shallow layers (15-30, 30-60cm): muted browns
        color_palette <- DEPTH_LEVELS$color_palettes$oc$shallow
      } else {
        # Deep layers (60-100, 100-200cm): blue-gray palette for low values
        color_palette <- DEPTH_LEVELS$color_palettes$oc$deep
      }
      
      raster_data$oc[[i]] <- list(
        raster = layer,
        domain = domain,
        palette = colorNumeric(color_palette, domain = domain, na.color = "transparent")
      )
    }
  }
  
  if (!is.null(ph_stack)) {
    cat("Processing pH stack with", nlayers(ph_stack), "bands\n")
    
    # Reproject and convert pH (divide by 10 for decimal pH)
    ph_proj <- projectRaster(ph_stack, crs = PROJECTION_CRS, method = "ngb")
    ph_decimal <- ph_proj / 10
    
    # Create individual processed layers for each depth
    raster_data$ph <- list()
    for (i in 1:nlayers(ph_decimal)) {
      layer <- ph_decimal[[i]]
      values <- na.omit(getValues(layer))
      unique_vals <- sort(unique(values))
      
      domain <- if(length(unique_vals) > 1) c(unique_vals[2], max(unique_vals)) else range(unique_vals)
      
      raster_data$ph[[i]] <- list(
        raster = layer,
        domain = domain,
        palette = colorNumeric(DEPTH_LEVELS$color_palettes$ph, domain = domain, na.color = "transparent")
      )
    }
  }
  
  # Load polygon data
  cat("Loading polygon data...\n")
  mapunit_table <- tryCatch({
    read.csv("Mapunit_OR_table.csv", stringsAsFactors = FALSE) %>%
      dplyr::rename(MUKEY = mukey, muname = muname) %>%
      dplyr::mutate(MUKEY = as.character(MUKEY))
  }, error = function(e) {
    warning("Could not load mapunit table: ", e$message)
    data.frame(MUKEY = character(), muname = character())
  })
  
  soil_polygons <- tryCatch({
    st_read("CSNM_Polygons_with_Data.geojson", quiet = TRUE) %>%
      st_transform(4326) %>%
      st_cast("POLYGON") %>%
      dplyr::mutate(
        MUKEY = as.character(MUKEY),
        taxorder = ifelse(is.na(taxorder) | taxorder == "", "Unknown", taxorder)
      ) %>%
      dplyr::left_join(mapunit_table, by = "MUKEY") %>%
      st_simplify(dTolerance = 0.0001)
  }, error = function(e) {
    warning("Could not load soil polygons: ", e$message)
    NULL
  })
  
  if (is.null(soil_polygons)) {
    return(list(polygons = NULL, components = NULL, rasters = raster_data))
  }
  
  # Add unique ID for each polygon
  soil_polygons$polygon_id <- seq_len(nrow(soil_polygons))
  
  # Component information
  component_info <- soil_polygons %>%
    st_drop_geometry() %>%
    dplyr::distinct(MUKEY, compname, comppct_r, majcompflag, taxorder) %>%
    dplyr::filter(!is.na(MUKEY))
  
  # Major taxonomic order for each map unit
  major_taxorder <- component_info %>%
    dplyr::group_by(MUKEY) %>%
    dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(MUKEY, major_taxorder = taxorder)
  
  # Join major order back to polygons and clean up soil orders
  soil_polygons <- soil_polygons %>%
    dplyr::left_join(major_taxorder, by = "MUKEY") %>%
    dplyr::mutate(
      # Clean up soil order names and handle missing/unknown values
      major_taxorder = case_when(
        is.na(major_taxorder) | major_taxorder == "" ~ "Unknown",
        major_taxorder == "Gelisol" ~ "Gelisols",  # Fix singular forms
        major_taxorder == "Alfisol" ~ "Alfisols",
        major_taxorder == "Andisol" ~ "Andisols", 
        major_taxorder == "Aridisol" ~ "Aridisols",
        major_taxorder == "Entisol" ~ "Entisols",
        major_taxorder == "Histosol" ~ "Histosols",
        major_taxorder == "Inceptisol" ~ "Inceptisols",
        major_taxorder == "Mollisol" ~ "Mollisols",
        major_taxorder == "Oxisol" ~ "Oxisols",
        major_taxorder == "Spodosol" ~ "Spodosols",
        major_taxorder == "Ultisol" ~ "Ultisols",
        major_taxorder == "Vertisol" ~ "Vertisols",
        !major_taxorder %in% names(SOIL_ORDER_COLORS) ~ "Unknown",  # Catch any others
        TRUE ~ major_taxorder
      )
    )
  
  return(list(
    polygons = soil_polygons,
    components = component_info,
    rasters = raster_data
  ))
}

# Soil order color palette ----
SOIL_ORDER_COLORS <- c(
  Alfisols = "#B5D55D", Andisols = "#EA028C", Aridisols = "#FDDCB9",
  Entisols = "#75CDD6", Gelisols = "#31A4BF", Histosols = "#AE5044",
  Inceptisols = "#CB7662", Mollisols = "#00A551", Oxisols = "#EC1F25",
  Spodosols = "#D4BEC4", Ultisols = "#FAAF19", Vertisols = "#FFF100",
  Unknown = "#808080"
)

create_soil_order_palette <- function() {
  # Get all unique soil orders from the data, including any unexpected ones
  all_orders <- unique(c(names(SOIL_ORDER_COLORS), "Unknown"))
  
  # Ensure we have colors for all possible values
  colors <- SOIL_ORDER_COLORS[all_orders]
  names(colors) <- all_orders
  
  # Use na.color for any values not in our defined palette
  colorFactor(colors, domain = all_orders, na.color = "#808080")
}

# UI Components ----
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

# Function to extract soil profile data at a point - optimized version
extract_soil_profile <- function(lat, lng, raster_data, property = "oc") {
  if (is.null(raster_data[[property]]) || length(raster_data[[property]]) == 0) {
    return(NULL)
  }
  
  # Create point more efficiently - avoid sp package
  point_sf <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  point_proj <- st_transform(point_sf, PROJECTION_CRS)
  
  # Convert to coordinates for raster extraction
  coords <- st_coordinates(point_proj)
  
  values <- numeric(length(raster_data[[property]]))
  
  # Extract from all layers at once - more efficient
  for (i in seq_along(raster_data[[property]])) {
    tryCatch({
      val <- raster::extract(raster_data[[property]][[i]]$raster, coords)
      values[i] <- if(is.na(val) || length(val) == 0) NA else val[1]
    }, error = function(e) {
      values[i] <<- NA
    })
  }
  
  # Return data frame with proper handling of missing values
  data.frame(
    depth = DEPTH_LEVELS$depths_cm,
    depth_label = DEPTH_LEVELS$labels,
    value = values,
    property = property,
    stringsAsFactors = FALSE
  )
}

# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Cascade-Siskiyou Soil Explorer"),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Data Info", tabName = "info", icon = icon("info-circle"))
    ),
    
    hr(),
    
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
    ),
    
    hr(),
    
    checkboxInput(
      "show_boundaries",
      "Show Map Unit Boundaries",
      value = FALSE
    ),
    
    conditionalPanel(
      condition = "output.has_polygon_data && output.has_click_data",
      hr(),
      h5("Selected Point Info:"),
      verbatimTextOutput("selection_info", placeholder = TRUE),
      
      hr(),
      h5("Soil Profile:"),
      plotlyOutput("soil_profile", height = "300px")
    )
  ),
  
  dashboardBody(
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
        /* Fix verbatim text output wrapping and add bold formatting */
        .shiny-text-output {
          white-space: pre-wrap !important;
          word-wrap: break-word !important;
          overflow-wrap: break-word !important;
          hyphens: auto !important;
          word-break: keep-all !important;
        }
        /* Simple bold formatting for field labels */
        .shiny-text-output::before {
          content: '';
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 12,
            status = "primary", 
            title = "Interactive Soil Map",
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
          )
        )
      ),
      
      tabItem(
        tabName = "info",
        fluidRow(
          box(
            width = 12,
            title = "Dataset Information",
            status = "info",
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
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  cat("=== SERVER FUNCTION STARTING ===\n")
  
  # Load data on startup
  cat("=== LOADING DATA ===\n")
  app_data <- load_and_prepare_data()
  cat("Data loading complete\n")
  cat("Polygons available:", !is.null(app_data$polygons), "\n")
  if (!is.null(app_data$polygons)) {
    cat("Number of polygons:", nrow(app_data$polygons), "\n")
  }
  cat("OC raster available:", !is.null(app_data$rasters$oc), "\n")
  cat("pH raster available:", !is.null(app_data$rasters$ph), "\n")
  
  # Reactive values to store click information
  click_data <- reactiveValues(
    lat = NULL,
    lng = NULL,
    has_data = FALSE
  )
  
  # Test if ANY Shiny events are working
  observe({
    cat("Map type changed to:", input$map_type, "\n")
  })
  
  observe({
    cat("Boundaries toggle:", input$show_boundaries, "\n")
  })
  
  # Check if polygon data is available
  output$has_polygon_data <- reactive({
    !is.null(app_data$polygons)
  })
  outputOptions(output, "has_polygon_data", suspendWhenHidden = FALSE)
  
  # Check if click data is available
  output$has_click_data <- reactive({
    click_data$has_data
  })
  outputOptions(output, "has_click_data", suspendWhenHidden = FALSE)
  
  # Create base map
  output$main_map <- renderLeaflet({
    cat("=== renderLeaflet CALLED ===\n")
    cat("Creating base map...\n")
    
    tryCatch({
      map <- leaflet() %>%
        setView(lng = MAP_CENTER$lng, lat = MAP_CENTER$lat, zoom = MAP_CENTER$zoom) %>%
        addProviderTiles(
          providers$Esri.WorldTerrain,
          group = "terrain",
          options = providerTileOptions(opacity = 0.8)
        ) %>%
        addProviderTiles(
          providers$Esri.WorldImagery,
          group = "satellite"
        )
      
      cat("Base map tiles added successfully\n")
      
      # Add polygon layers if available
      if (!is.null(app_data$polygons)) {
        cat("=== ADDING POLYGON LAYERS ===\n")
        # Debug: Check what soil orders we actually have
        unique_orders <- unique(app_data$polygons$major_taxorder)
        cat("Unique soil orders in data:", paste(unique_orders, collapse = ", "), "\n")
        
        # Check colors for each
        soil_palette <- create_soil_order_palette()
        for (order in unique_orders) {
          if (order %in% names(SOIL_ORDER_COLORS)) {
            cat("Order:", order, "Color:", SOIL_ORDER_COLORS[order], "\n")
          } else {
            cat("Order:", order, "NOT IN COLOR PALETTE\n")
          }
        }
        
        cat("Adding", nrow(app_data$polygons), "polygons to map\n")
        cat("Sample polygon_ids:", head(app_data$polygons$polygon_id, 10), "\n")
        
        map <- map %>%
          addPolygons(
            data = app_data$polygons,
            layerId = ~polygon_id,
            group = "soil_fill",
            fillColor = ~soil_palette(major_taxorder),
            fillOpacity = 0.6,
            color = "white",
            weight = 0,
            smoothFactor = 0.2,
            # Add explicit click event
            options = pathOptions(clickable = TRUE)
          ) %>%
          addPolygons(
            data = app_data$polygons,
            layerId = ~polygon_id,
            group = "soil_boundaries",
            fillOpacity = 0,
            color = "#FFD700",  # Yellow boundaries
            weight = 1.5,
            smoothFactor = 0.2,
            highlightOptions = highlightOptions(
              color = "#FFA500",  # Orange highlight on hover
              weight = 3,
              bringToFront = TRUE
            ),
            # Add explicit click event
            options = pathOptions(clickable = TRUE)
          )
        
        cat("Polygons added to map successfully\n")
      } else {
        cat("WARNING: No polygon data available\n")
      }
      
      cat("=== PROCESSING RASTER LAYERS ===\n")
      # Add all raster layers (hidden initially)
      if (!is.null(app_data$rasters$oc)) {
        cat("Adding", length(app_data$rasters$oc), "OC raster layers\n")
        for (i in 1:length(app_data$rasters$oc)) {
          map <- map %>%
            addRasterImage(
              app_data$rasters$oc[[i]]$raster,
              colors = app_data$rasters$oc[[i]]$palette,
              opacity = 0.8,
              group = paste0("oc_", i),
              project = FALSE
            )
        }
        cat("OC layers added\n")
      } else {
        cat("No OC raster data\n")
      }
      
      if (!is.null(app_data$rasters$ph)) {
        cat("Adding", length(app_data$rasters$ph), "pH raster layers\n")
        for (i in 1:length(app_data$rasters$ph)) {
          map <- map %>%
            addRasterImage(
              app_data$rasters$ph[[i]]$raster,
              colors = app_data$rasters$ph[[i]]$palette,
              opacity = 0.8,
              group = paste0("ph_", i),
              project = FALSE
            )
        }
        cat("pH layers added\n")
      } else {
        cat("No pH raster data\n")
      }
      
      # Define all possible groups
      all_groups <- c("terrain", "satellite", "soil_fill", "soil_boundaries",
                      paste0("oc_", 1:6), paste0("ph_", 1:6))
      
      cat("Setting initial layer visibility...\n")
      # Set initial layer visibility and enable mouse tracking
      map <- map %>%
        showGroup(c("terrain", "soil_fill")) %>%
        hideGroup(setdiff(all_groups, c("terrain", "soil_fill"))) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var map = this;
            console.log('Map render function called');
            
            // Basic click test
            map.on('click', function(e) {
              console.log('Raw Leaflet click at:', e.latlng.lat, e.latlng.lng);
              Shiny.setInputValue('test_click', {
                lat: e.latlng.lat,
                lng: e.latlng.lng,
                timestamp: Date.now()
              }, {priority: 'event'});
            });
            
            // Mouse move tracking
            map.on('mousemove', function(e) {
              Shiny.setInputValue('main_map_mousemove', {
                lat: e.latlng.lat,
                lng: e.latlng.lng
              }, {priority: 'event'});
            });
          }
        ")
      
      cat("=== MAP CREATION COMPLETE ===\n")
      return(map)
      
    }, error = function(e) {
      cat("ERROR in renderLeaflet:", e$message, "\n")
      cat("Error details:", str(e), "\n")
      return(leaflet() %>% setView(lng = -122.5, lat = 42.0, zoom = 8))
    })
  })
  
  # Handle layer switching
  observe({
    req(input$map_type)
    
    proxy <- leafletProxy("main_map")
    
    # Clear existing legends, popups, highlights, and markers when switching layers
    proxy %>% 
      clearControls() %>%
      clearPopups() %>%
      clearGroup("highlight") %>%
      clearGroup("click_marker")
    
    # Reset click data when switching layers
    click_data$has_data <- FALSE
    click_data$lat <- NULL
    click_data$lng <- NULL
    
    # Hide all groups first
    all_groups <- c("terrain", "satellite", "soil_fill", "soil_boundaries",
                    paste0("oc_", 1:6), paste0("ph_", 1:6))
    proxy %>% hideGroup(all_groups)
    
    # Show appropriate layers based on selection
    if (input$map_type == "soil") {
      proxy %>% showGroup(c("terrain", "soil_fill"))
      
    } else if (input$map_type == "satellite") {
      proxy %>% showGroup("satellite")
      
    } else if (input$map_type == "oc" && !is.null(app_data$rasters$oc)) {
      depth_idx <- as.numeric(input$oc_depth %||% 1)
      if (depth_idx <= length(app_data$rasters$oc)) {
        # Create custom label format with guaranteed min/max ticks
        domain <- app_data$rasters$oc[[depth_idx]]$domain
        
        # Create tick positions that include min, max, and 3-4 intermediate values
        tick_values <- c(
          domain[1],
          seq(domain[1], domain[2], length.out = 5)[2:4],
          domain[2]
        )
        
        proxy %>% 
          showGroup(c("terrain", paste0("oc_", depth_idx))) %>%
          addLegend(
            pal = app_data$rasters$oc[[depth_idx]]$palette,
            values = tick_values,
            title = paste("Organic Carbon<br>(g/kg)", DEPTH_LEVELS$labels[depth_idx]),
            position = "topleft",
            opacity = 1,
            labFormat = labelFormat(
              digits = 1,
              transform = function(x) sort(x)  # Ensure proper ordering
            )
          )
      }
      
    } else if (input$map_type == "ph" && !is.null(app_data$rasters$ph)) {
      depth_idx <- as.numeric(input$ph_depth %||% 1)
      if (depth_idx <= length(app_data$rasters$ph)) {
        # Create custom tick values for pH scale
        domain <- app_data$rasters$ph[[depth_idx]]$domain
        
        # Create tick positions that include min, max, and intermediate values
        tick_values <- c(
          domain[1],
          seq(domain[1], domain[2], length.out = 6)[2:5],
          domain[2]
        )
        
        proxy %>%
          showGroup(c("terrain", paste0("ph_", depth_idx))) %>%
          addLegend(
            pal = app_data$rasters$ph[[depth_idx]]$palette,
            values = tick_values,
            title = paste("Soil pH<br>", DEPTH_LEVELS$labels[depth_idx]),
            position = "topleft",
            opacity = 1,
            labFormat = labelFormat(
              digits = 1,
              transform = function(x) sort(x)  # Ensure proper ordering
            )
          )
      }
    }
    
    # Handle boundary overlay
    if (input$show_boundaries && !is.null(app_data$polygons)) {
      proxy %>% showGroup("soil_boundaries")
    } else {
      proxy %>% hideGroup("soil_boundaries")
    }
  })
  
  # Handle map clicks for both polygon info and soil profiles
  observeEvent(input$main_map_click, {
    click <- input$main_map_click
    if (is.null(click)) return()
    
    # Store click data
    click_data$lat <- click$lat
    click_data$lng <- click$lng
    click_data$has_data <- TRUE
    
    # Add marker at clicked location
    leafletProxy("main_map") %>%
      clearGroup("click_marker") %>%
      addCircleMarkers(
        lng = click$lng,
        lat = click$lat,
        group = "click_marker",
        radius = 8,
        color = "#FFD700",
        stroke = TRUE,
        weight = 3,
        fillOpacity = 0.7,
        fillColor = "#FF6B6B"
      )
  })
  
  # Handle polygon shape clicks for detailed info
  observeEvent(input$main_map_shape_click, {
    if (is.null(app_data$polygons)) return()
    
    click <- input$main_map_shape_click
    polygon_id <- click$id
    
    if (is.null(polygon_id)) return()
    
    # Clear previous popups and highlights when clicking on a new polygon
    leafletProxy("main_map") %>%
      clearPopups() %>%
      clearGroup("highlight")
    
    # Find clicked polygon
    selected_polygon <- app_data$polygons[app_data$polygons$polygon_id == polygon_id, ]
    
    if (nrow(selected_polygon) == 0) return()
    
    # Get component information
    components <- app_data$components %>%
      dplyr::filter(MUKEY == selected_polygon$MUKEY) %>%
      dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r))
    
    # Create popup content
    popup_content <- create_popup_content(selected_polygon, components)
    
    # Add popup and highlight
    leafletProxy("main_map") %>%
      addPolygons(
        data = selected_polygon,
        group = "highlight",
        color = "#FFD700",
        weight = 4,
        fillOpacity = 0.1,
        fillColor = "#FFD700"
      ) %>%
      addPopups(
        lng = click$lng,
        lat = click$lat,
        popup = popup_content,
        options = popupOptions(maxWidth = 300, closeOnClick = FALSE)
      )
    
    # Also update the click data for soil profile
    click_data$lat <- click$lat
    click_data$lng <- click$lng
    click_data$has_data <- TRUE
    
    # Add marker for polygon clicks too
    leafletProxy("main_map") %>%
      clearGroup("click_marker") %>%
      addCircleMarkers(
        lng = click$lng,
        lat = click$lat,
        group = "click_marker",
        radius = 8,
        color = "#FFD700",
        stroke = TRUE,
        weight = 3,
        fillOpacity = 0.7,
        fillColor = "#FF6B6B"
      )
  }, priority = 10)  # Higher priority than map click
  
  # Update selection info in sidebar
  output$selection_info <- renderText({
    if (!click_data$has_data) {
      return("Click anywhere on map to extract soil data")
    }
    
    lat <- click_data$lat
    lng <- click_data$lng
    
    # Try to get polygon info if available - more efficient approach
    polygon_info <- ""
    if (!is.null(app_data$polygons)) {
      # Create point and find intersection more efficiently
      point_sf <- st_sfc(st_point(c(lng, lat)), crs = 4326)
      
      # Use st_filter for better performance with large polygon datasets
      nearby_polygons <- st_filter(app_data$polygons, point_sf)
      
      if (nrow(nearby_polygons) > 0) {
        # Find the actual intersection
        intersected <- st_intersection(point_sf, nearby_polygons)
        if (length(intersected) > 0) {
          # Get the first intersected polygon's attributes
          attrs <- st_drop_geometry(nearby_polygons[1, ])
          
          # Get detailed component information
          components <- app_data$components %>%
            dplyr::filter(MUKEY == attrs$MUKEY) %>%
            dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r))
          
          # Build component list
          component_text <- ""
          if (nrow(components) > 0) {
            component_text <- "Components:\n"
            for (i in 1:nrow(components)) {
              comp <- components[i, ]
              major_flag <- if (comp$majcompflag == "Yes") " (Major)" else ""
              component_text <- paste0(component_text, 
                                       "  ", comp$compname, " - ", comp$comppct_r, "%", major_flag, 
                                       " [", comp$taxorder, "]\n")
            }
          } else {
            component_text <- "Components: No data available\n"
          }
          
          polygon_info <- paste0(
            "Map Unit: ", attrs$muname, "\n",
            "MUKEY: ", attrs$MUKEY, "\n", 
            "Major Order: ", attrs$major_taxorder, "\n",
            "Sub-Order: ", if(!is.na(attrs$taxsuborder)) attrs$taxsuborder else "Unknown", "\n\n",
            component_text, "\n"
          )
        }
      }
    }
    
    paste0(
      "Coordinates: ", round(lat, 5), ", ", round(lng, 5), "\n\n",
      polygon_info,
      "Soil profile data shown below"
    )
  })
  
  # Create soil profile plot with better performance
  output$soil_profile <- renderPlotly({
    if (!click_data$has_data) {
      return(plotly_empty() %>% 
               layout(title = "Click on map to see soil profile"))
    }
    
    lat <- click_data$lat
    lng <- click_data$lng
    
    # Show loading message
    withProgress(message = 'Extracting soil data...', value = 0, {
      
      incProgress(0.3, detail = "Processing organic carbon...")
      # Extract profiles for both properties
      oc_profile <- extract_soil_profile(lat, lng, app_data$rasters, "oc")
      
      incProgress(0.6, detail = "Processing pH...")
      ph_profile <- extract_soil_profile(lat, lng, app_data$rasters, "ph")
      
      incProgress(0.9, detail = "Creating plots...")
      
      if (is.null(oc_profile) && is.null(ph_profile)) {
        return(plotly_empty() %>% 
                 layout(title = "No soil data available at this location"))
      }
      
      # Create subplot with error handling
      plots <- list()
      
      # Organic Carbon plot
      if (!is.null(oc_profile) && any(!is.na(oc_profile$value))) {
        # Filter out NA values for plotting
        oc_clean <- oc_profile[!is.na(oc_profile$value), ]
        if (nrow(oc_clean) > 0) {
          plots$oc <- plot_ly(
            oc_clean, 
            x = ~value, 
            y = ~-depth, 
            type = "scatter", 
            mode = "lines+markers",
            line = list(color = "#8B4513", width = 3),
            marker = list(color = "#D2691E", size = 8),
            name = "Organic C",
            hovertemplate = "<b>%{customdata}</b><br>OC: %{x} g/kg<extra></extra>",
            customdata = ~depth_label
          ) %>%
            layout(
              xaxis = list(title = "Organic Carbon (g/kg)"),
              yaxis = list(title = "Depth (cm)", autorange = "reversed")
            )
        }
      }
      
      # pH plot  
      if (!is.null(ph_profile) && any(!is.na(ph_profile$value))) {
        # Filter out NA values for plotting
        ph_clean <- ph_profile[!is.na(ph_profile$value), ]
        if (nrow(ph_clean) > 0) {
          plots$ph <- plot_ly(
            ph_clean, 
            x = ~value, 
            y = ~-depth, 
            type = "scatter", 
            mode = "lines+markers",
            line = list(color = "#1f77b4", width = 3),
            marker = list(color = "#ff7f0e", size = 8),
            name = "pH",
            hovertemplate = "<b>%{customdata}</b><br>pH: %{x}<extra></extra>",
            customdata = ~depth_label
          ) %>%
            layout(
              xaxis = list(title = "pH"),
              yaxis = list(title = "Depth (cm)", autorange = "reversed")
            )
        }
      }
      
      # Create final plot
      if (length(plots) == 0) {
        return(plotly_empty() %>% 
                 layout(title = "No valid soil data at this location"))
      } else if (length(plots) == 1) {
        # Single plot
        return(plots[[1]] %>%
                 layout(
                   title = "Soil Profile",
                   margin = list(t = 40, b = 40, l = 40, r = 40)
                 ))
      } else {
        # Subplot
        p <- subplot(
          plots$oc, plots$ph,
          nrows = 1, shareY = TRUE, titleX = TRUE
        ) %>%
          layout(
            title = "Soil Profile",
            showlegend = FALSE,
            margin = list(t = 40, b = 40, l = 40, r = 40)
          )
        return(p)
      }
    })
  })
  
  # Detect clicks on empty space (not on polygons) to clear highlights
  observe({
    # When there's a map click but no shape click, clear highlights and popups
    map_click <- input$main_map_click
    shape_click <- input$main_map_shape_click
    
    if (!is.null(map_click)) {
      # Use a small delay to see if a shape click follows
      invalidateLater(100, session)
      
      # If we have a recent map click but no corresponding shape click, clear highlights
      if (is.null(shape_click) || 
          is.null(shape_click$timestamp) || 
          is.null(map_click$timestamp) ||
          map_click$timestamp > shape_click$timestamp) {
        
        # Check if click is actually on empty space by comparing coordinates
        recent_shape <- !is.null(shape_click) && 
          !is.null(shape_click$timestamp) && 
          !is.null(map_click$timestamp) &&
          abs(map_click$timestamp - shape_click$timestamp) < 500  # Within 500ms
        
        if (!recent_shape) {
          leafletProxy("main_map") %>%
            clearPopups() %>%
            clearGroup("highlight")
        }
      }
    }
  })
  observeEvent(input$main_map_popup_close, {
    # Clear highlights and markers when popup is manually closed
    leafletProxy("main_map") %>%
      clearGroup("highlight") %>%
      clearGroup("click_marker")
    
    # Reset click data
    click_data$has_data <- FALSE
    click_data$lat <- NULL
    click_data$lng <- NULL
  })
  
  # Mouse coordinates
  output$mouse_coordinates <- renderText({
    coords <- input$main_map_mousemove
    if (is.null(coords)) {
      "Move mouse over map"
    } else {
      sprintf("Lat: %.5f, Lng: %.5f", coords$lat, coords$lng)
    }
  })
  
}

# Helper function for popup content
create_popup_content <- function(polygon, components) {
  component_list <- if (nrow(components) > 0) {
    paste(lapply(1:nrow(components), function(i) {
      comp <- components[i, ]
      major_flag <- if (comp$majcompflag == "Yes") " (Major)" else ""
      paste0(comp$compname, " - ", comp$comppct_r, "%", major_flag, " [", comp$taxorder, "]")
    }), collapse = "<br>")
  } else {
    "No component data available"
  }
  
  HTML(paste0(
    "<div style='max-width: 280px;'>",
    "<h4 style='margin-top: 0;'>", polygon$muname, "</h4>",
    "<hr>",
    "<strong>Components:</strong><br>",
    component_list,
    "<hr>",
    "<strong>Map Unit Key:</strong> ", polygon$MUKEY,
    "</div>"
  ))
}

# Run the application
shinyApp(ui = ui, server = server)