# server_module.R - Server Logic Functions

#' Create the main server function
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
create_server <- function(input, output, session) {
  
  cat("=== SERVER FUNCTION STARTING ===\n")
  
  # Initialize AQP module
  initialize_aqp_module()
  
  # Load data on startup
  cat("=== LOADING DATA ===\n")
  app_data <- load_and_prepare_data()
  cat("Data loading complete\n")
  
  # Log data availability
  log_data_availability(app_data)
  
  # Initialize reactive values
  click_data <- initialize_click_data()
  
  # Set up observers and outputs
  setup_data_availability_outputs(output, app_data, click_data)
  setup_map_outputs(output, input, app_data, click_data, session)
  setup_interaction_observers(input, output, app_data, click_data, session)
  setup_ui_observers(input, output, app_data)
  setup_nrcs_profile_outputs(output, input, app_data, click_data)
  setup_navigation_observers(input, output, click_data, session)
}

#' Log data availability for debugging
#' @param app_data List containing loaded application data
log_data_availability <- function(app_data) {
  cat("Polygons available:", !is.null(app_data$polygons), "\n")
  if (!is.null(app_data$polygons)) {
    cat("Number of polygons:", nrow(app_data$polygons), "\n")
  }
  cat("OC raster available:", !is.null(app_data$rasters$oc), "\n")
  cat("pH raster available:", !is.null(app_data$rasters$ph), "\n")
}

#' Initialize reactive values for click data
#' @return reactiveValues object
initialize_click_data <- function() {
  reactiveValues(
    lat = NULL,
    lng = NULL,
    has_data = FALSE,
    clicked_map_unit = NULL,
    selected_components = NULL,
    profile_view_active = FALSE
  )
}

#' Set up data availability output flags
#' @param output Shiny output object
#' @param app_data List containing application data
#' @param click_data reactiveValues object
setup_data_availability_outputs <- function(output, app_data, click_data) {
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
  
  # Check if NRCS data is available
  output$has_nrcs_data <- reactive({
    !is.null(click_data$selected_components) && nrow(click_data$selected_components) > 0
  })
  outputOptions(output, "has_nrcs_data", suspendWhenHidden = FALSE)
}

#' Set up map-related outputs
#' @param output Shiny output object
#' @param input Shiny input object
#' @param app_data List containing application data
#' @param click_data reactiveValues object
#' @param session Shiny session object
setup_map_outputs <- function(output, input, app_data, click_data, session) {
  # Create main map
  output$main_map <- renderLeaflet({
    create_main_map(app_data)
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
  
  # Selection info (old text version - keeping for compatibility)
  output$selection_info <- renderText({
    format_selection_info(
      click_data$lat, 
      click_data$lng, 
      app_data$polygons, 
      app_data$components
    )
  })
  
  # Formatted selection info with clickable major component
  output$selection_info_formatted <- renderUI({
    create_formatted_selection_info(
      click_data$lat,
      click_data$lng,
      app_data$polygons,
      app_data$components
    )
  })
  
  # Soil profile plot
  output$soil_profile <- renderPlotly({
    create_soil_profile_plot(
      click_data$lat, 
      click_data$lng, 
      app_data$rasters
    )
  })
}

#' Set up interaction observers for map events
#' @param input Shiny input object
#' @param output Shiny output object
#' @param app_data List containing application data
#' @param click_data reactiveValues object
#' @param session Shiny session object
setup_interaction_observers <- function(input, output, app_data, click_data, session) {
  # Handle map clicks for soil profiles
  observeEvent(input$main_map_click, {
    handle_map_click(input$main_map_click, click_data)
  })
  
  # Handle polygon shape clicks for detailed info
  observeEvent(input$main_map_shape_click, {
    handle_shape_click(input$main_map_shape_click, app_data, click_data)
  }, priority = 10)
  
  # Handle popup close events
  observeEvent(input$main_map_popup_close, {
    handle_popup_close(click_data)
  })
  
  # Detect clicks on empty space to clear highlights
  observe({
    handle_empty_space_clicks(input, session)
  })
}

#' Set up UI-related observers
#' @param input Shiny input object
#' @param output Shiny output object
#' @param app_data List containing application data
setup_ui_observers <- function(input, output, app_data) {
  # Handle layer switching
  observe({
    handle_layer_switching_observer(input, app_data)
  })
  
  # Test if events are working (for debugging)
  observe({
    cat("Map type changed to:", input$map_type, "\n")
  })
  
  observe({
    cat("Boundaries toggle:", input$show_boundaries, "\n")
  })
}

#' Create the main leaflet map
#' @param app_data List containing application data
#' @return leaflet map object
create_main_map <- function(app_data) {
  cat("=== renderLeaflet CALLED ===\n")
  cat("Creating base map...\n")
  
  tryCatch({
    # Create base map
    map <- create_base_map()
    cat("Base map tiles added successfully\n")
    
    # Add polygon layers if available
    if (!is.null(app_data$polygons)) {
      cat("=== ADDING POLYGON LAYERS ===\n")
      debug_polygon_data(app_data$polygons)
      map <- add_polygon_layers(map, app_data$polygons)
      cat("Polygons added to map successfully\n")
    } else {
      cat("WARNING: No polygon data available\n")
    }
    
    # Add raster layers
    cat("=== PROCESSING RASTER LAYERS ===\n")
    map <- add_raster_layers(map, app_data$rasters)
    
    # Set initial visibility and add event handlers
    map <- map %>%
      set_initial_layer_visibility() %>%
      add_map_event_handlers()
    
    cat("=== MAP CREATION COMPLETE ===\n")
    return(map)
    
  }, error = function(e) {
    cat("ERROR in renderLeaflet:", e$message, "\n")
    cat("Error details:", str(e), "\n")
    return(leaflet() %>% setView(lng = -122.5, lat = 42.0, zoom = 8))
  })
}

#' Debug polygon data for troubleshooting
#' @param polygons sf object with polygon data
debug_polygon_data <- function(polygons) {
  # Debug: Check what soil orders we actually have
  unique_orders <- unique(polygons$major_taxorder)
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
  
  cat("Sample polygon_ids:", head(polygons$polygon_id, 10), "\n")
}

#' Handle map click events
#' @param click Map click input
#' @param click_data reactiveValues object
handle_map_click <- function(click, click_data) {
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
}

#' Handle polygon shape click events
#' @param click Shape click input
#' @param app_data List containing application data
#' @param click_data reactiveValues object
handle_shape_click <- function(click, app_data, click_data) {
  if (is.null(app_data$polygons) || is.null(click)) return()
  
  polygon_id <- click$id
  if (is.null(polygon_id)) return()
  
  # Clear previous popups and highlights
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
  
  # Store component data for NRCS profile fetching
  click_data$clicked_map_unit <- selected_polygon
  click_data$selected_components <- components
  
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
}

#' Handle popup close events
#' @param click_data reactiveValues object
handle_popup_close <- function(click_data) {
  # Clear highlights and markers when popup is manually closed
  leafletProxy("main_map") %>%
    clearGroup("highlight") %>%
    clearGroup("click_marker")
  
  # Reset click data
  click_data$has_data <- FALSE
  click_data$lat <- NULL
  click_data$lng <- NULL
}

#' Handle clicks on empty space
#' @param input Shiny input object
#' @param session Shiny session object
handle_empty_space_clicks <- function(input, session) {
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
}

#' Handle layer switching observer
#' @param input Shiny input object
#' @param app_data List containing application data
handle_layer_switching_observer <- function(input, app_data) {
  req(input$map_type)
  
  proxy <- leafletProxy("main_map")
  
  # Reset click data when switching layers
  # Note: This would need to be passed as a parameter if needed
  
  # Get depth selections
  oc_depth <- as.numeric(if (is.null(input$oc_depth)) 1 else input$oc_depth)
  ph_depth <- as.numeric(if (is.null(input$ph_depth)) 1 else input$ph_depth)
  
  # Determine which depth to use based on map type
  depth_idx <- switch(input$map_type,
                      "oc" = oc_depth,
                      "ph" = ph_depth,
                      NULL)
  
  # Handle layer switching
  handle_layer_switching(
    proxy, 
    input$map_type, 
    depth_idx, 
    app_data$rasters, 
    input$show_boundaries
  )
}

#' Set up NRCS soil profile outputs and reactives
#' @param output Shiny output object
#' @param input Shiny input object
#' @param app_data List containing application data
#' @param click_data reactiveValues object
setup_nrcs_profile_outputs <- function(output, input, app_data, click_data) {
  
  # Reactive expression for NRCS soil profile data
  nrcs_profile_data <- reactive({
    
    # Require component data from map unit click
    req(click_data$selected_components)
    components <- click_data$selected_components
    
    if (is.null(components) || nrow(components) == 0) {
      return(NULL)
    }
    
    # Extract soil series names from components
    soil_series <- extract_soil_series_from_components(components)
    
    if (length(soil_series) == 0) {
      return(NULL)
    }
    
    # Fetch NRCS profile data
    withProgress(message = 'Fetching NRCS soil data...', value = 0, {
      incProgress(0.3, detail = "Processing soil series names")
      
      profile_data <- fetch_nrcs_soil_profiles(soil_series, color_state = "moist")
      
      incProgress(0.7, detail = "Preparing visualization")
      
      if (!is.null(profile_data)) {
        incProgress(1.0, detail = "Complete")
        return(list(
          spc = profile_data$spc,
          metadata = profile_data$metadata,
          components = components
        ))
      } else {
        incProgress(1.0, detail = "No data found")
        return(NULL)
      }
    })
  })
  
  # NRCS soil profile plot output
  output$nrcs_soil_profile <- renderPlot({
    
    profile_data <- nrcs_profile_data()
    
    if (is.null(profile_data) || is.null(profile_data$spc)) {
      plot.new()
      text(0.5, 0.5, "No NRCS soil profile data available\nfor this map unit", 
           cex = 1.1, col = "gray50", adj = c(0.5, 0.5))
      return()
    }
    
    # Create AQP soil profile plot
    create_aqp_soil_profile_plot(
      spc = profile_data$spc, 
      map_unit_info = click_data$clicked_map_unit,
      plot_width = 350
    )
    
  }, height = function() {
    # Dynamic height based on number of profiles
    profile_data <- nrcs_profile_data()
    if (!is.null(profile_data) && !is.null(profile_data$spc)) {
      n_profiles <- length(profile_data$spc)
      return(max(300, min(500, n_profiles * 60 + 200)))
    } else {
      return(300)
    }
  })
  
  # NRCS profile summary table
  output$nrcs_profile_summary <- renderTable({
    
    profile_data <- nrcs_profile_data()
    
    if (is.null(profile_data) || is.null(profile_data$spc)) {
      return(data.frame(Message = "No profile data available"))
    }
    
    # Create profile summary
    summary_table <- create_profile_summary_table(
      spc = profile_data$spc,
      map_unit_components = profile_data$components
    )
    
    return(summary_table)
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
}

#' Create formatted selection info with clickable major component
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param polygons sf object with polygon data
#' @param components Data frame with component information
#' @return Shiny UI elements
create_formatted_selection_info <- function(lat, lng, polygons = NULL, components = NULL) {
  if (is.null(lat) || is.null(lng)) {
    return(div("Click anywhere on map to extract soil data"))
  }
  
  # Try to get polygon info if available
  if (is.null(polygons)) {
    return(div(
      p(paste0("Coordinates: ", round(lat, 5), ", ", round(lng, 5))),
      p("Soil profile data shown below")
    ))
  }
  
  # Extract polygon information
  polygon_data <- extract_polygon_data_at_point(lat, lng, polygons, components)
  
  if (is.null(polygon_data)) {
    return(div(
      p(paste0("Coordinates: ", round(lat, 5), ", ", round(lng, 5))),
      p("No map unit data available at this location")
    ))
  }
  
  # Get all components for this map unit
  all_components <- NULL
  major_component <- NULL
  if (!is.null(components)) {
    all_components <- components %>%
      dplyr::filter(MUKEY == polygon_data$MUKEY) %>%
      dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r))
    
    if (nrow(all_components) > 0) {
      major_component <- all_components[1, ]
    }
  }
  
  # Build UI elements
  ui_elements <- list(
    p(strong("Coordinates: "), paste0(round(lat, 5), ", ", round(lng, 5))),
    p(strong("Map Unit: "), polygon_data$muname),
    p(strong("MUKEY: "), polygon_data$MUKEY),
    p(strong("Major Order: "), polygon_data$major_taxorder)
  )
  
  # Add components section
  if (!is.null(all_components) && nrow(all_components) > 0) {
    # Components header
    ui_elements <- append(ui_elements, list(p(strong("Components:"))))
    
    # Create component list
    component_items <- list()
    
    for (i in 1:nrow(all_components)) {
      comp <- all_components[i, ]
      
      if (comp$majcompflag == "Yes") {
        # Major component - make it clickable
        comp_link <- actionLink(
          inputId = "view_major_component",
          label = paste0(comp$compname, " ", comp$comppct_r, "% (Major)"),
          style = "color: #337ab7; text-decoration: underline; cursor: pointer; margin-left: 15px;"
        )
        component_items <- append(component_items, list(p(comp_link)))
      } else {
        # Minor component - just display text
        component_items <- append(component_items, list(
          p(paste0(comp$compname, " ", comp$comppct_r, "%"), style = "margin-left: 15px;")
        ))
      }
    }
    
    ui_elements <- append(ui_elements, component_items)
  }
  
  return(div(ui_elements))
}

#' Extract polygon data at a specific point
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param polygons sf object with polygon data
#' @param components Data frame with component information
#' @return List with polygon attributes or NULL
extract_polygon_data_at_point <- function(lat, lng, polygons, components) {
  # Create point and find intersection
  point_sf <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  
  # Use st_filter for better performance
  nearby_polygons <- st_filter(polygons, point_sf)
  
  if (nrow(nearby_polygons) == 0) return(NULL)
  
  # Find the actual intersection
  intersected <- st_intersection(point_sf, nearby_polygons)
  if (length(intersected) == 0) return(NULL)
  
  # Get the first intersected polygon's attributes
  attrs <- st_drop_geometry(nearby_polygons[1, ])
  
  return(attrs)
}

#' Set up navigation observers for profile view
#' @param input Shiny input object
#' @param output Shiny output object
#' @param click_data reactiveValues object
#' @param session Shiny session object
setup_navigation_observers <- function(input, output, click_data, session) {
  
  # Observer for major component link click
  observeEvent(input$view_major_component, {
    # Switch to profile view
    click_data$profile_view_active <- TRUE
    
    # Hide main controls and main view, show profile view
    shinyjs::hide("main_controls")
    shinyjs::hide("main_view") 
    shinyjs::show("profile_view")
  })
  
  # Observer for back button click
  observeEvent(input$back_to_main, {
    # Switch back to main view
    click_data$profile_view_active <- FALSE
    
    # Show main controls and main view, hide profile view
    shinyjs::show("main_controls")
    shinyjs::show("main_view")
    shinyjs::hide("profile_view")
  })
}