# map_utils.R - Map and Spatial Utility Functions

#' Create soil order color palette function
#' @return colorFactor function for soil orders
create_soil_order_palette <- function() {
  # Get all unique soil orders from the data, including any unexpected ones
  all_orders <- unique(c(names(SOIL_ORDER_COLORS), "Unknown"))
  
  # Ensure we have colors for all possible values
  colors <- SOIL_ORDER_COLORS[all_orders]
  names(colors) <- all_orders
  
  # Use na.color for any values not in our defined palette
  colorFactor(colors, domain = all_orders, na.color = "#808080")
}

#' Extract soil profile data at a point - optimized version
#' @param lat Numeric latitude
#' @param lng Numeric longitude  
#' @param raster_data List of raster data
#' @param property Character, either "oc" or "ph"
#' @return Data frame with soil profile data or NULL
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

#' Create popup content for clicked polygons
#' @param polygon sf object with polygon data
#' @param components Data frame with component information
#' @return HTML content for popup
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

#' Create base leaflet map with tiles
#' @return leaflet map object
create_base_map <- function() {
  leaflet() %>%
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
}

#' Add polygon layers to leaflet map
#' @param map leaflet map object
#' @param polygons sf object with polygon data
#' @return leaflet map object with polygon layers added
add_polygon_layers <- function(map, polygons) {
  if (is.null(polygons)) return(map)
  
  cat("Adding", nrow(polygons), "polygons to map\n")
  
  soil_palette <- create_soil_order_palette()
  
  map %>%
    addPolygons(
      data = polygons,
      layerId = ~polygon_id,
      group = "soil_fill",
      fillColor = ~soil_palette(major_taxorder),
      fillOpacity = 0.6,
      color = "white",
      weight = 0,
      smoothFactor = 0.2,
      options = pathOptions(clickable = TRUE)
    ) %>%
    addPolygons(
      data = polygons,
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
      options = pathOptions(clickable = TRUE)
    )
}

#' Add raster layers to leaflet map
#' @param map leaflet map object
#' @param raster_data List containing raster data
#' @return leaflet map object with raster layers added
add_raster_layers <- function(map, raster_data) {
  # Add OC layers
  if (!is.null(raster_data$oc)) {
    cat("Adding", length(raster_data$oc), "OC raster layers\n")
    for (i in 1:length(raster_data$oc)) {
      map <- map %>%
        addRasterImage(
          raster_data$oc[[i]]$raster,
          colors = raster_data$oc[[i]]$palette,
          opacity = 0.8,
          group = paste0("oc_", i),
          project = FALSE
        )
    }
  }
  
  # Add pH layers
  if (!is.null(raster_data$ph)) {
    cat("Adding", length(raster_data$ph), "pH raster layers\n")
    for (i in 1:length(raster_data$ph)) {
      map <- map %>%
        addRasterImage(
          raster_data$ph[[i]]$raster,
          colors = raster_data$ph[[i]]$palette,
          opacity = 0.8,
          group = paste0("ph_", i),
          project = FALSE
        )
    }
  }
  
  return(map)
}

#' Add JavaScript event handlers to leaflet map
#' @param map leaflet map object
#' @return leaflet map object with event handlers
add_map_event_handlers <- function(map) {
  map %>%
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
}

#' Get all possible layer groups for the map
#' @return Character vector of all layer group names
get_all_layer_groups <- function() {
  c("terrain", "satellite", "soil_fill", "soil_boundaries",
    paste0("oc_", 1:6), paste0("ph_", 1:6))
}

#' Set initial layer visibility
#' @param map leaflet map object
#' @return leaflet map object with initial visibility set
set_initial_layer_visibility <- function(map) {
  all_groups <- get_all_layer_groups()
  
  map %>%
    showGroup(c("terrain", "soil_fill")) %>%
    hideGroup(setdiff(all_groups, c("terrain", "soil_fill")))
}

#' Handle layer switching logic
#' @param proxy leafletProxy object
#' @param map_type Character indicating the map type
#' @param depth_idx Integer for depth selection
#' @param raster_data List containing raster data
#' @param show_boundaries Boolean for boundary visibility
handle_layer_switching <- function(proxy, map_type, depth_idx = NULL, raster_data = NULL, show_boundaries = FALSE) {
  # Clear existing legends, popups, highlights, and markers
  proxy %>% 
    clearControls() %>%
    clearPopups() %>%
    clearGroup("highlight") %>%
    clearGroup("click_marker")
  
  # Hide all groups first
  all_groups <- get_all_layer_groups()
  proxy %>% hideGroup(all_groups)
  
  # Show appropriate layers based on selection
  if (map_type == "soil") {
    proxy %>% showGroup(c("terrain", "soil_fill"))
    
  } else if (map_type == "satellite") {
    proxy %>% showGroup("satellite")
    
  } else if (map_type == "oc" && !is.null(raster_data$oc)) {
    add_oc_layer_with_legend(proxy, depth_idx, raster_data$oc)
    
  } else if (map_type == "ph" && !is.null(raster_data$ph)) {
    add_ph_layer_with_legend(proxy, depth_idx, raster_data$ph)
  }
  
  # Handle boundary overlay
  if (show_boundaries) {
    proxy %>% showGroup("soil_boundaries")
  } else {
    proxy %>% hideGroup("soil_boundaries")
  }
}

#' Add OC layer with legend
#' @param proxy leafletProxy object
#' @param depth_idx Integer for depth selection
#' @param oc_data List of OC raster data
add_oc_layer_with_legend <- function(proxy, depth_idx, oc_data) {
  if (is.null(depth_idx)) depth_idx <- 1
  
  if (depth_idx <= length(oc_data)) {
    domain <- oc_data[[depth_idx]]$domain
    
    # Create tick positions that include min, max, and 3-4 intermediate values
    tick_values <- c(
      domain[1],
      seq(domain[1], domain[2], length.out = 5)[2:4],
      domain[2]
    )
    
    proxy %>% 
      showGroup(c("terrain", paste0("oc_", depth_idx))) %>%
      addLegend(
        pal = oc_data[[depth_idx]]$palette,
        values = tick_values,
        title = paste("Organic Carbon<br>(g/kg)", DEPTH_LEVELS$labels[depth_idx]),
        position = "topleft",
        opacity = 1,
        labFormat = labelFormat(
          digits = 1,
          transform = function(x) sort(x)
        )
      )
  }
}

#' Add pH layer with legend
#' @param proxy leafletProxy object
#' @param depth_idx Integer for depth selection
#' @param ph_data List of pH raster data
add_ph_layer_with_legend <- function(proxy, depth_idx, ph_data) {
  if (is.null(depth_idx)) depth_idx <- 1
  
  if (depth_idx <= length(ph_data)) {
    domain <- ph_data[[depth_idx]]$domain
    
    # Create tick positions that include min, max, and intermediate values
    tick_values <- c(
      domain[1],
      seq(domain[1], domain[2], length.out = 6)[2:5],
      domain[2]
    )
    
    proxy %>%
      showGroup(c("terrain", paste0("ph_", depth_idx))) %>%
      addLegend(
        pal = ph_data[[depth_idx]]$palette,
        values = tick_values,
        title = paste("Soil pH<br>", DEPTH_LEVELS$labels[depth_idx]),
        position = "topleft",
        opacity = 1,
        labFormat = labelFormat(
          digits = 1,
          transform = function(x) sort(x)
        )
      )
  }
}