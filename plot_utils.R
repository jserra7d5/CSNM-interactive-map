# plot_utils.R - Plotting and Visualization Utility Functions

#' Create soil profile plot using plotly
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param raster_data List containing raster data
#' @return plotly object with soil profile visualization
create_soil_profile_plot <- function(lat, lng, raster_data) {
  if (is.null(lat) || is.null(lng)) {
    return(create_empty_plot("Click on map to see soil profile"))
  }
  
  # Show loading message
  withProgress(message = 'Extracting soil data...', value = 0, {
    
    incProgress(0.3, detail = "Processing organic carbon...")
    # Extract profiles for both properties
    oc_profile <- extract_soil_profile(lat, lng, raster_data, "oc")
    
    incProgress(0.6, detail = "Processing pH...")
    ph_profile <- extract_soil_profile(lat, lng, raster_data, "ph")
    
    incProgress(0.9, detail = "Creating plots...")
    
    if (is.null(oc_profile) && is.null(ph_profile)) {
      return(create_empty_plot("No soil data available at this location"))
    }
    
    # Create subplot with error handling
    plots <- create_profile_plots(oc_profile, ph_profile)
    
    return(combine_profile_plots(plots))
  })
}

#' Create individual profile plots for OC and pH
#' @param oc_profile Data frame with OC profile data
#' @param ph_profile Data frame with pH profile data
#' @return List containing plotly objects
create_profile_plots <- function(oc_profile, ph_profile) {
  plots <- list()
  
  # Organic Carbon plot
  if (!is.null(oc_profile) && any(!is.na(oc_profile$value))) {
    plots$oc <- create_oc_plot(oc_profile)
  }
  
  # pH plot  
  if (!is.null(ph_profile) && any(!is.na(ph_profile$value))) {
    plots$ph <- create_ph_plot(ph_profile)
  }
  
  return(plots)
}

#' Create organic carbon profile plot
#' @param oc_profile Data frame with OC profile data
#' @return plotly object
create_oc_plot <- function(oc_profile) {
  # Filter out NA values for plotting
  oc_clean <- oc_profile[!is.na(oc_profile$value), ]
  
  if (nrow(oc_clean) == 0) return(NULL)
  
  plot_ly(
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

#' Create pH profile plot
#' @param ph_profile Data frame with pH profile data
#' @return plotly object
create_ph_plot <- function(ph_profile) {
  # Filter out NA values for plotting
  ph_clean <- ph_profile[!is.na(ph_profile$value), ]
  
  if (nrow(ph_clean) == 0) return(NULL)
  
  plot_ly(
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

#' Combine profile plots into final visualization
#' @param plots List of plotly objects
#' @return plotly object
combine_profile_plots <- function(plots) {
  if (length(plots) == 0) {
    return(create_empty_plot("No valid soil data at this location"))
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
}

#' Create empty plotly plot with message
#' @param message Character message to display
#' @return plotly object
create_empty_plot <- function(message) {
  plotly_empty() %>% 
    layout(title = message)
}

#' Format selection information text
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param polygons sf object with polygon data
#' @param components Data frame with component information
#' @return Character string with formatted information
format_selection_info <- function(lat, lng, polygons = NULL, components = NULL) {
  if (is.null(lat) || is.null(lng)) {
    return("Click anywhere on map to extract soil data")
  }
  
  # Try to get polygon info if available
  polygon_info <- ""
  if (!is.null(polygons)) {
    polygon_info <- extract_polygon_info_at_point(lat, lng, polygons, components)
  }
  
  paste0(
    "Coordinates: ", round(lat, 5), ", ", round(lng, 5), "\n\n",
    polygon_info,
    "Soil profile data shown below"
  )
}

#' Extract polygon information at a specific point
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param polygons sf object with polygon data
#' @param components Data frame with component information
#' @return Character string with polygon information
extract_polygon_info_at_point <- function(lat, lng, polygons, components) {
  # Create point and find intersection more efficiently
  point_sf <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  
  # Use st_filter for better performance with large polygon datasets
  nearby_polygons <- st_filter(polygons, point_sf)
  
  if (nrow(nearby_polygons) == 0) return("")
  
  # Find the actual intersection
  intersected <- st_intersection(point_sf, nearby_polygons)
  if (length(intersected) == 0) return("")
  
  # Get the first intersected polygon's attributes
  attrs <- st_drop_geometry(nearby_polygons[1, ])
  
  # Get detailed component information
  comp_info <- get_component_info_for_mukey(attrs$MUKEY, components)
  
  # Build formatted text
  paste0(
    "Map Unit: ", attrs$muname, "\n",
    "MUKEY: ", attrs$MUKEY, "\n", 
    "Major Order: ", attrs$major_taxorder, "\n",
    "Sub-Order: ", if(!is.na(attrs$taxsuborder)) attrs$taxsuborder else "Unknown", "\n\n",
    comp_info, "\n"
  )
}

#' Get formatted component information for a map unit
#' @param mukey Character map unit key
#' @param components Data frame with component information
#' @return Character string with component information
get_component_info_for_mukey <- function(mukey, components) {
  if (is.null(components)) return("Components: No data available\n")
  
  comp_subset <- components %>%
    dplyr::filter(MUKEY == mukey) %>%
    dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r))
  
  if (nrow(comp_subset) == 0) {
    return("Components: No data available\n")
  }
  
  component_text <- "Components:\n"
  for (i in 1:nrow(comp_subset)) {
    comp <- comp_subset[i, ]
    major_flag <- if (comp$majcompflag == "Yes") " (Major)" else ""
    component_text <- paste0(component_text, 
                             "  ", comp$compname, " - ", comp$comppct_r, "%", major_flag, 
                             " [", comp$taxorder, "]\n")
  }
  
  return(component_text)
}