# soil_sketch_module.R - Soil Profile Sketch Functions using AQP

#' Create soil profile sketch using AQP
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param app_data List containing application data
#' @return ggplot object with soil profile sketch
create_soil_profile_sketch <- function(lat, lng, app_data) {
  if (is.null(lat) || is.null(lng) || is.null(app_data$polygons)) {
    return(NULL)
  }
  
  tryCatch({
    # Find the polygon at the clicked location
    point <- st_sfc(st_point(c(lng, lat)), crs = 4326)
    point_proj <- st_transform(point, st_crs(app_data$polygons))
    
    # Find intersecting polygon
    intersection <- st_intersects(point_proj, app_data$polygons)
    if (length(intersection[[1]]) == 0) {
      return(NULL)
    }
    
    polygon_idx <- intersection[[1]][1]
    selected_polygon <- app_data$polygons[polygon_idx, ]
    
    # Get soil data for the selected polygon
    soil_data <- get_soil_data_for_sketch(selected_polygon$MUKEY, app_data$components)
    
    if (is.null(soil_data) || nrow(soil_data) == 0) {
      return(NULL)
    }
    
    # Create soil profile sketch
    sketch <- create_sketch_from_data(soil_data, selected_polygon$muname)
    
    return(sketch)
    
  }, error = function(e) {
    cat("Error creating soil sketch:", e$message, "\n")
    return(NULL)
  })
}

#' Get soil data for creating sketch
#' @param mukey Character map unit key
#' @param components Data frame with component information
#' @return Data frame with soil horizon data
get_soil_data_for_sketch <- function(mukey, components) {
  if (is.null(components) || nrow(components) == 0) {
    return(NULL)
  }
  
  # Filter components for the selected map unit
  soil_components <- components %>%
    dplyr::filter(MUKEY == mukey) %>%
    dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r))
  
  if (nrow(soil_components) == 0) {
    return(NULL)
  }
  
  # Try to get real SSURGO horizon data first
  horizons <- try_get_ssurgo_horizons(soil_components$cokey[1])
  
  # If no real data available, create synthetic horizons
  if (is.null(horizons) || nrow(horizons) == 0) {
    major_component <- soil_components[1, ]
    horizons <- create_synthetic_horizons(major_component$taxorder)
  }
  
  return(horizons)
}

#' Try to fetch real SSURGO horizon data
#' @param cokey Character component key
#' @return Data frame with horizon data or NULL if not available
try_get_ssurgo_horizons <- function(cokey) {
  if (!requireNamespace("soilDB", quietly = TRUE)) {
    return(NULL)
  }
  
  tryCatch({
    # Query SSURGO for horizon data
    query <- paste0("SELECT 
                      hzname, 
                      hzdept_r as top, 
                      hzdepb_r as bottom,
                      texture,
                      dbovendry_r as bulk_density,
                      ph1to1h2o_r as ph,
                      om_r as organic_matter
                    FROM chorizon 
                    WHERE cokey = '", cokey, "'
                    ORDER BY hzdept_r")
    
    # Note: This would require a connection to SSURGO database
    # For now, return NULL to use synthetic data
    return(NULL)
    
  }, error = function(e) {
    cat("Error fetching SSURGO data:", e$message, "\n")
    return(NULL)
  })
}

#' Create synthetic soil horizons based on soil order
#' @param taxorder Character soil taxonomic order
#' @return Data frame with horizon data
create_synthetic_horizons <- function(taxorder) {
  # Define typical horizon sequences for different soil orders
  horizon_sequences <- list(
    "Alfisols" = data.frame(
      horizon = c("A", "E", "Bt", "Btk", "C"),
      top = c(0, 10, 25, 50, 80),
      bottom = c(10, 25, 50, 80, 120),
      texture = c("silt loam", "silt loam", "clay loam", "clay loam", "loam"),
      color = c("#8B4513", "#F5DEB3", "#CD853F", "#D2691E", "#A0522D"),
      stringsAsFactors = FALSE
    ),
    "Andisols" = data.frame(
      horizon = c("A", "Bw", "BC", "C"),
      top = c(0, 15, 40, 70),
      bottom = c(15, 40, 70, 100),
      texture = c("silt loam", "silt loam", "loam", "loam"),
      color = c("#654321", "#8B7355", "#A0522D", "#8B4513"),
      stringsAsFactors = FALSE
    ),
    "Aridisols" = data.frame(
      horizon = c("A", "Bk", "Bky", "C"),
      top = c(0, 8, 25, 50),
      bottom = c(8, 25, 50, 80),
      texture = c("loam", "clay loam", "clay loam", "loam"),
      color = c("#F5DEB3", "#DEB887", "#D2691E", "#A0522D"),
      stringsAsFactors = FALSE
    ),
    "Entisols" = data.frame(
      horizon = c("A", "C"),
      top = c(0, 20),
      bottom = c(20, 60),
      texture = c("loam", "loam"),
      color = c("#8B4513", "#A0522D"),
      stringsAsFactors = FALSE
    ),
    "Inceptisols" = data.frame(
      horizon = c("A", "Bw", "C"),
      top = c(0, 12, 35),
      bottom = c(12, 35, 70),
      texture = c("silt loam", "silt loam", "loam"),
      color = c("#8B4513", "#CD853F", "#A0522D"),
      stringsAsFactors = FALSE
    ),
    "Mollisols" = data.frame(
      horizon = c("A", "AB", "Bt", "C"),
      top = c(0, 20, 40, 70),
      bottom = c(20, 40, 70, 100),
      texture = c("silt loam", "silt loam", "clay loam", "loam"),
      color = c("#654321", "#8B4513", "#CD853F", "#A0522D"),
      stringsAsFactors = FALSE
    ),
    "Spodosols" = data.frame(
      horizon = c("O", "A", "E", "Bh", "Bs", "C"),
      top = c(0, 2, 5, 15, 30, 50),
      bottom = c(2, 5, 15, 30, 50, 80),
      texture = c("organic", "sandy loam", "sandy loam", "sandy loam", "sandy loam", "loam"),
      color = c("#2F4F4F", "#8B4513", "#F5DEB3", "#8B7355", "#CD853F", "#A0522D"),
      stringsAsFactors = FALSE
    ),
    "Ultisols" = data.frame(
      horizon = c("A", "E", "Bt", "BC", "C"),
      top = c(0, 8, 20, 45, 70),
      bottom = c(8, 20, 45, 70, 100),
      texture = c("silt loam", "silt loam", "clay", "clay loam", "loam"),
      color = c("#8B4513", "#F5DEB3", "#CD853F", "#D2691E", "#A0522D"),
      stringsAsFactors = FALSE
    )
  )
  
  # Get horizon sequence for the soil order, or use default
  if (taxorder %in% names(horizon_sequences)) {
    horizons <- horizon_sequences[[taxorder]]
  } else {
    # Default horizon sequence
    horizons <- data.frame(
      horizon = c("A", "Bw", "C"),
      top = c(0, 15, 40),
      bottom = c(15, 40, 70),
      texture = c("loam", "loam", "loam"),
      color = c("#8B4513", "#CD853F", "#A0522D"),
      stringsAsFactors = FALSE
    )
  }
  
  return(horizons)
}

#' Create soil profile sketch from horizon data
#' @param horizons Data frame with horizon data
#' @param soil_name Character soil name
#' @return ggplot object with soil profile sketch
create_sketch_from_data <- function(horizons, soil_name) {
  if (is.null(horizons) || nrow(horizons) == 0) {
    return(NULL)
  }
  
  # Create a simple soil profile sketch using ggplot
  library(ggplot2)
  
  # Prepare data for plotting
  plot_data <- horizons %>%
    dplyr::mutate(
      y_min = -bottom,
      y_max = -top,
      y_mid = (y_min + y_max) / 2,
      thickness = bottom - top
    )
  
  # Create the sketch
  sketch <- ggplot(plot_data, aes(x = 0, y = y_mid)) +
    # Draw horizon rectangles with texture patterns
    geom_rect(
      aes(xmin = -0.5, xmax = 0.5, ymin = y_min, ymax = y_max, fill = color),
      color = "black", linewidth = 0.5
    ) +
    # Add horizon labels
    geom_text(
      aes(label = horizon),
      hjust = 0.5, vjust = 0.5, fontface = "bold", size = 3, color = "white"
    ) +
    # Add depth labels
    geom_text(
      aes(x = 0.7, label = paste0(top, "-", bottom, " cm")),
      hjust = 0, vjust = 0.5, size = 2.5
    ) +
    # Add texture labels
    geom_text(
      aes(x = -0.7, label = texture),
      hjust = 1, vjust = 0.5, size = 2.5, fontface = "italic"
    ) +
    # Add horizon boundaries
    geom_hline(
      data = plot_data,
      aes(yintercept = y_min),
      color = "black", linewidth = 0.3
    ) +
    # Customize appearance
    scale_fill_identity() +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(
      limits = c(-max(plot_data$bottom), 0),
      breaks = -plot_data$bottom,
      labels = plot_data$bottom
    ) +
    labs(
      title = paste("Soil Profile:", soil_name),
      subtitle = "Click on map to view soil profile",
      x = NULL,
      y = "Depth (cm)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8, color = "gray50"),
      axis.title.y = element_text(size = 9),
      axis.text.y = element_text(size = 8)
    )
  
  return(sketch)
}

#' Create enhanced soil profile sketch with additional properties
#' @param horizons Data frame with horizon data
#' @param soil_name Character soil name
#' @return ggplot object with enhanced soil profile sketch
create_enhanced_soil_sketch <- function(horizons, soil_name) {
  if (is.null(horizons) || nrow(horizons) == 0) {
    return(NULL)
  }
  
  library(ggplot2)
  library(gridExtra)
  
  # Prepare data for plotting
  plot_data <- horizons %>%
    dplyr::mutate(
      y_min = -bottom,
      y_max = -top,
      y_mid = (y_min + y_max) / 2,
      thickness = bottom - top
    )
  
  # Create main profile sketch
  main_sketch <- ggplot(plot_data, aes(x = 0, y = y_mid)) +
    geom_rect(
      aes(xmin = -0.5, xmax = 0.5, ymin = y_min, ymax = y_max, fill = color),
      color = "black", linewidth = 0.5
    ) +
    geom_text(
      aes(label = horizon),
      hjust = 0.5, vjust = 0.5, fontface = "bold", size = 3, color = "white"
    ) +
    geom_text(
      aes(x = 0.7, label = paste0(top, "-", bottom, " cm")),
      hjust = 0, vjust = 0.5, size = 2.5
    ) +
    geom_text(
      aes(x = -0.7, label = texture),
      hjust = 1, vjust = 0.5, size = 2.5, fontface = "italic"
    ) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(
      limits = c(-max(plot_data$bottom), 0),
      breaks = -plot_data$bottom,
      labels = plot_data$bottom
    ) +
    labs(
      title = paste("Soil Profile:", soil_name),
      x = NULL,
      y = "Depth (cm)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      axis.title.y = element_text(size = 9),
      axis.text.y = element_text(size = 8)
    )
  
  # Create horizon properties table
  properties_table <- plot_data %>%
    dplyr::select(horizon, top, bottom, texture) %>%
    dplyr::mutate(
      depth_range = paste0(top, "-", bottom, " cm"),
      thickness = bottom - top
    ) %>%
    dplyr::select(horizon, depth_range, thickness, texture)
  
  # Create table plot
  table_plot <- ggplot() +
    annotation_custom(
      tableGrob(properties_table, 
                rows = NULL,
                theme = ttheme_minimal(
                  base_size = 8,
                  padding = unit(c(2, 4), "mm")
                )),
      xmin = 0, xmax = 1, ymin = 0, ymax = 1
    ) +
    labs(title = "Horizon Properties") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))
  
  # Combine plots
  combined_plot <- grid.arrange(
    main_sketch, table_plot,
    ncol = 2, widths = c(2, 1)
  )
  
  return(combined_plot)
}

#' Create enhanced soil profile sketch with AQP
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param app_data List containing application data
#' @return plot object with AQP soil profile sketch
create_aqp_soil_sketch <- function(lat, lng, app_data) {
  if (is.null(lat) || is.null(lng) || is.null(app_data$polygons)) {
    return(NULL)
  }
  
  tryCatch({
    # Find the polygon at the clicked location
    point <- st_sfc(st_point(c(lng, lat)), crs = 4326)
    point_proj <- st_transform(point, st_crs(app_data$polygons))
    
    # Find intersecting polygon
    intersection <- st_intersects(point_proj, app_data$polygons)
    if (length(intersection[[1]]) == 0) {
      return(NULL)
    }
    
    polygon_idx <- intersection[[1]][1]
    selected_polygon <- app_data$polygons[polygon_idx, ]
    
    # Get soil data for the selected polygon
    soil_data <- get_soil_data_for_sketch(selected_polygon$MUKEY, app_data$components)
    
    if (is.null(soil_data) || nrow(soil_data) == 0) {
      return(NULL)
    }
    
    # Create AQP soil profile object
    sp <- SoilProfileCollection(
      id = selected_polygon$MUKEY,
      horizons = soil_data,
      depthcols = c("top", "bottom")
    )
    
    # Create AQP plot
    plot(sp, name = "horizon", color = "color", 
         main = paste("Soil Profile:", selected_polygon$muname))
    
  }, error = function(e) {
    cat("Error creating AQP soil sketch:", e$message, "\n")
    return(NULL)
  })
}

#' Get selected polygon from coordinates
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param polygons sf object with polygon data
#' @return sf object with selected polygon or NULL
get_selected_polygon <- function(lat, lng, polygons) {
  if (is.null(lat) || is.null(lng) || is.null(polygons)) {
    return(NULL)
  }
  
  tryCatch({
    # Find the polygon at the clicked location
    point <- st_sfc(st_point(c(lng, lat)), crs = 4326)
    point_proj <- st_transform(point, st_crs(polygons))
    
    # Find intersecting polygon
    intersection <- st_intersects(point_proj, polygons)
    if (length(intersection[[1]]) == 0) {
      return(NULL)
    }
    
    polygon_idx <- intersection[[1]][1]
    selected_polygon <- polygons[polygon_idx, ]
    
    return(selected_polygon)
    
  }, error = function(e) {
    cat("Error getting selected polygon:", e$message, "\n")
    return(NULL)
  })
} 