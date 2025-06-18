# data_module.R - Data Loading and Processing Functions

#' Load and prepare all spatial data
#' @return List containing polygons, components, and raster data
load_and_prepare_data <- function() {
  cat("Starting data loading process...\n")
  
  # Load raster data
  raster_data <- load_raster_data()
  
  # Load polygon data
  polygon_data <- load_polygon_data()
  
  return(list(
    polygons = polygon_data$polygons,
    components = polygon_data$components,
    rasters = raster_data
  ))
}

#' Load and process multi-band raster stacks
#' @return List containing processed OC and pH raster data
load_raster_data <- function() {
  cat("Loading raster data...\n")
  
  raster_data <- list()
  
  # Load Organic Carbon stack
  raster_data$oc <- load_oc_rasters()
  
  # Load pH stack
  raster_data$ph <- load_ph_rasters()
  
  return(raster_data)
}

#' Load and process Organic Carbon rasters
#' @return List of processed OC raster layers
load_oc_rasters <- function() {
  oc_stack <- tryCatch({
    stack(DATA_PATHS$oc_raster)
  }, error = function(e) {
    warning("Could not load OC multi-band raster: ", e$message)
    return(NULL)
  })
  
  if (is.null(oc_stack)) return(NULL)
  
  cat("Processing OC stack with", nlayers(oc_stack), "bands\n")
  
  # Reproject stack to Web Mercator
  oc_proj <- projectRaster(oc_stack, crs = PROJECTION_CRS, method = "ngb")
  
  # Create individual processed layers for each depth
  oc_layers <- list()
  for (i in 1:nlayers(oc_proj)) {
    oc_layers[[i]] <- process_oc_layer(oc_proj[[i]], i)
  }
  
  return(oc_layers)
}

#' Load and process pH rasters
#' @return List of processed pH raster layers
load_ph_rasters <- function() {
  ph_stack <- tryCatch({
    stack(DATA_PATHS$ph_raster)
  }, error = function(e) {
    warning("Could not load pH multi-band raster: ", e$message)
    return(NULL)
  })
  
  if (is.null(ph_stack)) return(NULL)
  
  cat("Processing pH stack with", nlayers(ph_stack), "bands\n")
  
  # Reproject and convert pH (divide by 10 for decimal pH)
  ph_proj <- projectRaster(ph_stack, crs = PROJECTION_CRS, method = "ngb")
  ph_decimal <- ph_proj / 10
  
  # Create individual processed layers for each depth
  ph_layers <- list()
  for (i in 1:nlayers(ph_decimal)) {
    ph_layers[[i]] <- process_ph_layer(ph_decimal[[i]], i)
  }
  
  return(ph_layers)
}

#' Process individual OC layer with appropriate color palette
#' @param layer RasterLayer object
#' @param depth_index Integer index for depth level
#' @return List with processed raster, domain, and palette
process_oc_layer <- function(layer, depth_index) {
  values <- na.omit(getValues(layer))
  unique_vals <- sort(unique(values))
  
  domain <- if(length(unique_vals) > 1) {
    c(unique_vals[2], max(unique_vals))
  } else {
    range(unique_vals)
  }
  
  # Choose color palette based on depth
  color_palette <- get_oc_color_palette(depth_index)
  
  return(list(
    raster = layer,
    domain = domain,
    palette = colorNumeric(color_palette, domain = domain, na.color = "transparent")
  ))
}

#' Process individual pH layer
#' @param layer RasterLayer object
#' @param depth_index Integer index for depth level
#' @return List with processed raster, domain, and palette
process_ph_layer <- function(layer, depth_index) {
  values <- na.omit(getValues(layer))
  unique_vals <- sort(unique(values))
  
  domain <- if(length(unique_vals) > 1) {
    c(unique_vals[2], max(unique_vals))
  } else {
    range(unique_vals)
  }
  
  return(list(
    raster = layer,
    domain = domain,
    palette = colorNumeric(DEPTH_LEVELS$color_palettes$ph, domain = domain, na.color = "transparent")
  ))
}

#' Get appropriate color palette for OC based on depth
#' @param depth_index Integer index for depth level
#' @return Character vector of colors
get_oc_color_palette <- function(depth_index) {
  if (depth_index <= 2) {
    # Surface layers (0-5, 5-15cm): traditional brown palette
    return(DEPTH_LEVELS$color_palettes$oc$surface)
  } else if (depth_index <= 4) {
    # Shallow layers (15-30, 30-60cm): muted browns
    return(DEPTH_LEVELS$color_palettes$oc$shallow)
  } else {
    # Deep layers (60-100, 100-200cm): blue-gray palette for low values
    return(DEPTH_LEVELS$color_palettes$oc$deep)
  }
}

#' Load and process polygon data
#' @return List containing processed polygons and component information
load_polygon_data <- function() {
  cat("Loading polygon data...\n")
  
  # Load mapunit table
  mapunit_table <- load_mapunit_table()
  
  # Load soil polygons
  soil_polygons <- load_soil_polygons(mapunit_table)
  
  if (is.null(soil_polygons)) {
    return(list(polygons = NULL, components = NULL))
  }
  
  # Add unique ID for each polygon
  soil_polygons$polygon_id <- seq_len(nrow(soil_polygons))
  
  # Process component information
  component_info <- extract_component_info(soil_polygons)
  major_taxorder <- get_major_taxonomic_orders(component_info)
  
  # Join major order back to polygons and clean up soil orders
  soil_polygons <- soil_polygons %>%
    dplyr::left_join(major_taxorder, by = "MUKEY") %>%
    dplyr::mutate(major_taxorder = clean_soil_order_names(major_taxorder))
  
  return(list(
    polygons = soil_polygons,
    components = component_info
  ))
}

#' Load mapunit table
#' @return Data frame with mapunit information
load_mapunit_table <- function() {
  tryCatch({
    read.csv(DATA_PATHS$mapunit_table, stringsAsFactors = FALSE) %>%
      dplyr::rename(MUKEY = mukey, muname = muname) %>%
      dplyr::mutate(MUKEY = as.character(MUKEY))
  }, error = function(e) {
    warning("Could not load mapunit table: ", e$message)
    data.frame(MUKEY = character(), muname = character())
  })
}

#' Load soil polygons
#' @param mapunit_table Data frame with mapunit information
#' @return sf object with soil polygon data
load_soil_polygons <- function(mapunit_table) {
  tryCatch({
    st_read(DATA_PATHS$soil_polygons, quiet = TRUE) %>%
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
}

#' Extract component information from soil polygons
#' @param soil_polygons sf object with soil data
#' @return Data frame with component information
extract_component_info <- function(soil_polygons) {
  soil_polygons %>%
    st_drop_geometry() %>%
    dplyr::distinct(MUKEY, compname, comppct_r, majcompflag, taxorder) %>%
    dplyr::filter(!is.na(MUKEY))
}

#' Get major taxonomic orders for each map unit
#' @param component_info Data frame with component information
#' @return Data frame with major taxonomic orders
get_major_taxonomic_orders <- function(component_info) {
  component_info %>%
    dplyr::group_by(MUKEY) %>%
    dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(MUKEY, major_taxorder = taxorder)
}

#' Clean up soil order names
#' @param major_taxorder Character vector of taxonomic orders
#' @return Character vector with cleaned names
clean_soil_order_names <- function(major_taxorder) {
  case_when(
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
}