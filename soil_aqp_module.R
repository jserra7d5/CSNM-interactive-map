# soil_aqp_module.R - AQP Soil Profile Visualization Module

#' Load required packages for AQP soil profile functionality
load_aqp_packages <- function() {
  required_packages <- c("aqp", "soilDB", "dplyr", "RColorBrewer", "shinyjs")
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing missing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

#' Fetch soil profile data from NRCS database
#' @param soil_series_names Character vector of soil series names
#' @param color_state Either "moist" or "dry" for soil colors
#' @return List containing SoilProfileCollection and metadata
fetch_nrcs_soil_profiles <- function(soil_series_names, color_state = "moist") {
  
  if (length(soil_series_names) == 0) {
    return(NULL)
  }
  
  # Clean and validate soil series names
  clean_names <- clean_soil_series_names(soil_series_names)
  
  if (length(clean_names) == 0) {
    warning("No valid soil series names provided")
    return(NULL)
  }
  
  cat("Fetching NRCS data for soil series:", paste(clean_names, collapse = ", "), "\n")
  
  tryCatch({
    # Fetch soil profile data using soilDB
    soil_data <- fetchOSD(clean_names, colorState = color_state, extended = TRUE)
    
    if (is.null(soil_data) || is.null(soil_data$SPC) || length(soil_data$SPC) == 0) {
      warning("No soil profile data found for the specified series")
      return(NULL)
    }
    
    # Prepare soil colors for visualization
    spc <- soil_data$SPC
    spc <- prepare_soil_colors_aqp(spc)
    
    return(list(
      spc = spc,
      metadata = soil_data[names(soil_data) != "SPC"]
    ))
    
  }, error = function(e) {
    warning("Failed to fetch NRCS soil data: ", e$message)
    return(NULL)
  })
}

#' Clean and standardize soil series names for NRCS lookup
#' @param raw_names Character vector of raw soil series names
#' @return Character vector of cleaned names
clean_soil_series_names <- function(raw_names) {
  if (is.null(raw_names) || length(raw_names) == 0) {
    return(character(0))
  }
  
  # Convert factors to characters
  if (is.factor(raw_names)) {
    raw_names <- as.character(raw_names)
  }
  
  # Handle comma-separated values
  if (length(raw_names) == 1 && grepl(",", raw_names)) {
    raw_names <- unlist(strsplit(raw_names, ","))
  }
  
  # Clean up names
  cleaned <- gsub("\\s+(variant|taxadjunct|family).*$", "", raw_names, ignore.case = TRUE)
  cleaned <- gsub("\\s*\\d+%\\s*", "", cleaned)  # Remove percentages
  cleaned <- trimws(cleaned)
  cleaned <- cleaned[cleaned != "" & !is.na(cleaned)]
  
  # Standardize capitalization for NRCS lookup
  cleaned <- tolower(cleaned)
  
  # Remove duplicates
  unique(cleaned)
}

#' Prepare soil colors for AQP visualization
#' @param spc SoilProfileCollection object
#' @return SoilProfileCollection with prepared colors
prepare_soil_colors_aqp <- function(spc) {
  
  h_data <- horizons(spc)
  
  # Default color for missing data
  default_color <- "#F5F5F5"
  colors <- rep(default_color, nrow(h_data))
  
  # Check for available Munsell color columns
  has_moist <- all(c('moist_hue', 'moist_value', 'moist_chroma') %in% names(h_data))
  has_dry <- all(c('dry_hue', 'dry_value', 'dry_chroma') %in% names(h_data))
  
  if (has_moist) {
    # Use moist colors (preferred for visualization)
    valid_moist <- with(h_data, !is.na(moist_hue) & !is.na(moist_value) & !is.na(moist_chroma))
    if (any(valid_moist)) {
      tryCatch({
        colors[valid_moist] <- with(h_data[valid_moist, ], 
                                   munsell2rgb(moist_hue, moist_value, moist_chroma))
      }, error = function(e) {
        warning("Error converting moist Munsell colors: ", e$message)
      })
    }
  } else if (has_dry) {
    # Fallback to dry colors
    valid_dry <- with(h_data, !is.na(dry_hue) & !is.na(dry_value) & !is.na(dry_chroma))
    if (any(valid_dry)) {
      tryCatch({
        colors[valid_dry] <- with(h_data[valid_dry, ], 
                                 munsell2rgb(dry_hue, dry_value, dry_chroma))
      }, error = function(e) {
        warning("Error converting dry Munsell colors: ", e$message)
      })
    }
  }
  
  # Add color column to horizons
  h_data$soil_color <- colors
  horizons(spc) <- h_data
  
  return(spc)
}

#' Extract soil series names from map unit components
#' @param map_unit_components Data frame with component information
#' @return Character vector of soil series names
extract_soil_series_from_components <- function(map_unit_components) {
  
  if (is.null(map_unit_components) || nrow(map_unit_components) == 0) {
    return(character(0))
  }
  
  # Extract component names, prioritizing major components
  component_names <- map_unit_components %>%
    dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r)) %>%
    dplyr::pull(compname) %>%
    na.omit() %>%
    as.character()
  
  # Take top 3-4 components to avoid overcrowding
  top_components <- head(component_names, 4)
  
  return(top_components)
}

#' Create AQP soil profile plot optimized for Shiny sidebar display
#' @param spc SoilProfileCollection object
#' @param map_unit_info Optional map unit information for title
#' @param plot_width Width constraint for sidebar display
#' @return Base graphics plot (returns invisibly)
create_aqp_soil_profile_plot <- function(spc, map_unit_info = NULL, plot_width = 350) {
  
  if (is.null(spc) || length(spc) == 0) {
    plot.new()
    text(0.5, 0.5, "No soil profile data available", 
         cex = 1.2, col = "gray50", adj = c(0.5, 0.5))
    return(invisible())
  }
  
  n_profiles <- length(spc)
  
  # Ensure horizon designation is set
  if (is.na(hzdesgnname(spc))) {
    hzdesgnname(spc) <- 'hzname'
  }
  
  # Sidebar-optimized parameters
  profile_width <- case_when(
    n_profiles == 1 ~ 0.7,      # Single profile - wider
    n_profiles == 2 ~ 0.45,     # Two profiles - medium
    n_profiles <= 4 ~ 0.3,      # 3-4 profiles - narrower
    TRUE ~ 0.2                  # 5+ profiles - very narrow
  )
  
  text_size <- case_when(
    n_profiles <= 2 ~ 0.85,
    n_profiles <= 4 ~ 0.7,
    TRUE ~ 0.6
  )
  
  # Set margins for sidebar display
  par(mar = c(2, 1, 3, 1), bg = "white", fg = "black")
  
  # Create the soil profile plot
  plotSPC(spc,
          # Visual styling
          color = 'soil_color',
          name = hzdesgnname(spc),
          
          # Sizing for sidebar
          width = profile_width,
          cex.names = text_size,
          cex.id = text_size * 1.1,
          
          # Horizon depth labels
          hz.depths = TRUE,
          hz.depths.offset = case_when(
            n_profiles <= 2 ~ 0.1,
            n_profiles <= 4 ~ 0.08,
            TRUE ~ 0.05
          ),
          hz.depths.lines = TRUE,
          fixLabelCollisions = TRUE,
          
          # Layout optimization
          name.style = 'center-center',
          
          # Depth axis (conditional based on profile count)
          depth.axis = if (n_profiles <= 3) {
            list(style = 'compact', line = -1.5, cex = 0.75)
          } else {
            FALSE
          },
          
          # Depth and styling
          max.depth = 200,
          divide.hz = TRUE,
          lwd = 1,
          default.color = grey(0.95))
  
  # Add informative title
  title_text <- if (!is.null(map_unit_info) && !is.null(map_unit_info$muname)) {
    paste0("NRCS Profiles: ", substr(map_unit_info$muname, 1, 30))
  } else {
    paste0("NRCS Soil Profiles (", n_profiles, ")")
  }
  
  title(title_text, 
        line = 1.8, 
        cex.main = min(1.0, max(0.8, 1.0 - (n_profiles * 0.03))))
  
  # Add profile count subtitle for multiple profiles
  if (n_profiles > 1) {
    mtext(paste(n_profiles, "soil series"), 
          line = 0.5, 
          cex = 0.65, 
          col = "grey50")
  }
  
  return(invisible())
}

#' Create profile summary information table
#' @param spc SoilProfileCollection object
#' @param map_unit_components Component data with percentages
#' @return Data frame with profile summary
create_profile_summary_table <- function(spc, map_unit_components = NULL) {
  
  if (is.null(spc) || length(spc) == 0) {
    return(data.frame(Message = "No profile data available"))
  }
  
  # Basic profile information
  profile_summary <- data.frame(
    Series = profile_id(spc),
    Horizons = sapply(1:length(spc), function(i) nrow(horizons(spc[i, ]))),
    Depth_cm = sapply(1:length(spc), function(i) {
      hz_data <- horizons(spc[i, ])
      if (nrow(hz_data) > 0) {
        max(hz_data$bottom, na.rm = TRUE)
      } else {
        NA
      }
    }),
    stringsAsFactors = FALSE
  )
  
  # Add component percentages if available
  if (!is.null(map_unit_components)) {
    component_pct <- map_unit_components %>%
      dplyr::arrange(desc(majcompflag == "Yes"), desc(comppct_r)) %>%
      dplyr::slice(1:nrow(profile_summary)) %>%
      dplyr::pull(comppct_r)
    
    if (length(component_pct) > 0) {
      profile_summary$Percent <- component_pct[1:nrow(profile_summary)]
    }
  }
  
  return(profile_summary)
}

#' Create reactive expression for soil profile data in Shiny
#' @param map_unit_components Reactive expression returning component data
#' @return Reactive expression returning soil profile data
create_soil_profile_reactive <- function(map_unit_components) {
  reactive({
    
    components <- map_unit_components()
    if (is.null(components) || nrow(components) == 0) {
      return(NULL)
    }
    
    # Extract soil series names
    soil_series <- extract_soil_series_from_components(components)
    
    if (length(soil_series) == 0) {
      return(NULL)
    }
    
    # Fetch NRCS soil profile data
    profile_data <- fetch_nrcs_soil_profiles(soil_series, color_state = "moist")
    
    return(profile_data)
  })
}

#' Initialize AQP module (call this in app startup)
initialize_aqp_module <- function() {
  cat("Initializing AQP soil profile module...\n")
  
  # Load required packages
  load_aqp_packages()
  
  cat("AQP module initialized successfully\n")
}