# test_aqp_integration.R - Testing script for AQP integration

# This script provides a simple test for the AQP integration
# Run this in your R console to test the basic functionality

# Test 1: Source all required modules
cat("Testing module loading...\n")
try({
  source("config.R")
  source("data_module.R")
  source("ui_module.R")
  source("map_utils.R")
  source("plot_utils.R")
  source("soil_aqp_module.R")
  source("server_module.R")
  cat("✓ All modules loaded successfully\n")
}, silent = FALSE)

# Test 2: Initialize AQP module
cat("Testing AQP module initialization...\n")
try({
  initialize_aqp_module()
  cat("✓ AQP module initialized successfully\n")
}, silent = FALSE)

# Test 3: Test soil series name cleaning
cat("Testing soil series name cleaning...\n")
try({
  test_names <- c("Cecil", "Drummer, variant", "Amador family", "50% Pentz")
  cleaned <- clean_soil_series_names(test_names)
  cat("Input:", paste(test_names, collapse = ", "), "\n")
  cat("Output:", paste(cleaned, collapse = ", "), "\n")
  cat("✓ Soil series name cleaning working\n")
}, silent = FALSE)

# Test 4: Test component extraction (mock data)
cat("Testing component extraction...\n")
try({
  mock_components <- data.frame(
    compname = c("Cecil", "Drummer", "Amador"),
    comppct_r = c(45, 30, 15),
    majcompflag = c("Yes", "No", "No"),
    stringsAsFactors = FALSE
  )
  extracted <- extract_soil_series_from_components(mock_components)
  cat("Extracted series:", paste(extracted, collapse = ", "), "\n")
  cat("✓ Component extraction working\n")
}, silent = FALSE)

# Test 5: Test NRCS data fetching (will only work with internet connection)
cat("Testing NRCS data fetching (requires internet)...\n")
try({
  # Test with common soil series
  test_series <- c("cecil", "drummer")
  profile_data <- fetch_nrcs_soil_profiles(test_series, color_state = "moist")
  
  if (!is.null(profile_data)) {
    cat("✓ NRCS data fetching successful\n")
    cat("Number of profiles:", length(profile_data$spc), "\n")
  } else {
    cat("⚠ NRCS data fetching returned NULL (check internet connection)\n")
  }
}, silent = FALSE)

cat("\nAQP integration test complete!\n")
cat("If all tests passed, the integration should work properly.\n")
cat("Run shiny::runApp() in your R console to start the application.\n")