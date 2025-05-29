# Filter Monument GeoJSON to Major Soil Orders Only
# This script filters each map unit to keep only the major soil order with highest percentage

# Load required libraries
library(sf)
library(dplyr)

# Read the original GeoJSON file
cat("Loading monument GeoJSON...\n")
monument_data <- st_read("CSNM_Polygons_with_Data.geojson", quiet = TRUE)

cat("Original data dimensions:", nrow(monument_data), "rows,", ncol(monument_data), "columns\n")
cat("Unique map units (MUKEY):", length(unique(monument_data$MUKEY)), "\n")

# Check and fix coordinate system
current_crs <- st_crs(monument_data)
cat("Current CRS:", current_crs$input, "\n")

# Transform to WGS84 (EPSG:4326) for web compatibility if needed
if (current_crs$input != "EPSG:4326" && !is.na(current_crs$input)) {
  cat("Transforming to WGS84 for web compatibility...\n")
  monument_data <- st_transform(monument_data, 4326)
  cat("Transformation complete. New CRS: EPSG:4326\n")
} else if (is.na(current_crs$input)) {
  cat("Warning: No CRS defined. Setting to WGS84...\n")
  monument_data <- st_set_crs(monument_data, 4326)
} else {
  cat("Data already in WGS84\n")
}

# Verify the coordinates are reasonable
bbox <- st_bbox(monument_data)
cat("Coordinates - Lat:", round(bbox["ymin"], 4), "to", round(bbox["ymax"], 4),
    "Lng:", round(bbox["xmin"], 4), "to", round(bbox["xmax"], 4), "\n")

# Check if coordinates are reasonable for Oregon/California border
if (bbox["ymin"] >= 40 && bbox["ymax"] <= 48 &&
    bbox["xmin"] >= -125 && bbox["xmax"] <= -115) {
  cat("✓ Coordinates look correct for the Cascade-Siskiyou region!\n")
} else {
  cat("⚠ Warning: Coordinates may be incorrect for CSNM region\n")
  cat("Expected roughly: Lat 42-43, Lng -122 to -123\n")
}

# Check what we're working with
cat("\nSample of majcompflag values:\n")
print(table(monument_data$majcompflag, useNA = "ifany"))

cat("\nSample of comppct_r values (first 20):\n")
print(head(monument_data$comppct_r, 20))

# Filter to keep only the major component with highest percentage for each MUKEY
filtered_data <- monument_data %>%
  # Convert MUKEY to character for consistency
  mutate(MUKEY = as.character(MUKEY)) %>%
  
  # Group by MUKEY (map unit)
  group_by(MUKEY) %>%
  
  # Within each MUKEY, prioritize:
  # 1. Components marked as major (majcompflag == "Yes")
  # 2. Highest percentage (comppct_r)
  # 3. Keep only the top one
  arrange(
    desc(majcompflag == "Yes"),  # Major components first
    desc(comppct_r),             # Then highest percentage
    .by_group = TRUE
  ) %>%
  
  # Keep only the first (highest priority) row for each MUKEY
  slice(1) %>%
  
  # Ungroup
  ungroup()

cat("\nFiltered data dimensions:", nrow(filtered_data), "rows,", ncol(filtered_data), "columns\n")
cat("Unique map units after filtering:", length(unique(filtered_data$MUKEY)), "\n")

# Check the filtering results
cat("\nAfter filtering - majcompflag distribution:\n")
print(table(filtered_data$majcompflag, useNA = "ifany"))

cat("\nAfter filtering - percentage range:\n")
cat("Min percentage:", min(filtered_data$comppct_r, na.rm = TRUE), "\n")
cat("Max percentage:", max(filtered_data$comppct_r, na.rm = TRUE), "\n")
cat("Mean percentage:", round(mean(filtered_data$comppct_r, na.rm = TRUE), 1), "\n")

# Show some examples of what was kept
cat("\nSample of filtered data (first 5 rows):\n")
sample_data <- filtered_data %>%
  select(MUKEY, compname, comppct_r, majcompflag, taxorder) %>%
  st_drop_geometry() %>%
  head(5)
print(sample_data)

# Check for any map units that lost their major component
non_major_kept <- filtered_data %>%
  filter(majcompflag != "Yes") %>%
  nrow()

if (non_major_kept > 0) {
  cat("\nWarning:", non_major_kept, "map units had no major component, kept highest percentage component instead\n")
  
  # Show examples of these cases
  cat("Examples of map units with no major component:\n")
  examples <- filtered_data %>%
    filter(majcompflag != "Yes") %>%
    select(MUKEY, compname, comppct_r, majcompflag, taxorder) %>%
    st_drop_geometry() %>%
    head(3)
  print(examples)
}

# Write the filtered GeoJSON (ensuring WGS84)
output_filename <- "CSNM_Polygons_MajorOnly.geojson"
cat("\nWriting filtered data to:", output_filename, "\n")

# Ensure the output is definitely in WGS84
if (st_crs(filtered_data)$input != "EPSG:4326") {
  cat("Final coordinate system check - converting to WGS84...\n")
  filtered_data <- st_transform(filtered_data, 4326)
}

st_write(filtered_data, output_filename, delete_dsn = TRUE)

cat("Export complete! Output is in WGS84 (EPSG:4326) format.\n")
cat("Original file size:", file.size("CSNM_Polygons_with_Data.geojson"), "bytes\n")
cat("Filtered file size:", file.size(output_filename), "bytes\n")

# Optional: Create a summary report
cat("\nCreating summary report...\n")

summary_report <- filtered_data %>%
  st_drop_geometry() %>%
  group_by(taxorder) %>%
  summarise(
    count = n(),
    avg_percentage = round(mean(comppct_r, na.rm = TRUE), 1),
    min_percentage = min(comppct_r, na.rm = TRUE),
    max_percentage = max(comppct_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

cat("\nSummary by soil order (major components only):\n")
print(summary_report)

# Save summary to CSV
write.csv(summary_report, "CSNM_MajorComponents_Summary.csv", row.names = FALSE)
cat("\nSummary saved to: CSNM_MajorComponents_Summary.csv\n")

cat("\n=== FILTERING COMPLETE ===\n")
cat("Input file: CSNM_Polygons_with_Data.geojson\n")
cat("Output file:", output_filename, "\n")
cat("Output CRS: EPSG:4326 (WGS84) - Ready for web mapping!\n")
cat("Rows before filtering:", nrow(monument_data), "\n")
cat("Rows after filtering:", nrow(filtered_data), "\n")
cat("Reduction:", nrow(monument_data) - nrow(filtered_data), "rows removed\n")
cat("File is now compatible with geojson.io, Leaflet, and other web mapping tools!\n")