# config.R - Configuration and Constants
# Cascade-Siskiyou National Monument Soil Explorer Configuration

# Projection and Map Settings ----
PROJECTION_CRS <- CRS("+init=EPSG:3857")
MAP_CENTER <- list(lat = 42.1, lng = -122.466, zoom = 11)

# Depth configuration with improved color schemes ----
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

# Soil order color palette ----
SOIL_ORDER_COLORS <- c(
  Alfisols = "#B5D55D", Andisols = "#EA028C", Aridisols = "#FDDCB9",
  Entisols = "#75CDD6", Gelisols = "#31A4BF", Histosols = "#AE5044",
  Inceptisols = "#CB7662", Mollisols = "#00A551", Oxisols = "#EC1F25",
  Spodosols = "#D4BEC4", Ultisols = "#FAAF19", Vertisols = "#FFF100",
  Unknown = "#808080"
)

# File paths (can be made configurable via environment variables) ----
DATA_PATHS <- list(
  oc_raster = "CSNM_OC_AllDepths.tif",
  ph_raster = "CSNM_pH_AllDepths.tif",
  mapunit_table = "Mapunit_OR_table.csv",
  soil_polygons = "CSNM_Polygons_with_Data.geojson"
)