# Barebones Leaflet Map for CSNM Major Soil Components

# Load libraries
library(leaflet)
library(sf)
library(RColorBrewer)

# Load data
soil_data <- st_read("CSNM_Polygons_MajorOnly.geojson", quiet = TRUE)

# Find soil order column and unique values
if ("taxorder" %in% names(soil_data)) {
  color_column <- "taxorder"
} else {
  order_cols <- names(soil_data)[grepl("order|tax", names(soil_data), ignore.case = TRUE)]
  color_column <- order_cols[1]
}

unique_orders <- unique(soil_data[[color_column]])
unique_orders <- unique_orders[!is.na(unique_orders) & unique_orders != ""]

# Create color palette
n_colors <- length(unique_orders)
if (n_colors <= 12) {
  colors <- RColorBrewer::brewer.pal(min(n_colors, 12), "Set3")
} else {
  colors <- rainbow(n_colors)
}

pal <- colorFactor(palette = colors, domain = unique_orders, na.color = "gray")

# Get map center
bbox <- st_bbox(soil_data)
center_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
center_lng <- mean(c(bbox["xmin"], bbox["xmax"]))

# Create map
leaflet(soil_data) %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addPolygons(
    fillColor = ~pal(get(color_column)),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "yellow",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = ~paste0(
      "<strong>MUKEY:</strong> ", 
      if ("MUKEY" %in% names(soil_data)) MUKEY else "N/A", "<br>",
      "<strong>Component:</strong> ", 
      if ("compname" %in% names(soil_data)) compname else "N/A", "<br>",
      "<strong>Soil Order:</strong> ", get(color_column), "<br>",
      "<strong>Percentage:</strong> ", 
      if ("comppct_r" %in% names(soil_data)) paste0(comppct_r, "%") else "N/A"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~get(color_column),
    title = "Soil Orders",
    position = "bottomright"
  ) %>%
  setView(lng = center_lng, lat = center_lat, zoom = 11)