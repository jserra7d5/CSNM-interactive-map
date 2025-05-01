# install.packages(c("sf", "leaflet", "magrittr"))

library(sf)
library(leaflet)
library(magrittr)


soil_polygons <- st_read("CSNM_Polygons_with_Data.geojson", quiet = FALSE) %>%
  st_transform(crs = 4326)

orders <- unique(soil_polygons$taxorder)
pal <- colorFactor("Set3", domain = orders)
map <- leaflet() %>%
  addTiles()


for(o in orders) {
  map <- map %>%
    addPolygons(
      data       = subset(soil_polygons, taxorder == o),
      fillColor  = pal(o),
      fillOpacity= 0.6,
      color      = "white",
      weight     = 1,
      group      = o,
      popup      = ~paste0("<b>Order:</b> ", taxorder,
                           "<br><b>Taxonomic  sub-Order:</b> ", taxsuborder,
                           "<br><b>MUKEY:</b> ", MUKEY,
                           "<br><b>Component:</b> ", compname)
    )
}

map %>%
  addLayersControl(
    overlayGroups = orders,
    options       = layersControlOptions(collapsed = FALSE)
  )

#run names(soil_polygons) to get the component terms
