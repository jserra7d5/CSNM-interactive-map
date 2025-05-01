# install.packages(c("sf", "leaflet", "magrittr"))

library(sf)
library(leaflet)
library(magrittr)


soil_polygons <- st_read("CSNM_Polygons_with_Data.geojson", quiet = FALSE) %>%
  st_transform(crs = 4326)


print(st_bbox(soil_polygons))


leaflet() %>%
  addTiles() %>%
  addPolygons(
    data       = soil_polygons,
    fillColor  = ~colorFactor("Set3", MUKEY)(MUKEY),
    fillOpacity= 0.6,
    color      = "white",
    weight     = 1,
    popup      = ~paste0(
      "<b>MUKEY:</b> ", MUKEY, "<br>",
      "<b>Component:</b> ", compname
    )
  )
