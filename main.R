# app.R

# ——————————————————————————————————————————
library(shiny)
library(dplyr)      # load before sf to avoid select mask
library(sf)
library(leaflet)
library(raster)     # for projectRaster(), cellStats(), getValues()
library(htmlwidgets)
library(magrittr)
# ——————————————————————————————————————————

# 0) Load your clipped OC & pH rasters
oc_clipped <- raster("CSNM_OC_0-5cm.tif")
ph_clipped <- raster("CSNM_pH_0-5cm.tif")

# 0.1) Reproject both with nearest‐neighbor to keep NAs outside
crs_3857 <- CRS("+init=EPSG:3857")
oc_proj  <- projectRaster(oc_clipped, crs = crs_3857, method = "ngb")
ph_proj  <- projectRaster(ph_clipped, crs = crs_3857, method = "ngb")

# 0.2) Convert pH to real values (divide by 10)
ph_dec   <- ph_proj / 10

# 0.3) Build domains, dropping the absolute minimum (ghost cells)
v_oc      <- na.omit(getValues(oc_proj)); uv_oc <- sort(unique(v_oc))
domain_oc <- if(length(uv_oc) > 1) c(uv_oc[2], max(uv_oc)) else range(uv_oc)

v_ph      <- na.omit(getValues(ph_dec)); uv_ph <- sort(unique(v_ph))
domain_ph <- if(length(uv_ph) > 1) c(uv_ph[2], max(uv_ph)) else range(uv_ph)

# 0.4) Palettes (NA transparent)
palOC <- colorNumeric(c("white","brown"), domain = domain_oc, na.color = "transparent")
palPH <- colorNumeric(c("blue","green"), domain = domain_ph, na.color = "transparent")

# 1) Prepare soil polygons & component info
mapunit_table <- read.csv("Mapunit_OR_table.csv", stringsAsFactors = FALSE) %>%
  rename(MUKEY = mukey, muname = muname) %>% mutate(MUKEY = as.character(MUKEY))

raw_polygons <- st_read("CSNM_Polygons_with_Data.geojson", quiet = TRUE) %>%
  st_transform(4326) %>% st_cast("POLYGON") %>%
  mutate(MUKEY = as.character(MUKEY),
         taxorder = ifelse(is.na(taxorder)|taxorder=="","Unknown",taxorder)) %>%
  left_join(mapunit_table, by = "MUKEY")

comp_info   <- raw_polygons %>% st_drop_geometry() %>%
  distinct(MUKEY, compname, comppct_r, majcompflag, taxorder)

major_order <- comp_info %>%
  group_by(MUKEY) %>%
  arrange(desc(majcompflag == "Yes"), desc(comppct_r)) %>%
  slice(1) %>% ungroup() %>%
  dplyr::select(MUKEY, major_taxorder = taxorder)

soil_polygons <- raw_polygons %>%
  left_join(major_order, by = "MUKEY") %>%
  st_simplify(dTolerance = 0.0001)
soil_polygons$id_ <- seq_len(nrow(soil_polygons))

# 2) Soil‐order palette
order_colors  <- c(
  Alfisols="#B5D55D", Andisols="#EA028C", Aridisols="#FDDCB9",
  Entisols="#75CDD6", Gelisols="#31A4BF", Histosols="#AE5044",
  Inceptisols="#CB7662", Mollisols="#00A551", Oxisols="#EC1F25",
  Spodosols="#D4BEC4", Ultisols="#FAAF19", Vertisols="#FFF100",
  Unknown="#000000"
)
palMajor     <- colorFactor(order_colors, domain = names(order_colors))
legend_labels <- names(order_colors)
legend_colors <- unname(order_colors)

# ——————————————————————————————————————————
ui <- fluidPage(
  titlePanel("CSNM Interactive Map"),
  tags$head(tags$style(HTML("
    /* Move soil‐order legend down to match addLegend */
    #soilLegend { top:80px !important; }
    /* Mouse coords above bottom by 40px */
    #mouseBox { position:absolute; bottom:40px; right:10px; }
  "))),
  
  div(style="position:relative; height:80vh;",
      leafletOutput("soilMap", width="100%", height="100%"),
      
      # Soil‐order legend (static)
      conditionalPanel(
        condition = "input.mapType == 'soil'",
        absolutePanel(
          id="soilLegend", top=10, left=10, draggable=FALSE,
          style="background:rgba(255,255,255,0.9); padding:10px;
                 border-radius:8px; width:140px; z-index:500;",
          h4("Soil Orders", style="margin:0 0 6px; font-size:14px;"),
          lapply(seq_along(legend_labels), function(i) {
            tags$div(style="display:flex; align-items:center; margin-bottom:4px;",
                     tags$div(style=paste0(
                       "width:16px; height:16px; background:", legend_colors[i],
                       "; margin-right:6px; border:1px solid #666;"
                     )),
                     tags$span(legend_labels[i], style="font-size:12px;")
            )
          })
        )
      ),
      
      # Controls
      absolutePanel(
        top=10, right=10, width=260, draggable=FALSE,
        style="background:rgba(255,255,255,0.8); padding:10px;
               border-radius:8px; z-index:1000;",
        div(class="info-panel",
            radioButtons("mapType", "Map Selection:",
                         c("Soil Order Map" = "soil",
                           "Satellite Map"  = "sat",
                           "Organic C Map"  = "oc",
                           "pH Map"         = "ph"),
                         selected = "soil"),
            tags$hr(),
            checkboxGroupInput("overlaySel", "Overlay Selection:",
                               choices = c("Map Unit Overlay"),
                               selected = "Map Unit Overlay")
        )
      ),
      
      div(id="mouseBox", textOutput("mouseCoords", inline=TRUE))
  )
)

server <- function(input, output, session) {
  
  # Base map render
  output$soilMap <- renderLeaflet({
    leaflet(options = leafletOptions(doubleClickZoom = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldTerrain,  group = "Terrain") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      # Soil polygons
      addPolygons(data = soil_polygons, layerId = ~id_,
                  group = "filled", fillColor = ~palMajor(major_taxorder),
                  fillOpacity = 0.5, color = "white", weight = 0) %>%
      addPolygons(data = soil_polygons, layerId = ~id_,
                  group = "empty", fillOpacity = 0,
                  color = "white", weight = 1,
                  highlightOptions = highlightOptions(color = "yellow", weight = 3)) %>%
      
      # Rasters with higher opacity
      addRasterImage(oc_proj, colors = palOC, opacity = 1,
                     group = "oc", project = FALSE) %>%
      addRasterImage(ph_dec, colors = palPH, opacity = 1,
                     group = "ph", project = FALSE) %>%
      
      showGroup(c("Terrain","filled")) %>%
      hideGroup(c("Satellite","empty","oc","ph")) %>%
      
      onRender("
        function(el, x) {
          this.on('mousemove', function(e) {
            Shiny.setInputValue('mousePos', {
              lat: e.latlng.lat.toFixed(5),
              lng: e.latlng.lng.toFixed(5)
            }, {priority:'event'});
          });
        }
      ")
  })
  
  # Toggle layers & legends
  observe({
    proxy   <- leafletProxy("soilMap")
    overlay <- "Map Unit Overlay" %in% (input$overlaySel %||% "")
    
    proxy %>% clearControls() %>%
      hideGroup(c("Terrain","Satellite","filled","empty","oc","ph"))
    
    if (input$mapType == "soil") {
      proxy %>% showGroup(c("Terrain","filled"))
      # static legend remains
    }
    else if (input$mapType == "sat") {
      proxy %>% showGroup("Satellite")
    }
    else if (input$mapType == "oc") {
      proxy %>% showGroup(c("Terrain","oc")) %>%
        addLegend(pal = palOC, values = domain_oc,
                  title = "Organic C (0–5 cm)",
                  position = "topleft")
    }
    else if (input$mapType == "ph") {
      proxy %>% showGroup(c("Terrain","ph")) %>%
        addLegend(pal = palPH,
                  values = seq(domain_ph[1], domain_ph[2], length.out = 6),
                  labFormat = labelFormat(digits = 1),
                  title = "pH (0–5 cm)",
                  position = "topleft")
    }
    
    if (overlay) proxy %>% showGroup("empty") else proxy %>% hideGroup("empty")
    proxy %>% clearGroup("highlight") %>% clearPopups()
  })
  
  # Click popups
  observeEvent(input$soilMap_shape_click, {
    clk <- input$soilMap_shape_click; id <- clk$id
    if (is.null(id) || !(id %in% soil_polygons$id_)) return()
    
    feat <- filter(soil_polygons, id_ == id)
    sel  <- comp_info %>% filter(MUKEY == feat$MUKEY) %>% arrange(desc(comppct_r))
    
    popup_html <- tags$div(
      style = "max-width:260px;font-size:12px;",
      tags$div(strong("Description:")),
      tags$div(feat$muname),
      tags$hr(style="border:none;border-top:2px solid #333;"),
      tags$div(strong("Composition:")),
      tags$ul(lapply(seq_len(nrow(sel)), function(i) {
        r <- sel[i,]
        order_lbl <- if (r$taxorder == "Unknown") {
          "None"
        } else {
          tags$a(
            href   = paste0(
              "https://www.uidaho.edu/cals/soil-orders/",
              tolower(gsub("\\s+","-", r$taxorder))
            ),
            target = "_blank",
            r$taxorder
          )
        }
        txt <- paste0(r$compname, " — ", r$comppct_r, "%")
        if (r$majcompflag == "Yes") tags$li(strong(HTML(paste(txt, order_lbl))))
        else                        tags$li(HTML(paste(txt, order_lbl)))
      })),
      tags$hr(style="border:none;border-top:2px solid #333;"),
      tags$div(strong("Map Unit Data:")),
      tags$p(strong("Sub-Order: "), feat$taxsuborder),
      tags$p(strong("MUKEY: "), feat$MUKEY)
    )
    
    leafletProxy("soilMap") %>%
      clearGroup("highlight") %>%
      clearPopups() %>%
      addPolygons(data = feat, color = "yellow", weight = 3,
                  fill = FALSE, group = "highlight") %>%
      addPopups(lng = clk$lng, lat = clk$lat,
                popup = as.character(popup_html),
                options = popupOptions(closeButton = TRUE, closeOnClick = FALSE))
  })
  
  # Mouse coords
  output$mouseCoords <- renderText({
    pos <- input$mousePos
    if (is.null(pos)) "Move over map for coordinates"
    else paste0("Lat: ", pos$lat, "  |  Lng: ", pos$lng)
  })
}

shinyApp(ui, server)
