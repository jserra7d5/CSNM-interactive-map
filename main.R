# app.R

# ——————————————————————————————————————————
# Libraries
library(shiny)
library(sf)
library(leaflet)
library(raster)
library(htmlwidgets)
library(magrittr)
library(dplyr)
# ——————————————————————————————————————————

# 0) Load your OC & pH rasters (assumes they live alongside app.R)
oc_raster <- raster("CSNM_OC_0-5cm.tif")
ph_raster <- raster("CSNM_pH_0-5cm.tif")

# Define continuous palettes matching your GEE viz
palOC <- colorNumeric(palette = c("white","brown"), domain = c(0,50), na.color = "transparent")
palPH <- colorNumeric(palette = c("blue","green"), domain = c(3,9), na.color = "transparent")

# 1) Map‐unit name lookup
mapunit_table <- read.csv("Mapunit_OR_table.csv", stringsAsFactors = FALSE) %>%
  rename(MUKEY = mukey, muname = muname) %>%
  mutate(MUKEY = as.character(MUKEY))

# 2) Load & split MULTI→single POLYGONs, join mapunit names
raw_polygons <- st_read("CSNM_Polygons_with_Data.geojson", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_cast("POLYGON") %>%
  mutate(
    MUKEY    = as.character(MUKEY),
    taxorder = ifelse(is.na(taxorder) | taxorder == "", "Unknown", taxorder)
  ) %>%
  left_join(mapunit_table, by = "MUKEY")

comp_info <- raw_polygons %>%
  st_drop_geometry() %>%
  distinct(MUKEY, compname, comppct_r, majcompflag, taxorder)

major_order <- comp_info %>%
  group_by(MUKEY) %>%
  arrange(desc(majcompflag == "Yes"), desc(comppct_r)) %>%
  slice(1) %>%
  ungroup() %>%
  select(MUKEY, major_taxorder = taxorder)

soil_polygons <- raw_polygons %>%
  left_join(major_order, by = "MUKEY") %>%
  st_simplify(dTolerance = 0.0001)
soil_polygons$id_ <- seq_len(nrow(soil_polygons))

# 3) Palette for major orders
order_colors <- c(
  "Alfisols"    = "#B5D55D", "Andisols"    = "#EA028C",
  "Aridisols"   = "#FDDCB9", "Entisols"    = "#75CDD6",
  "Gelisols"    = "#31A4BF", "Histosols"   = "#AE5044",
  "Inceptisols" = "#CB7662", "Mollisols"   = "#00A551",
  "Oxisols"     = "#EC1F25", "Spodosols"   = "#D4BEC4",
  "Ultisols"    = "#FAAF19", "Vertisols"   = "#FFF100",
  "Unknown"     = "#000000"
)
palMajor <- colorFactor(order_colors, domain = names(order_colors))
legend_labels <- names(order_colors)
legend_colors <- unname(order_colors)

# ——————————————————————————————————————————
ui <- fluidPage(
  titlePanel("CSNM Interactive Map"),
  tags$head(tags$style(HTML("
    .info-panel summary::marker { content: none; }
    .info-panel summary::before { content: '▴'; font-size:1.5em; margin-right:0.5em; }
    .info-panel details[open] summary::before { content: '▾'; font-size:1.5em; }
    .info-panel summary { cursor: pointer; }
    #mouseBox {
      position:absolute; bottom:30px; right:10px;
      background:rgba(255,255,255,0.75);
      padding:6px 8px; border-radius:4px; font-size:12px; z-index:1000;
    }
    #legend {
      position:absolute; top:10px; left:10px;
      background:rgba(255,255,255,0.9);
      padding:10px; border-radius:8px; font-size:12px;
      z-index:1000; max-width:180px;
    }
    #legend .item { display:flex; align-items:center; margin-bottom:4px; }
    #legend .swatch { width:16px; height:16px; margin-right:6px; border:1px solid #666; }
    .info-panel hr { border:none; border-top:2px solid #111; margin:12px 0; }
  "))),
  
  div(style="position:relative; height:80vh;",
      leafletOutput("soilMap", width="100%", height="100%"),
      
      # Soil legend
      conditionalPanel(
        condition = "input.mapType == 'soil'",
        absolutePanel(
          id="legend", draggable=FALSE,
          h4("Soil Orders", style="margin-top:0; font-size:14px;"),
          lapply(seq_along(legend_labels), function(i) {
            tags$div(class="item",
                     tags$div(class="swatch", style=paste0("background:", legend_colors[i], ";")),
                     tags$span(legend_labels[i])
            )
          })
        )
      ),
      
      # Controls panel
      absolutePanel(
        top=10, right=10, width=260, draggable=FALSE,
        style="background:rgba(255,255,255,0.8); padding:10px; border-radius:8px; z-index:1000;",
        div(class="info-panel",
            radioButtons(
              "mapType", "Map Selection:",
              c(
                "Soil Data Map"     = "soil",
                "Satellite Map"     = "sat",
                "Organic C Map"     = "oc",
                "pH Map"            = "ph"
              ),
              selected="soil", inline=FALSE
            ),
            tags$hr(),
            checkboxGroupInput(
              "overlaySel", "Overlay Selection:",
              choices = c("Map Unit Overlay"),
              selected = "Map Unit Overlay"
            )
        )
      ),
      
      div(id="mouseBox", textOutput("mouseCoords", inline=TRUE))
  )
)

server <- function(input, output, session) {
  selectedPatch <- reactiveVal(NULL)
  
  # Initial map render
  output$soilMap <- renderLeaflet({
    leaflet(options = leafletOptions(doubleClickZoom = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldTerrain,  group = "Terrain") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      # Soil polygons
      addPolygons(
        data        = soil_polygons,
        layerId     = ~id_,
        group       = "filled",
        fillColor   = ~palMajor(major_taxorder),
        fillOpacity = 0.5,
        color       = "white",
        weight      = 0
      ) %>%
      addPolygons(
        data        = soil_polygons,
        layerId     = ~id_,
        group       = "empty",
        fillOpacity = 0,
        color       = "white",
        weight      = 1,
        highlightOptions = highlightOptions(color = "yellow", weight = 3)
      ) %>%
      
      # Organic C raster
      addRasterImage(
        oc_raster,
        colors  = palOC,
        opacity = 0.7,
        group   = "oc"
      ) %>%
      
      # pH raster
      addRasterImage(
        ph_raster,
        colors  = palPH,
        opacity = 0.7,
        group   = "ph"
      ) %>%
      
      # Initial group visibility
      showGroup(c("Terrain","filled","empty")) %>%
      hideGroup(c("Satellite","oc","ph")) %>%
      
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
  
  proxy <- leafletProxy("soilMap")
  
  # Switch layers based on mapType
  observe({
    mapType <- input$mapType
    overlay <- "Map Unit Overlay" %in% (input$overlaySel %||% "")
    
    if (mapType == "soil") {
      proxy %>%
        showGroup("Terrain") %>%
        showGroup("filled") %>%
        { if (overlay) showGroup(., "empty") else hideGroup(., "empty") } %>%
        hideGroup(c("Satellite","oc","ph"))
      
    } else if (mapType == "sat") {
      proxy %>%
        showGroup("Satellite") %>%
        { if (overlay) showGroup(., "empty") else hideGroup(., "empty") } %>%
        hideGroup(c("Terrain","filled","oc","ph"))
      
    } else if (mapType == "oc") {
      proxy %>%
        showGroup("oc") %>%
        hideGroup(c("Terrain","filled","empty","Satellite","ph"))
      
    } else if (mapType == "ph") {
      proxy %>%
        showGroup("ph") %>%
        hideGroup(c("Terrain","filled","empty","Satellite","oc"))
    }
    
    # Always clear any soil‐popup highlight
    proxy %>% clearGroup("highlight") %>% clearPopups()
  })
  
  # Soil‐polygon click for popups (unchanged)
  observeEvent(input$soilMap_shape_click, {
    clk <- input$soilMap_shape_click; id <- clk$id
    if (is.null(id) || !(id %in% soil_polygons$id_)) return()
    
    feat <- filter(soil_polygons, id_ == id)
    sel  <- comp_info %>% filter(MUKEY == feat$MUKEY) %>% arrange(desc(comppct_r))
    
    popup_html <- tags$div(
      style="max-width:260px;font-size:12px;",
      tags$div(strong("Description:")),
      tags$div(feat$muname, paste0(" (", feat$MUSYM, ")")),
      tags$hr(style="border:none;border-top:2px solid #333;"),
      tags$div(strong("Composition:")),
      tags$ul(
        lapply(seq_len(nrow(sel)), function(i) {
          r <- sel[i,]
          order_lbl <- if (r$taxorder == "Unknown") {
            "None"
          } else {
            tags$a(
              r$taxorder,
              href = paste0("https://www.uidaho.edu/cals/soil-orders/",
                            tolower(gsub("\\s+","-",r$taxorder))),
              target = "_blank"
            )
          }
          if (r$majcompflag == "Yes") {
            tags$li(style="font-weight:bold;", 
                    r$compname, " (", order_lbl, ") — ", paste0(r$comppct_r, "%"))
          } else {
            tags$li(
              r$compname, " (", order_lbl, ") — ", paste0(r$comppct_r, "%")
            )
          }
        })
      ),
      tags$hr(style="border:none;border-top:2px solid #333;"),
      tags$div(strong("Map Unit Data:")),
      tags$p(strong("Sub-Order: "), feat$taxsuborder),
      tags$p(strong("MUKEY: "), feat$MUKEY)
    )
    
    proxy %>%
      clearGroup("highlight") %>%
      clearPopups() %>%
      addPolygons(
        data  = feat,
        color = "yellow", weight = 3,
        fill  = FALSE, group = "highlight"
      ) %>%
      addPopups(
        lng     = clk$lng, lat = clk$lat,
        popup   = as.character(popup_html),
        options = popupOptions(closeButton = TRUE, closeOnClick = FALSE)
      )
  })
  
  # Mouse coords
  output$mouseCoords <- renderText({
    pos <- input$mousePos
    if (is.null(pos)) "Move over map for coordinates"
    else paste0("Lat: ", pos$lat, "  |  Lng: ", pos$lng)
  })
}

shinyApp(ui, server)
