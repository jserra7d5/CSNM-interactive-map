# app.R

# ——————————————————————————————————————————
# Libraries
library(shiny)
library(sf)
library(leaflet)
library(htmlwidgets)
library(magrittr)
library(dplyr)
library(RColorBrewer)
# ——————————————————————————————————————————

# 1) Read CSV lookup table for mapunit names
mapunit_table <- read.csv("Mapunit_OR_table.csv", stringsAsFactors = FALSE) %>%
  rename(MUKEY = mukey, muname = muname) %>%
  mutate(MUKEY = as.character(MUKEY))

# 2) Load & split MULTI→single POLYGONs, join on MUKEY
raw_polygons <- st_read("CSNM_Polygons_with_Data.geojson", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_cast("POLYGON") %>%
  mutate(
    MUKEY     = as.character(MUKEY),
    taxorder  = ifelse(is.na(taxorder) | taxorder == "", "Unknown", taxorder)
  ) %>%
  left_join(mapunit_table, by = "MUKEY")

# simplify geometry to speed redraw
soil_polygons <- st_simplify(raw_polygons, dTolerance = 0.0001)
soil_polygons$id_ <- seq_len(nrow(soil_polygons))

# 3) Palette for soil orders
orders <- sort(unique(soil_polygons$taxorder))
pal    <- colorFactor(brewer.pal(max(length(orders), 3), "Set3"), domain = orders)

ui <- fluidPage(
  titlePanel("CSNM Interactive Map"),
  tags$head(tags$style(HTML("
    /* larger ▲/▼ arrows */
    .info-panel summary::marker { content: none; }
    .info-panel summary::before { content: '\\25B2'; font-size:1.2em; margin-right:0.5em; }
    .info-panel details[open] summary::before { content: '\\25BC'; font-size:1.2em; }
    .info-panel summary { cursor: pointer; }

    /* coords box */
    #mouseBox {
      position: absolute; bottom:30px; right:10px;
      background: rgba(255,255,255,0.75);
      padding:6px 8px; border-radius:4px; font-size:12px;
      z-index:1000;
    }

    /* dividers */
    .info-panel hr {
      border:none; border-top:2px solid #444; margin:12px 0;
    }
  "))),
  div(style="position:relative; height:80vh; margin-bottom:20px;",
      leafletOutput("soilMap", width="100%", height="100%"),
      absolutePanel(
        id="overlay", top=10, right=10, width=300, draggable=FALSE,
        style="background:rgba(255,255,255,0.75); padding:10px; border-radius:8px; z-index:1000;",
        div(class="info-panel",
            tags$div(style="text-align:center; margin-bottom:10px; font-size:16px;",
                     textOutput("mapUnitFullName", inline=TRUE)),
            tags$hr(),
            tags$details(id="compPanel",
                         tags$summary("Map Unit Composition"),
                         uiOutput("components"),
                         uiOutput("orderLine")
            ),
            tags$hr(),
            tags$details(id="dataPanel",
                         tags$summary("Map Unit Data"),
                         tags$p(strong("Sub-Order: "), textOutput("taxsuborder", inline=TRUE)),
                         tags$p(strong("Map Unit Key: "), textOutput("MUKEY", inline=TRUE))
            ),
            tags$hr(),
            radioButtons("mapType","Background view:",
                         c("Soil Data Map"="soil","Satellite Map"="sat"),
                         selected="soil", inline=TRUE)
        )
      ),
      div(id="mouseBox", textOutput("mouseCoords", inline=TRUE))
  )
)

server <- function(input, output, session) {
  selectedPatch <- reactiveVal(NULL)
  
  # initial render: two pre-added polygon layers (filled & empty)
  output$soilMap <- renderLeaflet({
    leaflet(options = leafletOptions(doubleClickZoom = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldTerrain, group="Terrain") %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
      # filled layer
      addPolygons(
        data             = soil_polygons,
        layerId          = ~id_,
        group            = "filled",
        fillColor        = ~pal(taxorder),
        fillOpacity      = 0.5,
        color            = "white",
        weight           = 1,
        highlightOptions = highlightOptions(color="yellow", weight=3)
      ) %>%
      # empty (transparent) layer
      addPolygons(
        data             = soil_polygons,
        layerId          = ~id_,
        group            = "empty",
        fillOpacity      = 0,
        color            = "white",
        weight           = 1,
        highlightOptions = highlightOptions(color="yellow", weight=3)
      ) %>%
      showGroup(c("Terrain","filled")) %>%
      hideGroup(c("Satellite","empty")) %>%
      onRender("
        function(el,x){
          this.on('mousemove',function(e){
            Shiny.setInputValue('mousePos',{
              lat: e.latlng.lat.toFixed(5),
              lng: e.latlng.lng.toFixed(5)
            },{priority:'event'});
          });
        }
      ")
  })
  
  proxy <- leafletProxy("soilMap")
  
  # toggle background + show only one polygon layer
  observeEvent(input$mapType, {
    if (input$mapType=="soil") {
      proxy %>%
        showGroup(c("Terrain","filled")) %>%
        hideGroup(c("Satellite","empty"))
    } else {
      proxy %>%
        showGroup(c("Satellite","empty")) %>%
        hideGroup(c("Terrain","filled"))
    }
  })
  
  # click highlight + store selected
  observeEvent(input$soilMap_shape_click, {
    clk <- input$soilMap_shape_click; id <- clk$id
    if (is.null(id) || !(id %in% soil_polygons$id_)) {
      proxy %>% clearGroup("highlight"); selectedPatch(NULL); return()
    }
    patch <- soil_polygons[soil_polygons$id_==as.integer(id),]
    proxy %>%
      clearGroup("highlight") %>%
      addPolygons(
        data   = patch,
        color  = "yellow",
        weight = 3,
        fill   = FALSE,
        group  = "highlight"
      )
    selectedPatch(patch)
  })
  
  # components list + percentages
  output$components <- renderUI({
    p <- selectedPatch()
    if (is.null(p)) return(NULL)
    comps <- soil_polygons %>%
      st_drop_geometry() %>%
      filter(MUKEY==p$MUKEY) %>%
      distinct(compname, comppct_r, majcompflag) %>%
      arrange(desc(comppct_r))
    if (nrow(comps)==0) return(tags$p("No component data available."))
    tags$ul(
      lapply(seq_len(nrow(comps)), function(i){
        r <- comps[i,]
        lbl <- paste0(r$comppct_r,"% ",r$compname,
                      ifelse(r$majcompflag=="Yes"," (major)",""))
        tags$li(lbl)
      })
    )
  })
  
  # other info-panel outputs
  output$mapUnitFullName <- renderText({
    p <- selectedPatch()
    if (is.null(p)) "Select a Map Unit"
    else paste0(p$muname," (",p$MUSYM,")")
  })
  output$orderLine <- renderUI({
    p <- selectedPatch()
    if (is.null(p) || p$taxorder=="") return(tags$p(strong("Order: "), ""))
    slug <- tolower(gsub("\\s+","-",p$taxorder))
    url  <- paste0("https://www.uidaho.edu/cals/soil-orders/",slug)
    tags$p(strong("Order: "),
           tags$a(p$taxorder,href=url,target="_blank",
                  title="Click for more soil-order info"))
  })
  output$taxsuborder <- renderText({ p<-selectedPatch(); if(is.null(p)) "" else p$taxsuborder })
  output$MUKEY       <- renderText({ p<-selectedPatch(); if(is.null(p)) "" else p$MUKEY })
  
  # mouse coords
  output$mouseCoords <- renderText({
    pos <- input$mousePos
    if (is.null(pos)) "Move over map for coordinates"
    else paste0("Lat: ",pos$lat,"  |  Lng: ",pos$lng)
  })
}

shinyApp(ui, server)
