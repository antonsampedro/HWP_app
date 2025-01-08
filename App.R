library(shiny)
library(leaflet)
library(raster)
library(dismo)
library(gstat)
library(sp)
library(ENMeval)
library(geodata)
library(spatialEco)
library(sf)
library(ggplot2)
library(readxl)
library(spThin)
library(rJava)
library(doParallel)
library(dplyr)
library(raster)
library(terra)

asia <- shapefile("asia.shp")
study.area <- shapefile("study.area.maxent3.shp")
# Preparación de datos fuera de Shiny
asia_sf <- st_as_sf(asia) %>%
  st_transform(crs = 3857)
study.area_sf <- st_as_sf(study.area) %>%
  st_transform(crs = 3857)
ext2 <- st_bbox(study.area_sf)
library(basemaps)
bm <- basemap_terra(ext = ext2, map_service = "esri", map_type = "world_physical_map") %>%
  as("Raster")



load_data <- function() {

  pred.hw <- raster("pred.hw.tif")
  pred.hw.ssp1.2021.2040 <- raster("pred.hw.ssp1.2021.2040.tif")
  pred.hw.ssp1.2041.2060 <- raster("pred.hw.ssp1.2041.2060.tif")
  pred.hw.ssp1.2061.2080 <- raster("pred.hw.ssp1.2061.2080.tif")
  pred.hw.ssp2.2021.2040 <- raster("pred.hw.ssp2.2021.2040.tif")
  pred.hw.ssp2.2041.2060 <- raster("pred.hw.ssp2.2041.2060.tif")
  pred.hw.ssp2.2061.2080 <- raster("pred.hw.ssp2.2061.2080.tif")
  pred.hw.ssp4.2021.2040 <- raster("pred.hw.ssp4.2021.2040.tif")
  pred.hw.ssp4.2041.2060 <- raster("pred.hw.ssp4.2041.2060.tif")
  pred.hw.ssp4.2061.2080 <- raster("pred.hw.ssp4.2061.2080.tif")
  return(list(
    pred.hw, pred.hw.ssp1.2021.2040, pred.hw.ssp1.2041.2060, pred.hw.ssp1.2061.2080,
    pred.hw.ssp2.2021.2040, pred.hw.ssp2.2041.2060, pred.hw.ssp2.2061.2080,
    pred.hw.ssp4.2021.2040, pred.hw.ssp4.2041.2060, pred.hw.ssp4.2061.2080
  ))
}

# UI de Shiny
ui <- fluidPage(
  titlePanel("Potential habitat suitability for Himalayan wolf"),
  
  fluidRow(
    column(6), # Espaciado vacío para alinear los selectores a la derecha
    column(6,
           fluidRow(
             column(6,
                    h4("Global warming scenario"),
                    selectInput(
                      inputId = "scenario",
                      label = NULL, # Ocultar etiqueta
                      choices = c("SSP1-RCP 2.6", "SSP2-RCP 4.5", "SSP4-RCP 8.5"),
                      selected = "SSP2-RCP 2.6"
                    )
             ),
             column(6,
                    h4("Prediction time range        "),
                    selectInput(
                      inputId = "time_range",
                      label = NULL, # Ocultar etiqueta
                      choices = c("2021-2040", "2041-2060", "2061-2080"),
                      selected = "2021-2040"
                    )
             )
           )
    )
  ),
  
  fluidRow(
    column(6, leafletOutput(outputId = "map_base", height = "500px")),
    column(6, leafletOutput(outputId = "map_selected", height = "500px"))
  )
)

# Servidor de Shiny
server <- function(input, output, session) {
  
  # Cargar los datos en un reactiveValue, para que no se recarguen
  values <- reactiveValues()
  
  # Cargar los datos solo si no están cargados ya
  observe({
    if (is.null(values$rasters)) {
      values$rasters <- load_data()
    }
  })
  
  # Mapeo de las capas
  layer_mapping <- list(
    "SSP1-RCP 2.6" = list(
      "2021-2040" = values$rasters[[2]],
      "2041-2060" = values$rasters[[3]],
      "2061-2080" = values$rasters[[4]]
    ),
    "SSP2-RCP 4.5" = list(
      "2021-2040" = values$rasters[[5]],
      "2041-2060" = values$rasters[[6]],
      "2061-2080" = values$rasters[[7]]
    ),
    "SSP4-RCP 8.5" = list(
      "2021-2040" = values$rasters[[8]],
      "2041-2060" = values$rasters[[9]],
      "2061-2080" = values$rasters[[10]]
    )
  )
  
  # Mapa base
  output$map_base <- renderLeaflet({
    pred.base <- aggregate(values$rasters[[1]], fact = 1) # Usar el primer raster precargado
    pred.base <- projectRaster(pred.base, crs = 4326)
    
    values_pred_base <- as.numeric(values(pred.base))
    
    colors_base <- colorNumeric(
      palette = c("#e76409", "#e0d009", "#27a105"),
      domain = c(0.1, 1),  # Gradación continua entre estos valores
      na.color = "transparent",
      reverse = FALSE
    )
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(pred.base, colors = colors_base, opacity = 0.7) %>%
      addLegend(position = "bottomright", pal = colors_base, values = c(0, 1), opacity = 1, title = "Habitat suitability") %>%
      addPolygons(data = st_transform(study.area_sf, crs = 4326), color = "transparent", fillOpacity = 0.2, weight = 1) %>%
      addPolygons(data = st_transform(asia_sf, crs = 4326), color = "black", fillOpacity = 0, weight = 1)
  })
  
  # Mapa con los datos seleccionados
  output$map_selected <- renderLeaflet({
    pred.selected <- layer_mapping[[input$scenario]][[input$time_range]]
    pred.reduced <- aggregate(pred.selected, fact = 1)
    pred.reduced <- projectRaster(pred.reduced, crs = 4326)
    
    values_pred_reduced <- as.numeric(values(pred.reduced))
    
    colors_pred <- colorNumeric(
      palette = c("#e76409", "#e0d009", "#27a105"),
      domain = c(0.1, 1),  # Gradación continua entre estos valores
      na.color = "transparent",
      reverse = FALSE
    )
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(pred.reduced, colors = colors_pred, opacity = 0.7) %>%
      addLegend(position = "bottomright", pal = colors_pred, values = c(0, 1), opacity = 1, title = "Habitat Suitability") %>%
      addPolygons(data = st_transform(study.area_sf, crs = 4326), color = "transparent", fillOpacity = 0.2, weight = 1) %>%
      addPolygons(data = st_transform(asia_sf, crs = 4326), color = "black", fillOpacity = 0, weight = 1)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
