# install.packages(c("shiny", "leaflet", "tmap", "sf", "dplyr", "readr", "ggplot2", "SpatialAcc", "tmaptools"))

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(SpatialAcc)
library(tmap)

ui <- fluidPage(
  titlePanel("Geospatial Analytics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("amenity",
                  "Choose an Amenity:",
                  choices = c("Schools", "MRT", "Mall", "Supermarket", "Hawker"))
    ),
    mainPanel(
      fluidRow(plotOutput("accessibilityPlotHansen", height = "400px")),
      fluidRow(plotOutput("accessibilityPlotKD2SFCA", height = "400px")),
      fluidRow(plotOutput("accessibilityPlotSAM", height = "400px"))
    )
  )
)

server <- function(input, output) {
  
  # Load Data
  hexagon_Hansen <- readRDS("data/rds/hexagon_Hansen.rds")
  hexagon_KD2SFCA <- readRDS("data/rds/hexagon_KD2SFCA.rds")
  hexagon_SAM <- readRDS("data/rds/hexagon_SAM.rds")
  
  schools_sf <- readRDS("data/rds/schools_sf.rds")
  MRT_sf <- readRDS("data/rds/MRT_sf.rds")
  mall_sf <- readRDS("data/rds/mall_sf.rds")
  supermarket_sf <- readRDS("data/rds/supermarket_sf.rds")
  hawker_sf <- readRDS("data/rds/hawker_sf.rds")
  
  mapex <- readRDS("data/rds/mapex.rds")
  
  renderPlotForMethod <- function(method, palette) {
    hex_data <- switch(method,
                       "Hansen" = hexagon_Hansen,
                       "KD2SFCA" = hexagon_KD2SFCA,
                       "SAM" = hexagon_SAM)
    
    amenity_data <- switch(input$amenity,
                           "Schools" = schools_sf,
                           "MRT" = MRT_sf,
                           "Mall" = mall_sf,
                           "Supermarket" = supermarket_sf,
                           "Hawker" = hawker_sf)
    
    amenity_name <- switch(input$amenity,
                           "Schools" = "schools",
                           "MRT" = "MRT",
                           "Mall" = "mall",
                           "Supermarket" = "supermarket",
                           "Hawker" = "hawker")
    
    score_col <- paste0(amenity_name, "_acc", method)
    
    tmap_mode("plot")
    tm <- tm_shape(hex_data, bbox = mapex) +
      tm_fill(col = score_col,
              n = 10,
              style = "quantile",
              palette = palette,
              border.col = "black",
              border.lwd = 1) +
      tm_shape(amenity_data) +
      tm_symbols(size = 0.1) +
      tm_layout(main.title = paste("Accessibility to", input$amenity, ": ", method, "method"),
                main.title.position = "center",
                main.title.size = 1.5,
                legend.outside = FALSE,
                legend.height = 0.45, 
                legend.width = 3.0,
                legend.format = list(digits = 6),
                legend.position = c("right", "top"),
                frame = TRUE) +
      tm_compass(type = "8star", size = 2) +
      tm_scale_bar(width = 0.15) +
      tm_grid(lwd = 0.1, alpha = 0.5)
    
    print(tm)
  }
  
  output$accessibilityPlotHansen <- renderPlot({
    renderPlotForMethod("Hansen", "Reds")
  })
  
  output$accessibilityPlotKD2SFCA <- renderPlot({
    renderPlotForMethod("KD2SFCA", "Greens")
  })
  
  output$accessibilityPlotSAM <- renderPlot({
    renderPlotForMethod("SAM", "Blues")
  })
  
}

shinyApp(ui = ui, server = server)
