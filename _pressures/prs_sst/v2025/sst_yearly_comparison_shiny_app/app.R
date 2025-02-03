library(shiny)
library(leaflet)
library(terra)
library(bslib)
library(viridisLite) # for the mako color theme
library(here)

## for hosting the app
# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='qcz3wo-sophia-lecuona',
#                           token='303AD3AC5CCE039111B28E08270C58A4',
#                           secret='YDs6QWccnNavLMEmuE6w1aL7wEsB41MiKP3Bgwg8')
# 
# shiny_dir <- here::here("_pressures","prs_sst","v2025","sst_yearly_comparison_shiny_app")
# rsconnect::deployApp(here::here(shiny_dir))


# load in yearly average multilayer rasters
historical_raster <- terra::rast("data/historical_yearly_sst_averages_goc_eq_area_1980_2000.tif")  # historical (1980 - 2000) SST raster
current_raster <- terra::rast("data/current_yearly_sst_averages_goc_eq_area_2014_2023.tif")        # current (2014 - 2023) SST raster

# for sliders:
historical_years <- seq(1980, 2000)  # years available for each
current_years <- seq(2014, 2023)

# defining the user interface (how viewers interact with the app)
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Sea Surface Temperature Yearly Averages"),
  
  # show the maps first at the top of the app
  fluidRow(
    column(6, leafletOutput("map_hist", height = "500px")),
    column(6, leafletOutput("map_curr", height = "500px"))
  ),
  
  # have each slider for the year below the map
  fluidRow(
    column(12, 
           div(
             style = "padding: 20px;",  # for spacing to look better
             fluidRow(
               column(6, sliderInput("year_hist", "Select a historical year",
                                     min = min(historical_years), max = max(historical_years),
                                     value = min(historical_years), step = 1,
                                     animate = animationOptions(interval = 1000), sep = "",
                                     width = "100%")),
               column(6, sliderInput("year_curr", "Select a current year",
                                     min = min(current_years), max = max(current_years),
                                     value = min(current_years), step = 1,
                                     animate = animationOptions(interval = 1000), sep = "",
                                     width = "100%"))
               )
             )
           )
    )
)

# defining the server
server <- function(input, output, session) {
  
  # for historical SST map
  output$map_hist <- renderLeaflet({
    selected_layer <- which(historical_years == input$year_hist)
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        historical_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(mako(256), values(historical_raster, na.rm = TRUE), na.color = "transparent"),
        layerId = "historical_layer"
      ) %>%
      addLegend(
        pal = colorNumeric(mako(256), values(historical_raster, na.rm = TRUE), na.color = "transparent"),
        values = values(historical_raster, na.rm = TRUE),
        title = "SST (K)",
        position = "bottomright"
      )
  })
  
  observe({
    selected_layer <- which(historical_years == input$year_hist)
    
    leafletProxy("map_hist") %>%
      addRasterImage(
        historical_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(mako(256), values(historical_raster, na.rm = TRUE), na.color = "transparent"),
        layerId = "historical_layer"
      )
  })
  
  # for current SST map
  output$map_curr <- renderLeaflet({
    selected_layer <- which(current_years == input$year_curr)
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        current_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(mako(256), values(current_raster, na.rm = TRUE), na.color = "transparent"),
        layerId = "current_layer"
      ) %>%
      addLegend(
        pal = colorNumeric(mako(256), values(current_raster, na.rm = TRUE)),
        values = values(current_raster, na.rm = TRUE),
        title = "SST (K)",
        position = "bottomright" # put a legend on both maps for easier viewing
      )
  })
  
  observe({
    selected_layer <- which(current_years == input$year_curr)
    
    leafletProxy("map_curr") %>%
      addRasterImage(
        current_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(mako(256), values(current_raster, na.rm = TRUE), na.color = "transparent"),
        layerId = "current_layer"
      )
  })
}

# run the app! 
shinyApp(ui, server)
