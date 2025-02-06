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
# all_shiny_dir <- here::here("_pressures","prs_sst","v2025","sst_all_years_shiny_app")
# rsconnect::deployApp(here::here(all_shiny_dir))


# load in yearly average multilayer rasters
all_years_raster <- terra::rast("data/all_yearly_sst_averages_celsius_goc_eq_area_1980_2023.tif")  # comparison SST raster (2001 - 2023)

# for sliders:
all_years <- seq(1980, 2023)

# defining the user interface (how viewers interact with the app)
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel(" "),
  
  # have each slider for the year below the map
  fluidRow(
    column(12, 
           leafletOutput("map_all", height = "450px"),
           div(
             style = "padding-top: 20px;",  # spacing between map and slider
             sliderInput("years_all", 
                         "Select a year for its average SST",
                         min = min(all_years), 
                         max = max(all_years),
                         value = min(all_years), 
                         step = 1,
                         animate = animationOptions(interval = 1000), 
                         sep = "",
                         width = "100%")
           )
           
    )
  )
)

# defining the server
server <- function(input, output, session) {
  
  # for comparison SST map
  output$map_all <- renderLeaflet({
    selected_layer <- which(all_years == input$years_all)
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        all_years_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(mako(256), values(all_years_raster, na.rm = TRUE), na.color = "transparent")
      ) %>%
      addLegend(
        pal = colorNumeric(rev(mako(256)), values(all_years_raster, na.rm = TRUE), na.color = "transparent"),
        values = values(all_years_raster, na.rm = TRUE),
        title = "SST (C)",
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)) # reverse the label order so it goes positive --> negative change rather than the opposite
      )
  })
  
  observe({
    selected_layer <- which(all_years == input$years_all)
    leafletProxy("map_all") %>%
      clearImages() %>% # clear previous layers before adding new ones
      addRasterImage(
        all_years_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(mako(256), values(all_years_raster, na.rm = TRUE), na.color = "transparent")
      )
  })
  
}

# run the app! 
shinyApp(ui, server)
