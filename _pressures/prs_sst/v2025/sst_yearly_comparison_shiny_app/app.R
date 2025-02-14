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
comparison_raster <- terra::rast("data/comparison_current_hist_baseline_celsius_goc_eq_area_2001_2023.tif")  # comparison SST raster (2001 - 2023)

# for sliders:
comparison_years <- seq(2001, 2023)

# defining the user interface (how viewers interact with the app)
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel(" "),
  
  # have each slider for the year below the map
  fluidRow(
    column(12, 
           leafletOutput("map_comp", height = "450px"),
           div(
             style = "padding-top: 20px;",  # spacing between map and slider
             sliderInput("years_comp", 
                         "Select a year for its change in SST compared to the historical baseline",
                         min = min(comparison_years), 
                         max = max(comparison_years),
                         value = min(comparison_years), 
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
  output$map_comp <- renderLeaflet({
    selected_layer <- which(comparison_years == input$years_comp)
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        comparison_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(inferno(256), values(comparison_raster, na.rm = TRUE), na.color = "transparent")
      ) %>%
      addLegend(
        pal = colorNumeric(rev(inferno(256)), values(comparison_raster, na.rm = TRUE), na.color = "transparent"),
        values = values(comparison_raster, na.rm = TRUE),
        title = "SST (C)",
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)) # reverse the label order so it goes positive --> negative change rather than the opposite
      )
  })
  
  observe({
    selected_layer <- which(comparison_years == input$years_comp)
    leafletProxy("map_comp") %>%
      clearImages() %>% # clear previous layers before adding new ones
      addRasterImage(
        comparison_raster[[selected_layer]],
        opacity = 0.8,
        colors = colorNumeric(inferno(256), values(comparison_raster, na.rm = TRUE), na.color = "transparent")
      )
  })
  
}

# run the app! 
shinyApp(ui, server)
