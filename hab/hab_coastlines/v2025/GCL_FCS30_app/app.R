library(shiny)
library(leaflet)
library(sf)
library(viridis)

gcl_2020_cropped <- st_read(here::here("hab", "hab_coastlines", "v2025","GCL_FCS30_app","www", "gcl_2020_cropped.shp"))

# gcl_2020_cropped$class <- as.character(gcl_2020_cropped$class)

class_descriptions <- c(
  "0" = "Artificial: Human-built structures",
  "1" = "Biogenic: Mangroves, marshes, reefs",
  "2" = "Sandy: Beaches, dunes",
  "3" = "Muddy: Tidal flats, mudflats",
  "4" = "Rocky: Cliffs, bedrock",
  "5" = "Estuary: River-sea transition zones"
)

# user interface
ui <- fluidPage(
  titlePanel("GCL_FCS30 Coastline Map"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "classFilter",
        "Select Coastline Types:",
        # use names(class_descriptions) for values, class_descriptions for labels
        choices = names(class_descriptions),
        selected = names(class_descriptions),
        choiceNames = names(class_descriptions),  
        choiceValues = names(class_descriptions)
      )
    ),
    
    mainPanel(
      leafletOutput("coastlineMap")
    )
  )
)

# define server
server <- function(input, output, session) {
  filteredData <- reactive({
    req(input$classFilter)  # Ensure input$classFilter is not NULL
    gcl_2020_cropped[gcl_2020_cropped$class %in% input$classFilter, ]
  })
  
  # make leaflet
  output$coastlineMap <- renderLeaflet({
    data <- filteredData()
    
    # if no data is selected, display a message instead of the map
    if (nrow(data) == 0) {
      return(leaflet() %>%
               addTiles() %>%
               setView(lng = -112, lat = 28, zoom = 6) %>%
               addPopups(-112, 28, "No data selected."))
    }
    
    viridis_colors <- viridis(6)
    
    # match colors to class meanings
    color_mapping <- c(
      "0" = viridis_colors[1],  # artificial
      "1" = viridis_colors[2],  # biogenic
      "2" = viridis_colors[3],  # sandy
      "3" = viridis_colors[4],  # muddy
      "4" = viridis_colors[5],  # rocky
      "5" = viridis_colors[6]   # estuary
    )
    
    pal <- colorFactor(
      palette = color_mapping,
      domain = data$class
    )
    
    leaflet(data) %>%
      addTiles() %>%
      fitBounds(st_bbox(data)[[1]], st_bbox(data)[[2]], st_bbox(data)[[3]], st_bbox(data)[[4]]) %>%
      addPolylines(
        color = ~pal(class),
        weight = 6,
        opacity = 0.7,
        popup = ~paste(
          "<b>Class:</b>", class, "<br>",
          "<b>Description:</b>", class_descriptions[as.character(class)]
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~class,
        title = "Coastline Type",
        opacity = 1,
        labFormat = labelFormat(
          transform = function(value) {
            class_descriptions[as.character(value)]
          }
        )
      )
  })
}

shinyApp(ui = ui, server = server)


