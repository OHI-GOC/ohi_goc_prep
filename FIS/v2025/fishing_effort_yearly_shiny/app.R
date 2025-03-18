library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(viridisLite)
library(RColorBrewer)
library(bslib)

## for hosting the app
# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='qcz3wo-sophia-lecuona',
#                           token='303AD3AC5CCE039111B28E08270C58A4',
#                           secret='YDs6QWccnNavLMEmuE6w1aL7wEsB41MiKP3Bgwg8')
# shiny_dir <- here::here("FIS","v2025","fishing_effort_yearly_shiny")
# rsconnect::deployApp(here::here(shiny_dir))

# define the years for the sliders
available_years <- 2012:2020

# define the user interface
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  # titlePanel("Fishing Effort in the Gulf of California"),
  
  fluidRow(
    column(12, 
           leafletOutput("map", height = "700px"),
           div(
             style = "padding-top: 20px;",  # spacing between map and slider
             sliderInput("selected_year", 
                         "Select year to view fishing effort data by gear type",
                         min = min(available_years), 
                         max = max(available_years),
                         value = 2020, # start with the most recent year because 2012 does not have much data
                         step = 1,
                         animate = animationOptions(interval = 1000), 
                         sep = "",
                         width = "100%")
           )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Get all unique gear types across all years at app initialization
  all_gear_types <- reactive({
    # This will collect all unique gear types across all years
    all_types <- c()
    
    for (year in available_years) {
      # Try to load data for each year
      tryCatch({
        file_path <- sprintf("/home/shares/ohi/OHI_GOC/goal_prep/fis/v2025/int/yearly_summary_cell_gear_sf/yearly_summary_sf_%d.shp", year)
        
        if (!file.exists(file_path)) {
          file_path <- sprintf("yearly_summary_sf_%d.shp", year)
          if (!file.exists(file_path)) next
        }
        
        year_data <- st_read(file_path, quiet = TRUE)
        if ("gear" %in% colnames(year_data)) {
          all_types <- c(all_types, unique(year_data$gear))
        }
      }, error = function(e) {
        # Just continue if there's an error
      })
    }
    
    return(unique(all_types))
  })
  
  # Create a consistent color palette based on all gear types
  gear_pal <- reactive({
    gear_types <- all_gear_types()
    
    # Create a fixed color palette for all gear types
    colorFactor(
      palette = c("#009E73", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"), # colorblind friendly
      domain = gear_types
      # palette = colorRampPalette(brewer.pal(12, "Paired"))(length(gear_types)),
      # domain = gear_types
      # palette = c("firebrick4", "lightskyblue", "gray33", "palevioletred4", "green4", "gold4", "aquamarine3", "blue4"),
      # domain = gear_types
    )
  })
  
  # Test direct reading of 2020 data on startup - use observeEvent instead of observe with once=TRUE
  observeEvent(1, {
    # This will run once when the app starts
    test_year <- 2020
    test_path <- sprintf("/home/shares/ohi/OHI_GOC/goal_prep/fis/v2025/int/yearly_summary_cell_gear_sf/yearly_summary_sf_%d.shp", test_year)
    
    message("Testing direct read of 2020 data on startup")
    if (file.exists(test_path)) {
      message("File exists: ", test_path)
      tryCatch({
        test_data <- st_read(test_path, quiet = TRUE)
        message("Successfully read 2020 data with columns: ", paste(colnames(test_data), collapse = ", "))
      }, error = function(e) {
        message("Error reading 2020 data: ", e$message)
      })
    } else {
      message("File does not exist: ", test_path)
      
      # Try the alternate path from your original code
      alt_path <- sprintf("yearly_summary_sf_%d.shp", test_year)
      if (file.exists(alt_path)) {
        message("File exists at alternate path: ", alt_path)
        tryCatch({
          test_data <- st_read(alt_path, quiet = TRUE)
          message("Successfully read 2020 data from alternate path with columns: ", paste(colnames(test_data), collapse = ", "))
        }, error = function(e) {
          message("Error reading 2020 data from alternate path: ", e$message)
        })
      } else {
        message("File does not exist at alternate path either: ", alt_path)
      }
    }
  }, once = TRUE)
  
  # Function to load data for a specific year - greatly simplified
  load_yearly_data <- function(year) {
    tryCatch({
      # Create the file path using the provided year parameter
      file_path <- sprintf("/home/shares/ohi/OHI_GOC/goal_prep/fis/v2025/int/yearly_summary_cell_gear_sf/yearly_summary_sf_%d.shp", year)
      
      # Check if file exists at primary path
      if (file.exists(file_path)) {
        message(paste("Attempting to load:", file_path))
      } else {
        # Try alternate path
        file_path <- sprintf("yearly_summary_sf_%d.shp", year)
        if (!file.exists(file_path)) {
          message(paste("File not found for year", year))
          return(NULL)
        }
        message(paste("Attempting to load from alternate path:", file_path))
      }
      
      # Simply try to read the shapefile directly
      fishing_data <- st_read(file_path, quiet = TRUE)
      
      # Display columns found
      message("Columns found: ", paste(colnames(fishing_data), collapse = ", "))
      
      # Basic error checking on columns
      if (!"ttl_fs_" %in% colnames(fishing_data)) {
        message("Missing required column 'ttl_fs_'")
        return(NULL)
      }
      
      if (!"gear" %in% colnames(fishing_data)) {
        message("Missing required column 'gear'")
        return(NULL)
      }
      
      # Transform to WGS84 - simplify this step
      fishing_data <- st_transform(fishing_data, 4326)
      
      # Create summary
      fishing_summary <- fishing_data %>%
        group_by(geometry, gear) %>% 
        summarize(
          point_count = n(),
          total_effort = sum(ttl_fs_, na.rm = TRUE),
          .groups = "drop"
        )
      
      message(paste("Successfully loaded data for year", year, "with", nrow(fishing_summary), "records"))
      return(fishing_summary)
      
    }, error = function(e) {
      message(paste("Error loading data for year", year, ":", e$message))
      return(NULL)
    })
  }
  
  # Create base map only once
  output$map <- renderLeaflet({
    # Start with a base map
    leaflet() %>%
      addTiles() %>%
      # Set the view to focus on the Gulf of California
      setView(lng = -110, lat = 27, zoom = 5.5)
  })
  
  # Update map when year changes
  observe({
    # Get the selected year from the input slider
    selected_year <- input$selected_year
    
    # Load data for selected year
    fishing_data <- load_yearly_data(selected_year)
    
    # Check if data loaded successfully
    if (is.null(fishing_data) || nrow(fishing_data) == 0) {
      # Show message on map if no data
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearControls() %>%
        addControl(
          html = paste("<strong>No data available for year", selected_year, "</strong><br>Please check console for debug info."),
          position = "topright"
        )
      return()
    }
    
    # consistent color palette for all years and gears
    palette <- gear_pal()
    
    # circle size scaling
    max_effort <- max(fishing_data$total_effort, na.rm = TRUE)
    min_effort <- min(fishing_data$total_effort, na.rm = TRUE)
    if (is.na(max_effort) || max_effort == 0) max_effort <- 1
    
    min_radius <- 2
    max_radius <- 15
    
    # Calculate representative values for the legend:
    high_value <- max_effort
    medium_value <- (max_effort + min_effort) / 2
    low_value <- min_effort
    
    # Calculate corresponding sizes for legend circles:
    high_radius <- max_radius
    medium_radius <- min_radius + sqrt(medium_value/max_effort) * (max_radius - min_radius)
    low_radius <- min_radius
    
    # update the map
    leafletProxy("map", data = fishing_data) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        radius = ~pmin(max_radius, min_radius + sqrt(total_effort/max_effort) * (max_radius - min_radius)),
        color = ~palette(gear),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste(
          "Gear Type: ", gear, "<br>",
          "Fishing Effort: ", round(total_effort, 2), "<br>",
          "Points: ", point_count
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palette,
        values = all_gear_types(),  # use all gear types for the legend for consistency
        title = paste("Gear Type"),
        opacity = 0.7
      ) %>%
      # Add custom HTML circle size legend
      addControl(
        html = HTML(
          paste0(
            '<div style="background-color: white; padding: 10px; border-radius: 5px; opacity: 0.8;">',
            '<p style="margin-bottom: 8px;"><strong>Fishing Effort</strong></p>',
            
            # All circles will use the same fixed width container
            '<div style="display: flex; align-items: center; margin-bottom: 8px;">',
            '<div style="width: ', high_radius*2, 'px; text-align: center;">',
            '<svg width="', high_radius*2, '" height="', high_radius*2, '">',
            '<circle cx="', high_radius, '" cy="', high_radius, '" r="', high_radius, '" fill="gray" opacity="0.7"/>',
            '</svg>',
            '</div>',
            '<span style="margin-left: 8px;">High (', round(high_value, 1), ')</span>',
            '</div>',
            
            '<div style="display: flex; align-items: center; margin-bottom: 8px;">',
            '<div style="width: ', high_radius*2, 'px; text-align: center;">',
            '<svg width="', medium_radius*2, '" height="', medium_radius*2, '" style="margin-left: ', (high_radius-medium_radius), 'px;">',
            '<circle cx="', medium_radius, '" cy="', medium_radius, '" r="', medium_radius, '" fill="gray" opacity="0.7"/>',
            '</svg>',
            '</div>',
            '<span style="margin-left: 8px;">Medium (', round(medium_value, 1), ')</span>',
            '</div>',
            
            '<div style="display: flex; align-items: center;">',
            '<div style="width: ', high_radius*2, 'px; text-align: center;">',
            '<svg width="', low_radius*2, '" height="', low_radius*2, '" style="margin-left: ', (high_radius-low_radius), 'px;">',
            '<circle cx="', low_radius, '" cy="', low_radius, '" r="', low_radius, '" fill="gray" opacity="0.7"/>',
            '</svg>',
            '</div>',
            '<span style="margin-left: 8px;">Low (', round(low_value, 1), ')</span>',
            '</div>',
            
            '</div>'
          )
        ),
        position = "bottomleft"
      )
  })
}

# Run the app
shinyApp(ui, server)
