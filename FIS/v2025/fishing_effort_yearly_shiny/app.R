library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(leaflet)
library(sf)
library(viridisLite)
library(RColorBrewer)
library(bslib)

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

# server portion
server <- function(input, output, session) {
  
  # unique gear types across all years
  all_gear_types <- reactive({
    
    # collect all unique gear types across all years
    all_types <- c()
  
    
    #### added to catch errors in reading the files
    for (year in available_years) {
      
      # load data for each year
      file_path <- file.path("www", "data", sprintf("yearly_summary_sf_%d.shp", year))
      
      year_data <- st_read(file_path)
      
      all_types <- c(all_types, unique(year_data$gear))
      
    }
    
    return(unique(all_types))
  })
  
  # consistent color palette based on all gear types
  gear_pal <- reactive({
    
    gear_types <- all_gear_types()
    
    colorFactor(
      palette = c("#009E73", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"), # colorblind friendly
      domain = gear_types
      # palette = colorRampPalette(brewer.pal(12, "Paired"))(length(gear_types)),
      # domain = gear_types
      # palette = c("firebrick4", "lightskyblue", "gray33", "palevioletred4", "green4", "gold4", "aquamarine3", "blue4"),
      # domain = gear_types
    )
  })
  
  # # testing the direct reading of 2020 data on startup - use observeEvent instead of observe with once=TRUE (done because of errors reading in files previously)
  # observeEvent(1, {
  #   
  #   # this will run once when the app starts
  #   test_year <- 2020
  #   test_path <- sprintf("/home/shares/ohi/OHI_GOC/goal_prep/fis/v2025/int/yearly_summary_cell_gear_sf/yearly_summary_sf_%d.shp", test_year)
  #   
  #   message("Testing direct read of 2020 data on startup")
  #   if (file.exists(test_path)) {
  #     message("File exists: ", test_path)
  #     tryCatch({
  #       test_data <- st_read(test_path, quiet = TRUE)
  #       message("Successfully read 2020 data with columns: ", paste(colnames(test_data), collapse = ", "))
  #     }, error = function(e) {
  #       message("Error reading 2020 data: ", e$message)
  #     })
  #   } else {
  #     message("File does not exist: ", test_path)
  #     
  #     # trying the alternate path from the OG code
  #     alt_path <- sprintf("yearly_summary_sf_%d.shp", test_year)
  #     if (file.exists(alt_path)) {
  #       message("File exists at alternate path: ", alt_path)
  #       tryCatch({
  #         test_data <- st_read(alt_path, quiet = TRUE)
  #         message("Successfully read 2020 data from alternate path with columns: ", paste(colnames(test_data), collapse = ", "))
  #       }, error = function(e) {
  #         message("Error reading 2020 data from alternate path: ", e$message)
  #       })
  #     } else {
  #       message("File does not exist at alternate path either: ", alt_path)
  #     }
  #   }
  # }, once = TRUE)
  
  # load data for each year
  load_yearly_data <- function(year) {
    # tryCatch({
      
    file_path <- file.path("www","data", sprintf("yearly_summary_sf_%d.shp", year))
      
      # # check again that the file exists...
      # if (file.exists(file_path)) {
      #   message(paste("Attempting to load:", file_path))
      # } else {
      #   # Try alternate path
      #   file_path <- sprintf("yearly_summary_sf_%d.shp", year)
      #   if (!file.exists(file_path)) {
      #     message(paste("File not found for year", year))
      #     return(NULL)
      #   }
      #   message(paste("Attempting to load from alternate path:", file_path))
      # }
      
      # read the shapefile directly
      fishing_data <- st_read(file_path)
      
      # show columns found to ensure effort is ttl_fs_ for all years
      message("Columns found: ", paste(colnames(fishing_data), collapse = ", "))
      
      # # error checking on columns
      # if (!"ttl_fs_" %in% colnames(fishing_data)) {
      #   message("Missing required column 'ttl_fs_'")
      #   return(NULL)
      # }
      # 
      # if (!"gear" %in% colnames(fishing_data)) {
      #   message("Missing required column 'gear'")
      #   return(NULL)
      # }
      
      # transform to WGS84 for leaflet
      fishing_data <- st_transform(fishing_data, 4326)
      
      # summarize by gear and geometric lat/long
      fishing_summary <- fishing_data %>%
        group_by(geometry, gear) %>% 
        summarize(
          point_count = n(),
          total_effort = sum(ttl_fs_, na.rm = TRUE),
          .groups = "drop"
        )
      
      # message(paste("Successfully loaded data for year", year, "with", nrow(fishing_summary), "records"))
      return(fishing_summary)
      
    # }, error = function(e) {
    #   message(paste("Error loading data for year", year, ":", e$message))
    #   return(NULL)
    # })
  }
  
  # make the base map
  output$map <- renderLeaflet({
   
     # start with a base map
    leaflet() %>%
      addTiles() %>%
      # set the view to the GoC
      setView(lng = -110, lat = 27, zoom = 5.5)
    
  })
  
  # UPDATE the map when year changes
  observe({
    
    # obtain the selected year from the input slider
    selected_year <- input$selected_year
    
    # load the data for "selected year"
    fishing_data <- load_yearly_data(selected_year)
    
    # double check if data loaded successfully...
    # if (is.null(fishing_data) || nrow(fishing_data) == 0) {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addControl(
    #       html = paste("<strong>No data available for year", selected_year, "</strong><br>Please check console for debug info."),
    #       position = "topright"
    #     )
    #   return()
    # }
    
    # using that consistent color palette for all years and gears
    palette <- gear_pal()
    
    ##### ------- circle size scaling ---------
    max_effort <- max(fishing_data$total_effort, na.rm = TRUE)
    min_effort <- min(fishing_data$total_effort, na.rm = TRUE)
    if (is.na(max_effort) || max_effort == 0) max_effort <- 1
    
    min_radius <- 2
    max_radius <- 15
    
    # calculate representative values for the legend:
    high_value <- max_effort
    medium_value <- (max_effort + min_effort) / 2
    low_value <- min_effort
    
    # calculate corresponding sizes for legend circles:
    high_radius <- max_radius
    medium_radius <- min_radius + sqrt(medium_value/max_effort) * (max_radius - min_radius)
    low_radius <- min_radius
    
    # update the map
    leafletProxy("map", data = fishing_data) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        radius = ~pmin(max_radius, min_radius + sqrt(total_effort/max_effort) * (max_radius - min_radius)), # done to ensure they accurately represent the amount of points with the size.  May need to reevaluate this
        color = ~palette(gear),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste(
          "Gear Type: ", gear, "<br>",
          "Fishing Effort: ", round(total_effort, 3), "<br>",
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
      
      # -------- custom HTML circle size legend (not perfect, low is still a bit off center but this is the best I could get it) --------
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

# run the app!
shinyApp(ui, server)
