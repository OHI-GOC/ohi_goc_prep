# Join expert opinion CSV to beach geometries and save an interactive Leaflet map.
#
# Prerequisites: beach.shp and a filled expert CSV (e.g. LaPaz.csv).

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(here)
  library(leaflet)
  library(htmltools)
  library(htmlwidgets)
})

data_dir <- here("_pressures/access/data")
beach_path <- file.path(data_dir, "beach.shp")
expert_csv <- file.path(data_dir, "LaPaz.csv")
output_html <- file.path(data_dir, "lapaz_beach_access_map.html")

beach <- st_read(beach_path)
expert <- read_csv(expert_csv, show_col_types = FALSE)

missing_ids <- setdiff(expert$ID_Beach, beach$ID_Beach)
if (length(missing_ids)) {
  warning("Expert CSV has ID_Beach values not in beach layer: ", paste(missing_ids, collapse = ", "))
}

beach_sub <- beach |> filter(.data$ID_Beach %in% expert$ID_Beach)
map_data <- left_join(beach_sub, expert, by = "ID_Beach")

map_data$length_km <- as.numeric(st_length(map_data)) / 1000

pal <- colorNumeric(
  palette = colorRampPalette(c("red", "orange", "yellow", "green", "cyan", "blue"))(256),
  domain = c(0, 10),
  na.color = "gray80"
)

map_data$popup_text <- sprintf(
  paste0(
    "<b>%s</b><br/>",
    "<b>Equitable access score:</b> %s<br/>",
    "<b>Category:</b> %s<br/>",
    "<b>Urban/rural:</b> %s<br/>",
    "<b>Largely gated/blocked:</b> %s<br/>",
    "<b>Public roads within 1 km:</b> %s<br/>",
    "<b>Vehicle:</b> %s<br/>",
    "<b>Parking:</b> %s<br/>",
    "<b>Number of paths:</b> %s<br/>",
    "<b>Protected/fee:</b> %s<br/>",
    "<b>Notes:</b> %s"
  ),
  ifelse(is.na(map_data$Name), "", map_data$Name),
  ifelse(is.na(map_data$equitable_access_score), "", as.character(map_data$equitable_access_score)),
  ifelse(is.na(map_data$Category), "", map_data$Category),
  ifelse(is.na(map_data$urban_rural), "", map_data$urban_rural),
  ifelse(is.na(map_data$largely_gated_blocked), "", map_data$largely_gated_blocked),
  ifelse(is.na(map_data$public_roads_within_1km), "", map_data$public_roads_within_1km),
  ifelse(is.na(map_data$vehicle), "", map_data$vehicle),
  ifelse(is.na(map_data$parking), "", map_data$parking),
  ifelse(is.na(map_data$number_paths), "", as.character(map_data$number_paths)),
  ifelse(is.na(map_data$protected_fee), "", map_data$protected_fee),
  ifelse(is.na(map_data$notes), "", map_data$notes)
)

m <- leaflet(map_data) |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addPolylines(
    color = ~ pal(equitable_access_score),
    weight = 6,
    popup = ~ popup_text
  )

saveWidget(m, output_html, selfcontained = TRUE)
message("Wrote ", output_html)

