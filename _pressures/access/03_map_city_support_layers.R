# Single mapview map for ALL urban areas listed in `data/GofC_urban_areas.csv`:
# union of buffered locality polygons = one study region; roads, paths, parking,
# ports/piers, MPAs, and beaches are clipped to that region.
#
# Output: `_pressures/access/output/gofc_urban_support_layers.html`
#
# Prereq: run `01_collect_access_support_data.R` at least once (or ensure shapefiles exist).

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(here)
  library(units)
  library(mapview)
})

buffer_m <- set_units(20000, "m")

data_dir <- here("_pressures/access/data")
out_dir <- here("_pressures/access/output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

urban_areas_csv <- file.path(data_dir, "GofC_urban_areas.csv")
cities_path <- "/home/shares/ohi/OHI_GOC/goal_prep/pressures/population_localities/inegi_boundaries_pop.shp"
beach_path <- file.path(data_dir, "beach.shp")

paths_path <- file.path(data_dir, "paths_osm_Jan7_2025.shp")
parking_path <- file.path(data_dir, "parking_osm_Jan7_2025.shp")

roads_path <- "/home/shares/ohi/OHI_GOC/_raw_data/inegi_roads/794551132166_s/conjunto_de_datos/red_vial.shp"
site_path <- "/home/shares/ohi/OHI_GOC/_raw_data/inegi_roads/794551132166_s/conjunto_de_datos/sitio_de_interes.shp"

mpa_path <- "/home/shares/ohi/OHI_GOC/_raw_data/CONANP/anpmx/anpmx.shp"

urban_areas <- read_csv(urban_areas_csv, show_col_types = FALSE) |>
  mutate(
    CVE_ENT = as.integer(.data$CVE_ENT),
    CVE_MUN = as.integer(.data$CVE_MUN),
    CVE_LOC = as.integer(.data$CVE_LOC)
  ) |>
  distinct(.data$CVE_ENT, .data$CVE_MUN, .data$CVE_LOC, .keep_all = TRUE)

if (!all(c("city", "CVE_ENT", "CVE_MUN", "CVE_LOC") %in% names(urban_areas))) {
  stop("Expected columns `city`, `CVE_ENT`, `CVE_MUN`, `CVE_LOC` in ", urban_areas_csv)
}

cities_m <- st_read(cities_path) |>
  filter(.data$AMBITO == "Urbana") |>
  st_make_valid()

for (nm in c("CVE_ENT", "CVE_MUN", "CVE_LOC")) {
  if (nm %in% names(cities_m)) cities_m[[nm]] <- suppressWarnings(as.integer(cities_m[[nm]]))
}

city_polys <- cities_m |>
  inner_join(urban_areas, by = c("CVE_ENT", "CVE_MUN", "CVE_LOC")) |>
  st_make_valid()

if (nrow(city_polys) == 0) {
  stop("No urban polygons matched `GofC_urban_areas.csv` keys. Check CVE_ENT/CVE_MUN/CVE_LOC.")
}

beach <- st_read(beach_path) |> st_make_valid()
paths <- st_read(paths_path) |> st_make_valid()
parking <- st_read(parking_path) |> st_make_valid()

roads <- st_read(roads_path) |> st_transform(st_crs(beach))
site <- st_read(site_path) |> st_transform(st_crs(beach))
port_pier <- site |> filter(.data$CLASE %in% c("Puerto", "Muelle o Embarcadero"))
bus <- site |> filter(.data$CLASE %in% c("Central Camionera"))

mpa <- st_read(mpa_path) |> st_make_valid() |> st_transform(st_crs(beach))

# One buffer per included locality row, then dissolve into a single study region
urban_bufs <- city_polys |>
  st_transform(st_crs(cities_m)) |>
  st_buffer(dist = as.numeric(buffer_m)) |>
  st_transform(st_crs(beach))

buf_union <- st_union(urban_bufs)
buf_sf <- st_as_sf(buf_union) |> st_set_crs(st_crs(beach))

# Clip support layers to the combined region (same CRS as beach)
beach_scope <- suppressWarnings(st_intersection(beach, buf_sf))
paths_scope <- suppressWarnings(st_intersection(paths, buf_sf))
parking_scope <- suppressWarnings(st_intersection(parking, buf_sf))
roads_scope <- suppressWarnings(st_intersection(roads, buf_sf))
port_pier_scope <- suppressWarnings(st_intersection(port_pier, buf_sf))
bus_scope <- suppressWarnings(st_intersection(bus, buf_sf))
mpa_scope <- suppressWarnings(st_intersection(mpa, buf_sf))

city_polys_b <- city_polys |> st_transform(st_crs(beach))
urban_bufs_b <- urban_bufs

m <-
  mapview(buf_sf, color = "magenta", alpha.regions = 0.08, layer.name = "Study region (all urban buffers, union)", legend = FALSE) +
  mapview(city_polys_b, zcol = "city", alpha.regions = 0.25, layer.name = "Urban locality footprints", legend = FALSE) +
  mapview(mpa_scope, color = "olivedrab3", alpha.regions = 0.12, layer.name = "MPAs (clipped)", legend = FALSE) +
  mapview(roads_scope, color = "orange", layer.name = "INEGI roads (clipped)", legend = FALSE) +
  mapview(paths_scope, color = "forestgreen", layer.name = "OSM paths (clipped)", legend = FALSE) +
  mapview(parking_scope, col.regions = "purple", color = "purple", layer.name = "OSM parking (clipped)", legend = FALSE) +
  mapview(port_pier_scope, color = "deepskyblue3", layer.name = "Ports/piers (clipped)", legend = FALSE) +
  mapview(bus_scope, color = "brown", layer.name = "Bus terminals (clipped)", legend = FALSE) +
  mapview(beach_scope, lwd = 4, color = "gold", layer.name = "Beaches (clipped)", legend = FALSE)

m

out_path <- file.path(out_dir, "gofc_urban_support_layers.html")
mapview::mapviewOptions(fgb = FALSE)
mapview::mapshot(m, file = out_path)
message("Wrote ", out_path)
