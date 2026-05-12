# Collect underlying support data for beach access analysis:
# urban localities (INEGI), beaches (coastline-derived or existing shapefile),
# OSM paths (pedestrian access), parking, INEGI roads / points of interest, CONANP MPAs.
#
# Run from repo root or any working directory; uses `here::here()` for outputs under
# `_pressures/access/data/`.

# --- Config ---
DOWNLOAD_OSM <- FALSE # set TRUE to re-fetch OSM layers (slow; needs network)
REBUILD_BEACH_SHP <- FALSE # set TRUE to rebuild beach.shp from INEGI coastline

# --- Packages ---
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(osmdata)
  library(here)
})

options(osmdata_timeout = 300)
try(get_overpass_url(), silent = TRUE)
try(set_overpass_url("https://overpass-api.de/api/interpreter"), silent = TRUE)

osm_get <- function(bbox, key, value = NULL,
                    return = c("points", "lines", "polygons", "multilines", "multipolygons"),
                    timeout = 180) {
  if (!requireNamespace("osmdata", quietly = TRUE)) stop("Package 'osmdata' required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' required.")

  q <- osmdata::opq(bbox = bbox, timeout = timeout) |>
    osmdata::add_osm_feature(key = key, value = value)
  res <- osmdata::osmdata_sf(q)

  out <- list(
    points = res$osm_points,
    lines = res$osm_lines,
    polygons = res$osm_polygons,
    multilines = res$osm_multilines,
    multipolygons = res$osm_multipolygons
  )
  requested <- match.arg(return, choices = names(out), several.ok = TRUE)
  final_out <- out[requested]
  Filter(Negate(is.null), final_out)
}

# Gulf of Mexico region bbox (lon/lat), same as original notebook
bb1 <- st_as_sfc(st_bbox(c(xmin = -115.5, ymin = 20.4, xmax = -104.9, ymax = 32), crs = st_crs(4326)))
bb_mex <- st_transform(bb1, crs = 6365)

data_dir <- here("_pressures/access/data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

beach_shp <- file.path(data_dir, "beach.shp")
paths_shp <- file.path(data_dir, "paths_osm_Jan7_2025.shp")
parking_shp <- file.path(data_dir, "parking_osm_Jan7_2025.shp")

if (REBUILD_BEACH_SHP) {
  coastline <- st_read("/home/shares/ohi/OHI_GOC/_raw_data/inegi_coastline/conjunto_de_datos_2024/conjunto_de_datos/lc_rep_mex_v_2024.shp")
  beach_new <- coastline |>
    filter(.data$SUBTIPO %in% c("Playa"), .data$TIPO == "Continental") |>
    st_transform(crs = 4326)
  beach_new$ID_Beach <- seq_len(nrow(beach_new))
  st_write(beach_new, beach_shp, delete_dsn = TRUE)
  message("Wrote ", beach_shp)
}

if (DOWNLOAD_OSM) {

  paths <- osm_get(bb1, key = "highway", value = c("footway", "path", "steps", "pedestrian", "cycleway", "living_street", "sidewalk"), return = "lines")
  paths_l <- paths$lines |> select(any_of(c("osm_id", "name", "bicycle", "foot", "highway", "horse", "motor_vehicle", "motorcar", "surface")))
  st_write(paths_l, paths_shp, delete_dsn = TRUE)
  message("Wrote ", paths_shp)

  parking <- osm_get(bb1, key = "amenity", value = c("parking", "parking_space"), return = "polygons")
  parking_p <- parking$polygons |> select(any_of(c("osm_id")))
  st_write(parking_p, parking_shp, delete_dsn = TRUE)
  message("Wrote ", parking_shp)
}

# --- Read layers used downstream (paths unchanged from notebook) ---
cities_m <- st_read("/home/shares/ohi/OHI_GOC/goal_prep/pressures/population_localities/inegi_boundaries_pop.shp") |>
  filter(.data$AMBITO == "Urbana")

muns <- st_read("/home/shares/ohi/OHI_GOC/_raw_data/inegi_boundaries/889463807469_s/conjunto_de_datos/00mun.shp") |>
  st_transform(crs = 4326)

beach <- st_read(beach_shp)

paths <- st_read(paths_shp)
parking <- st_read(parking_shp) |> mutate(osm_id = row_number())

roads <- st_read("/home/shares/ohi/OHI_GOC/_raw_data/inegi_roads/794551132166_s/conjunto_de_datos/red_vial.shp") |>
  st_transform(crs = 4326)

site <- st_read("/home/shares/ohi/OHI_GOC/_raw_data/inegi_roads/794551132166_s/conjunto_de_datos/sitio_de_interes.shp") |>
  st_transform(crs = 4326)

port_pier <- filter(site, .data$CLASE %in% c("Puerto", "Muelle o Embarcadero"))
bus <- filter(site, .data$CLASE %in% c("Central Camionera"))

mpa <- st_read("/home/shares/ohi/OHI_GOC/_raw_data/CONANP/anpmx/anpmx.shp") |>
  st_make_valid()

message("Support layers loaded: cities_m, muns, beach, paths, parking, roads, site, port_pier, bus, mpa.")
