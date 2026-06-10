# Build yearly 1 km vessel pressure GeoTIFFs (uncapped + capped) for the GoC OHI workflow.
#
# Pressures:
#   1. vessel_traffic       — sum of capped VMS hours
#   2. benthic_destructive    — bottom-destructive trawl hours (shelf, speed filter)
#   3. ghost_gear             — Gilman ghost risk × capped hours
#
# Capped layers use the 2024 99th-percentile value per pressure (fixed across years).
#
# Outputs:
#   /home/shares/ohi/OHI_GOC/goal_prep/pressures/vessels/{subdir}/{stub}_{year}_1km.tif
#   /home/shares/ohi/OHI_GOC/goal_prep/pressures/vessels/{subdir}/{stub}_{year}_1km_capped.tif
#
# Prerequisites:
#   - data/gear_target_benthic_destructive.csv
#   - data/gear_gilman_crosswalk.csv
#
# Run from repo root:
#   Rscript _pressures/vessels/scripts/build_vessel_pressure_rasters.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(sf)
  library(terra)
})

GOC_STATES <- c(
  "BAJA CALIFORNIA",
  "BAJA CALIFORNIA SUR",
  "JALISCO",
  "NAYARIT",
  "SINALOA",
  "SONORA"
)

VMS_ROOT <- "/home/shares/ohi/OHI_GOC/_raw_data/vms_juan_carlos"
PRESSURE_ROOT <- "/home/shares/ohi/OHI_GOC/goal_prep/pressures/vessels"
CAP_YEAR <- 2024L
YEAR_MIN <- 2007L
YEAR_MAX <- 2025L
HOURS_CAP <- 3

ohi_marine <- rast(here("spatial/ohi_regions/ohi_marine_raster_1km.tif"))
ohi_marine <- ifel(ohi_marine >= 1, 1, NA)

destructive_lookup <- read_csv(
  here("_pressures/vessels/data/gear_target_benthic_destructive.csv"),
  show_col_types = FALSE
) %>%
  select(gear_type, target_species, bottom_destructive)

gilman_by_permit <- read_csv(
  here("_pressures/vessels/data/gear_gilman_crosswalk.csv"),
  show_col_types = FALSE
) %>%
  select(gear_type, target_species, H_gear_ghost)

vessels_registry <- read_csv(
  file.path(VMS_ROOT, "vessel.csv"),
  show_col_types = FALSE
) %>%
  filter(state %in% GOC_STATES)

load_vms_year <- function(year) {
  vms_path <- file.path(VMS_ROOT, "vms", sprintf("vms_%s.csv", year))
  if (!file.exists(vms_path)) {
    stop("Missing VMS file: ", vms_path)
  }

  read_csv(vms_path, show_col_types = FALSE) %>%
    select(-name, -port, -economic_unit) %>%
    left_join(vessels_registry, by = "vessel_rnpa")
}

filter_vessel_traffic <- function(vms_year) {
  vms_year %>%
    filter(!is.na(hours), hours > 0) %>%
    mutate(hours = pmin(hours, HOURS_CAP)) %>%
    filter(!is.na(lat), !is.na(lon))
}

filter_bottom_destructive <- function(vms_year) {
  vms_year %>%
    left_join(destructive_lookup, by = c("gear_type", "target_species")) %>%
    filter(!is.na(bottom_destructive), bottom_destructive == TRUE) %>%
    mutate(water_depth_m = -depth_m) %>%
    filter(!is.na(water_depth_m), water_depth_m <= 200) %>%
    filter(!is.na(implied_speed_knots), implied_speed_knots <= 6) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(hours), hours > 0) %>%
    mutate(hours = pmin(hours, HOURS_CAP))
}

filter_ghost_gear <- function(vms_year) {
  vms_year %>%
    left_join(gilman_by_permit, by = c("gear_type", "target_species")) %>%
    mutate(ghost_gear_risk = H_gear_ghost) %>%
    filter(!is.na(ghost_gear_risk)) %>%
    filter(!is.na(implied_speed_knots), implied_speed_knots <= 6) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(hours), hours > 0) %>%
    mutate(
      hours_capped = pmin(hours, HOURS_CAP),
      ghost_risk_hours = ghost_gear_risk * hours_capped
    )
}

rasterize_to_marine <- function(pings, field, layer_name) {
  pings_sf <- pings %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_transform(crs(ohi_marine))

  out <- rasterize(
    vect(pings_sf),
    y = ohi_marine,
    field = field,
    fun = "sum",
    background = 0
  )
  out <- ifel(is.na(ohi_marine), NA, out)
  names(out) <- layer_name
  out
}

q99_cap <- function(r) {
  as.numeric(quantile(values(r), 0.99, na.rm = TRUE))
}

write_pressure_pair <- function(r, uncapped_path, capped_path, cap_value) {
  dir.create(dirname(uncapped_path), recursive = TRUE, showWarnings = FALSE)
  writeRaster(r, uncapped_path, overwrite = TRUE)
  writeRaster(ifel(r > cap_value, cap_value, r), capped_path, overwrite = TRUE)
}

build_cap_from_year <- function(year, filter_fn, field, layer_name) {
  message("Computing 99th-percentile cap from ", year, " (", layer_name, ")")
  pings <- load_vms_year(year) %>% filter_fn()
  r <- rasterize_to_marine(pings, field, layer_name)
  q99_cap(r)
}

build_vessel_traffic_year <- function(year, cap_value) {
  pings <- load_vms_year(year) %>% filter_vessel_traffic()
  r <- rasterize_to_marine(pings, "hours", "hours_count")
  uncapped <- file.path(PRESSURE_ROOT, "vessel_traffic", sprintf("vms_hours_%s_1km.tif", year))
  capped <- file.path(PRESSURE_ROOT, "vessel_traffic", sprintf("vms_hours_%s_1km_capped.tif", year))
  write_pressure_pair(r, uncapped, capped, cap_value)
}

build_bottom_destructive_year <- function(year, cap_value) {
  pings <- load_vms_year(year) %>% filter_bottom_destructive()
  r <- rasterize_to_marine(pings, "hours", "bottom_trawl_hours")
  uncapped <- file.path(
    PRESSURE_ROOT,
    "benthic_destructive_fishing",
    sprintf("destructive_fishing_hours_%s_1km.tif", year)
  )
  capped <- file.path(
    PRESSURE_ROOT,
    "benthic_destructive_fishing",
    sprintf("destructive_fishing_hours_%s_1km_capped.tif", year)
  )
  write_pressure_pair(r, uncapped, capped, cap_value)
}

build_ghost_gear_year <- function(year, cap_value) {
  pings <- load_vms_year(year) %>% filter_ghost_gear()
  r <- rasterize_to_marine(pings, "ghost_risk_hours", "ghost_risk_hours")
  uncapped <- file.path(PRESSURE_ROOT, "ghost_gear", sprintf("ghost_gear_risk_%s_1km.tif", year))
  capped <- file.path(PRESSURE_ROOT, "ghost_gear", sprintf("ghost_gear_risk_%s_1km_capped.tif", year))
  write_pressure_pair(r, uncapped, capped, cap_value)
}

years <- YEAR_MIN:YEAR_MAX

q99_vessel <- build_cap_from_year(CAP_YEAR, filter_vessel_traffic, "hours", "hours_count")
q99_bottom <- build_cap_from_year(CAP_YEAR, filter_bottom_destructive, "hours", "bottom_trawl_hours")
q99_ghost <- build_cap_from_year(CAP_YEAR, filter_ghost_gear, "ghost_risk_hours", "ghost_risk_hours")

message(
  "Caps from ", CAP_YEAR, ": vessel=", signif(q99_vessel, 4),
  ", bottom=", signif(q99_bottom, 4),
  ", ghost=", signif(q99_ghost, 4)
)

walk(years, function(year) {
  message("\n=== Year ", year, " ===")
  build_vessel_traffic_year(year, q99_vessel)
  build_bottom_destructive_year(year, q99_bottom)
  build_ghost_gear_year(year, q99_ghost)
  message("Finished year ", year)
})

message("\nDone. Wrote ", length(years), " years × 3 pressures (uncapped + capped).")
