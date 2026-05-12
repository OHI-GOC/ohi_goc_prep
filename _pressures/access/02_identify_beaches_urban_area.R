# Iterate over `data/GofC_urban_areas.csv` and, for each `city`, buffer the included
# urban locality polygons and intersect beaches. Writes one CSV per city to
# `_pressures/access/data/city_csv/`.
#
# Notes:
# - Cities can have multiple polygons (multiple rows in `GofC_urban_areas.csv`).
# - No excluded areas; only included CVE_ENT/CVE_MUN/CVE_LOC are used.
# - Output includes beach length in km (length of the intersected segment).

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(here)
  library(units)
})

# --- Parameters ---
buffer_m <- set_units(20000, "m")

data_dir <- here("_pressures/access/data")
beach_path <- file.path(data_dir, "beach.shp")
cities_path <- "/home/shares/ohi/OHI_GOC/goal_prep/pressures/population_localities/inegi_boundaries_pop.shp"
urban_areas_csv <- file.path(data_dir, "GofC_urban_areas.csv")

out_dir <- file.path(data_dir, "city_csv")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

slugify <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

urban_areas <- read_csv(urban_areas_csv, show_col_types = FALSE) |>
  mutate(
    CVE_ENT = as.integer(.data$CVE_ENT),
    CVE_MUN = as.integer(.data$CVE_MUN),
    CVE_LOC = as.integer(.data$CVE_LOC)
  )

if (!all(c("city", "CVE_ENT", "CVE_MUN", "CVE_LOC") %in% names(urban_areas))) {
  stop("Expected columns `city`, `CVE_ENT`, `CVE_MUN`, `CVE_LOC` in ", urban_areas_csv)
}

cities_m <- st_read(cities_path) |>
  filter(.data$AMBITO == "Urbana") |>
  st_make_valid()

beach <- st_read(beach_path) |>
  st_make_valid()

# Coerce shapefile CVE fields (often stored as strings with padding) to integers
for (nm in c("CVE_ENT", "CVE_MUN", "CVE_LOC")) {
  if (nm %in% names(cities_m)) {
    cities_m[[nm]] <- suppressWarnings(as.integer(cities_m[[nm]]))
  }
}

if (!all(c("CVE_ENT", "CVE_MUN", "CVE_LOC") %in% names(cities_m))) {
  stop("Urban localities layer is missing one of CVE_ENT/CVE_MUN/CVE_LOC: ", cities_path)
}

city_list <- sort(unique(urban_areas$city))

for (city_nm in city_list) {
  #city_nm <- city_list[1]
  inc <- urban_areas |> filter(.data$city == city_nm)

  city_polys <- cities_m |>
    inner_join(inc, by = c("CVE_ENT", "CVE_MUN", "CVE_LOC"))

  if (nrow(city_polys) == 0) {
    warning("No matching polygons found for city = ", encodeString(city_nm), ". Skipping.")
    next
  }

  # Buffer each included locality polygon, then dissolve (union) into one buffer geometry
  urban_bufs <- city_polys |>
    st_transform(crs = st_crs(cities_m)) |>
    st_buffer(dist = as.numeric(buffer_m)) |>
    st_transform(crs = st_crs(beach))

  buf_sf <- st_as_sf(st_union(urban_bufs)) |> st_set_crs(st_crs(beach))

  beach_scope <- suppressWarnings(st_intersection(beach, buf_sf))

  out_path <- file.path(out_dir, paste0(slugify(city_nm), "_beaches_in_urban_buffer.csv"))

  if (nrow(beach_scope) == 0) {
    message("No beaches intersect buffer for city = ", city_nm, ". Writing empty CSV.")
    write_csv(tibble(), out_path)
    next
  }

  # Attribute each intersected piece by overlapping buffered locality (longest overlap wins)
  beach_scope <- beach_scope |> mutate(beach_piece_id = seq_len(nrow(beach_scope)))

  ints <- st_intersection(
    beach_scope |> select("beach_piece_id", "ID_Beach"),
    urban_bufs |> select(any_of(c("CVE_ENT", "CVE_MUN", "CVE_LOC", "NOM_LOC", "NOM_MUN")))
  )

  ints$seg_len_m <- as.numeric(st_length(ints))

  best <- ints |>
    st_drop_geometry() |>
    group_by(.data$beach_piece_id) |>
    slice_max(order_by = .data$seg_len_m, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select("beach_piece_id", any_of(c("CVE_ENT", "CVE_MUN", "CVE_LOC", "NOM_LOC", "NOM_MUN")))

  beach_labeled <- beach_scope |>
    left_join(best, by = "beach_piece_id") |>
    select(-"beach_piece_id")

  beach_labeled$length_km <- as.numeric(st_length(beach_labeled)) / 1000
  beach_labeled$city <- city_nm

  name_col <- if ("NOMBRE" %in% names(beach_labeled)) "NOMBRE" else if ("Nombre" %in% names(beach_labeled)) "Nombre" else NULL

  # Add expert-opinion fields expected by the visualization popup.
  # These will be blank (NA) until filled by an expert.
  beach_labeled$Name <- if (!is.null(name_col) && name_col %in% names(beach_labeled)) beach_labeled[[name_col]] else NA_character_
  beach_labeled$equitable_access_score <- NA_real_
  beach_labeled$notes <- NA_character_
  beach_labeled$Category <- NA_character_
  beach_labeled$urban_rural <- NA_character_
  beach_labeled$largely_gated_blocked <- NA_character_
  beach_labeled$public_roads_within_1km <- NA_character_
  beach_labeled$vehicle <- NA_character_
  beach_labeled$public_transportation <- NA_character_
  beach_labeled$parking <- NA_character_
  beach_labeled$number_paths <- NA_integer_
  beach_labeled$protected_fee <- NA_character_

  keep <- c(
    "city",
    "CVE_ENT", "CVE_MUN", "CVE_LOC",
    "NOM_MUN", "NOM_LOC",
    "ID_Beach",
    "Name",
    "length_km",
    "equitable_access_score",
    "Category",
    "urban_rural",
    "largely_gated_blocked",
    "public_roads_within_1km",
    "vehicle",
    "public_transportation",
    "parking",
    "number_paths",
    "protected_fee",
    "notes"
  )
  keep <- keep[!is.na(keep) & keep %in% names(beach_labeled)]

  listing <- beach_labeled |>
    st_drop_geometry() |>
    select(any_of(keep)) |>
    arrange(.data$ID_Beach, .data$CVE_LOC)

  write_csv(listing, out_path)
  message("Wrote ", out_path)
}


