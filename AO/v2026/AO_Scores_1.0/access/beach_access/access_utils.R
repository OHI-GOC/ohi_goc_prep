# Shared helpers for the Gulf of California beach access workflow.

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

#' Slugify a locality / analyst region name for filenames.
slugify_city <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

#' Path to master locality list CSV.
access_localities_csv <- function(data_dir) {
  file.path(data_dir, "GofC_access_localities.csv")
}

#' Path to per-locality beach scoring worksheet.
locality_beach_csv_path <- function(locality_nm, data_dir) {
  file.path(
    data_dir, "locality_beaches",
    paste0(slugify_city(locality_nm), "_beaches.csv")
  )
}

#' @rdname locality_beach_csv_path
city_csv_path <- locality_beach_csv_path

#' Directory for per-locality clipped support layers (GeoPackage extracts).
locality_support_dir <- function(locality_nm, data_dir) {
  file.path(data_dir, "locality_support", slugify_city(locality_nm))
}

#' @rdname locality_support_dir
city_support_dir <- locality_support_dir

#' Path for per-locality support-layer Leaflet HTML map.
locality_support_map_path <- function(locality_nm, access_dir) {
  file.path(
    access_dir, "localities_support_layers",
    paste0(slugify_city(locality_nm), "_support_layers.html")
  )
}

#' Expert-opinion columns carried through beach CSVs and maps.
expert_columns <- function() {
  c(
    "Name",
    "equitable_access_score",
    "Category",
    "urban_rural",
    "largely_gated_blocked",
    "malecon",
    "public_roads_within_1km",
    "vehicle",
    "public_transportation",
    "parking",
    "number_paths",
    "protected_fee",
    "notes"
  )
}

#' Merge expert columns from `expert_df` into `template_df` by `ID_Beach`.
merge_expert_scores <- function(template_df, expert_df, id_col = "ID_Beach") {
  expert_cols <- intersect(expert_columns(), names(expert_df))
  if (!length(expert_cols)) {
    return(template_df)
  }

  expert_df <- expert_df |>
    mutate(!!id_col := as.character(.data[[id_col]])) |>
    select(any_of(c(id_col, expert_cols)))

  template_df <- template_df |>
    mutate(!!id_col := as.character(.data[[id_col]]))

  merged <- template_df |>
    left_join(expert_df, by = id_col, suffix = c("", ".expert"))

  numeric_cols <- c("equitable_access_score", "number_paths", "length_km")
  integer_cols <- c("number_paths", "ID_Beach", "CVE_ENT", "CVE_MUN", "CVE_LOC")

  for (col in expert_cols) {
    expert_col <- paste0(col, ".expert")
    if (!expert_col %in% names(merged)) next

    expert_vals <- merged[[expert_col]]
    template_vals <- merged[[col]]
    keep_expert <- !is.na(expert_vals) & as.character(expert_vals) != ""
    merged[[col]] <- ifelse(keep_expert, expert_vals, template_vals)
    merged[[expert_col]] <- NULL

    if (col %in% integer_cols) {
      merged[[col]] <- suppressWarnings(as.integer(merged[[col]]))
    } else if (col %in% numeric_cols) {
      merged[[col]] <- suppressWarnings(as.numeric(merged[[col]]))
    }
  }

  merged
}

#' Convert legacy Mazatlán pilot CSV to beach worksheet schema.
legacy_mazatlan_to_expert <- function(legacy_df) {
  blockages_map <- c(
    none = "no",
    medium = "partial",
    high = "yes",
    obstructed = "yes"
  )

  legacy_df |>
    transmute(
      ID_Beach = as.character(.data$ID_beach),
      Name = .data$name,
      equitable_access_score = as.numeric(.data$accessibility_score),
      Category = NA_character_,
      urban_rural = NA_character_,
      largely_gated_blocked = dplyr::recode(
        tolower(as.character(.data$blockages)),
        !!!blockages_map,
        .default = NA_character_
      ),
      malecon = .data$malecon,
      public_roads_within_1km = .data$roads,
      vehicle = NA_character_,
      public_transportation = dplyr::if_else(
        tolower(as.character(.data$bus_access)) == "yes",
        "bus",
        dplyr::if_else(
          tolower(as.character(.data$bus_access)) == "no",
          "no",
          NA_character_
        )
      ),
      parking = .data$parking,
      number_paths = suppressWarnings(as.integer(.data$footways)),
      protected_fee = NA_character_,
      notes = NA_character_
    )
}

osm_path_highway_tags <- function() {
  c(
    "footway", "path", "steps", "pedestrian", "cycleway", "living_street",
    "sidewalk", "bridleway", "corridor", "elevator", "crossing"
  )
}

osm_access_road_highway_tags <- function() {
  c(
    "residential", "unclassified", "tertiary", "tertiary_link", "secondary",
    "secondary_link", "service", "track", "road"
  )
}

#' Load INEGI locality polygons and 20 km buffer for one analyst region (`city` column).
load_locality_study_region <- function(locality_nm,
                                       localities_csv,
                                       inegi_localities_path,
                                       buffer_m = units::set_units(20000, "m")) {
  listed <- readr::read_csv(
    localities_csv,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  ) |>
    mutate(
      CVE_ENT = as.integer(.data$CVE_ENT),
      CVE_MUN = as.integer(.data$CVE_MUN),
      CVE_LOC = as.integer(.data$CVE_LOC)
    ) |>
    filter(.data$city == locality_nm)

  if (nrow(listed) == 0) {
    stop("Locality not found in ", localities_csv, ": ", locality_nm)
  }

  polys <- st_read(inegi_localities_path, quiet = TRUE) |> st_make_valid()
  for (nm in c("CVE_ENT", "CVE_MUN", "CVE_LOC")) {
    if (nm %in% names(polys)) {
      polys[[nm]] <- suppressWarnings(as.integer(polys[[nm]]))
    }
  }

  locality_polys <- polys |>
    inner_join(listed, by = c("CVE_ENT", "CVE_MUN", "CVE_LOC")) |>
    st_make_valid()

  if (nrow(locality_polys) == 0) {
    stop("No INEGI polygons matched for locality = ", locality_nm)
  }

  locality_bufs <- locality_polys |>
    st_transform(st_crs(polys)) |>
    st_buffer(dist = as.numeric(buffer_m)) |>
    st_transform(4326)

  buf_sf <- st_as_sf(st_union(locality_bufs)) |> st_set_crs(4326)

  list(
    listed = listed,
    locality_polys = locality_polys,
    locality_bufs = locality_bufs,
    buf_sf = buf_sf,
    city_polys = locality_polys,
    city_bufs = locality_bufs
  )
}

#' @rdname load_locality_study_region
load_city_study_region <- load_locality_study_region

clip_layer_to_region <- function(layer, region, intersect_lines = TRUE) {
  if (is.null(layer) || nrow(layer) == 0) {
    return(layer)
  }
  layer <- st_make_valid(layer)
  bbox_sfc <- st_as_sfc(st_bbox(region), crs = st_crs(region))
  layer_bb <- suppressWarnings(st_filter(layer, bbox_sfc))
  if (nrow(layer_bb) == 0) {
    return(layer_bb)
  }
  if (!intersect_lines && all(st_geometry_type(layer_bb) %in% c("LINESTRING", "MULTILINESTRING"))) {
    return(suppressWarnings(st_filter(layer_bb, region)))
  }
  suppressWarnings(st_intersection(layer_bb, region))
}

fmt_popup <- function(x) {
  ifelse(is.na(x) | x == "", "", as.character(x))
}

beach_popup_labels <- function(df, id_col = "ID_Beach") {
  name_vals <- if ("Name" %in% names(df)) {
    df$Name
  } else if ("NOMBRE" %in% names(df)) {
    df$NOMBRE
  } else {
    rep(NA_character_, nrow(df))
  }

  city_vals <- if ("city" %in% names(df)) df$city else rep(NA_character_, nrow(df))
  length_vals <- if ("length_km" %in% names(df)) df$length_km else rep(NA_real_, nrow(df))
  score_vals <- if ("equitable_access_score" %in% names(df)) {
    df$equitable_access_score
  } else {
    rep(NA_real_, nrow(df))
  }

  sprintf(
    paste0(
      "<b>ID_Beach: %s</b><br/>",
      "<b>Name:</b> %s<br/>",
      "<b>Locality:</b> %s<br/>",
      "<b>Segment length (km):</b> %s<br/>",
      "<b>Equitable access score:</b> %s"
    ),
    fmt_popup(df[[id_col]]),
    fmt_popup(name_vals),
    fmt_popup(city_vals),
    fmt_popup(ifelse(is.na(length_vals), "", sprintf("%.3f", length_vals))),
    fmt_popup(score_vals)
  )
}

beach_hit_weight <- 14L

#' Default paths to shared-drive inputs (outside `_pressures/access`).
default_external_paths <- function(data_dir) {
  list(
    inegi_localities = "/home/shares/ohi/OHI_GOC/goal_prep/pressures/population_localities/inegi_boundaries_pop.shp",
    inegi_roads = "/home/shares/ohi/OHI_GOC/_raw_data/inegi_roads/794551132166_s/conjunto_de_datos/red_vial.shp",
    inegi_sites = "/home/shares/ohi/OHI_GOC/_raw_data/inegi_roads/794551132166_s/conjunto_de_datos/sitio_de_interes.shp",
    mpa = "/home/shares/ohi/OHI_GOC/_raw_data/CONANP/anpmx/anpmx.shp",
    beach = file.path(data_dir, "beach.shp"),
    osm_paths = file.path(data_dir, "paths_osm_Jan7_2025.shp"),
    osm_parking = file.path(data_dir, "parking_osm_Jan7_2025.shp")
  )
}

#' Read master locality list with integer CVE keys.
load_access_localities <- function(data_dir) {
  readr::read_csv(
    access_localities_csv(data_dir),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  ) |>
    mutate(
      CVE_ENT = as.integer(.data$CVE_ENT),
      CVE_MUN = as.integer(.data$CVE_MUN),
      CVE_LOC = as.integer(.data$CVE_LOC)
    )
}

#' Identify beaches within each analyst locality's 20 km buffer; write per-locality CSVs.
identify_all_locality_beaches <- function(data_dir,
                                          paths = default_external_paths(data_dir),
                                          buffer_m = units::set_units(20000, "m"),
                                          localities = NULL) {
  localities <- localities %||% load_access_localities(data_dir)
  if (!all(c("city", "CVE_ENT", "CVE_MUN", "CVE_LOC") %in% names(localities))) {
    stop("Expected columns city, CVE_ENT, CVE_MUN, CVE_LOC in locality CSV")
  }

  out_dir <- file.path(data_dir, "locality_beaches")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  cities_m <- sf::st_read(paths$inegi_localities, quiet = TRUE) |> sf::st_make_valid()
  beach <- sf::st_read(paths$beach, quiet = TRUE) |> sf::st_make_valid()
  for (nm in c("CVE_ENT", "CVE_MUN", "CVE_LOC")) {
    if (nm %in% names(cities_m)) {
      cities_m[[nm]] <- suppressWarnings(as.integer(cities_m[[nm]]))
    }
  }

  locality_list <- sort(unique(localities$city))
  for (locality_nm in locality_list) {
    inc <- localities |> filter(.data$city == locality_nm)
    city_polys <- cities_m |> inner_join(inc, by = c("CVE_ENT", "CVE_MUN", "CVE_LOC"))
    if (nrow(city_polys) == 0) {
      warning("No matching polygons for locality = ", locality_nm, "; skipping.")
      next
    }

    urban_bufs <- city_polys |>
      sf::st_transform(crs = sf::st_crs(cities_m)) |>
      sf::st_buffer(dist = as.numeric(buffer_m)) |>
      sf::st_transform(crs = sf::st_crs(beach))
    buf_sf <- sf::st_as_sf(sf::st_union(urban_bufs)) |> sf::st_set_crs(sf::st_crs(beach))
    beach_scope <- suppressWarnings(sf::st_intersection(beach, buf_sf))

    out_path <- locality_beach_csv_path(locality_nm, data_dir)
    existing_expert <- if (file.exists(out_path)) {
      readr::read_csv(out_path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
    } else {
      NULL
    }

    if (nrow(beach_scope) == 0) {
      message("No beaches in buffer for ", locality_nm)
      readr::write_csv(tibble::tibble(), out_path)
      next
    }

    beach_scope <- beach_scope |> mutate(beach_piece_id = seq_len(nrow(beach_scope)))
    ints <- sf::st_intersection(
      beach_scope |> select("beach_piece_id", "ID_Beach"),
      urban_bufs |> select(any_of(c("CVE_ENT", "CVE_MUN", "CVE_LOC", "NOM_LOC", "NOM_MUN")))
    )
    ints$seg_len_m <- as.numeric(sf::st_length(ints))
    best <- ints |>
      sf::st_drop_geometry() |>
      group_by(.data$beach_piece_id) |>
      slice_max(order_by = .data$seg_len_m, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select("beach_piece_id", any_of(c("CVE_ENT", "CVE_MUN", "CVE_LOC", "NOM_LOC", "NOM_MUN")))

    beach_labeled <- beach_scope |>
      left_join(best, by = "beach_piece_id") |>
      select(-"beach_piece_id")
    beach_labeled$length_km <- as.numeric(sf::st_length(beach_labeled)) / 1000
    beach_labeled$city <- locality_nm

    name_col <- if ("NOMBRE" %in% names(beach_labeled)) {
      "NOMBRE"
    } else if ("Nombre" %in% names(beach_labeled)) {
      "Nombre"
    } else {
      NULL
    }
    beach_labeled$Name <- if (!is.null(name_col)) beach_labeled[[name_col]] else NA_character_
    for (col in setdiff(expert_columns(), c("Name"))) {
      if (!col %in% names(beach_labeled)) {
        beach_labeled[[col]] <- if (col %in% c("equitable_access_score", "length_km")) NA_real_ else NA_character_
      }
    }
    if (!"number_paths" %in% names(beach_labeled)) beach_labeled$number_paths <- NA_integer_

    keep <- c(
      "city", "CVE_ENT", "CVE_MUN", "CVE_LOC", "NOM_MUN", "NOM_LOC", "ID_Beach",
      expert_columns(), "length_km"
    )
    keep <- unique(keep[keep %in% names(beach_labeled)])
    listing <- beach_labeled |>
      sf::st_drop_geometry() |>
      select(any_of(keep)) |>
      arrange(.data$ID_Beach, .data$CVE_LOC)

    if (!is.null(existing_expert)) {
      listing <- merge_expert_scores(listing, existing_expert)
    }
    readr::write_csv(listing, out_path)
    message("Wrote ", out_path, " (n=", nrow(listing), ")")
  }
  invisible(locality_list)
}

#' Simple Leaflet overview of all analyst localities and 20 km buffers.
build_localities_overview_map <- function(data_dir,
                                          access_dir,
                                          paths = default_external_paths(data_dir),
                                          buffer_m = units::set_units(20000, "m")) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' required")
  }
  localities <- load_access_localities(data_dir)
  cities_m <- sf::st_read(paths$inegi_localities, quiet = TRUE) |> sf::st_make_valid()
  for (nm in c("CVE_ENT", "CVE_MUN", "CVE_LOC")) {
    if (nm %in% names(cities_m)) cities_m[[nm]] <- suppressWarnings(as.integer(cities_m[[nm]]))
  }

  polys <- cities_m |>
    inner_join(localities, by = c("CVE_ENT", "CVE_MUN", "CVE_LOC")) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)

  nom_loc <- if ("NOM_LOC" %in% names(polys)) {
    polys$NOM_LOC
  } else if ("NOM_LOC.x" %in% names(polys)) {
    polys$NOM_LOC.x
  } else {
    rep(NA_character_, nrow(polys))
  }
  ambito <- if ("AMBITO" %in% names(polys)) polys$AMBITO else rep(NA_character_, nrow(polys))
  polys$popup <- paste0("<b>", polys$city, "</b><br>", nom_loc, " (", ambito, ")")

  bufs <- polys |>
    sf::st_transform(sf::st_crs(cities_m)) |>
    sf::st_buffer(dist = as.numeric(buffer_m)) |>
    sf::st_transform(4326)

  bb <- sf::st_bbox(sf::st_union(bufs))
  out_html <- file.path(access_dir, "output", "all_localities_map.html")
  dir.create(dirname(out_html), showWarnings = FALSE, recursive = TRUE)

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light basemap") |>
    leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
    leaflet::addPolygons(
      data = bufs, color = "#ff00ff", weight = 1, fillColor = "#ff00ff",
      fillOpacity = 0.05, group = "20 km buffers"
    ) |>
    leaflet::addPolygons(
      data = polys, color = "#ffffff", weight = 2, fillColor = "#00bcd4",
      fillOpacity = 0.35, popup = ~popup, group = "Locality footprints"
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Esri.WorldImagery", "Light basemap"),
      overlayGroups = c("20 km buffers", "Locality footprints"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  htmlwidgets::saveWidget(m, out_html, selfcontained = TRUE)
  message("Wrote ", out_html)
  invisible(out_html)
}

#' Leaflet map of beach segments colored by equitable_access_score.
build_beach_access_map <- function(locality_nm,
                                   data_dir,
                                   access_dir,
                                   paths = default_external_paths(data_dir),
                                   expert_csv = NULL,
                                   output_html = NULL) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' required")
  }
  expert_csv <- expert_csv %||% locality_beach_csv_path(locality_nm, data_dir)
  output_html <- output_html %||% file.path(
    access_dir, "output", "pilot",
    paste0(slugify_city(locality_nm), "_beach_access_map.html")
  )
  if (!file.exists(expert_csv)) stop("Beach CSV not found: ", expert_csv)

  beach <- sf::st_read(paths$beach, quiet = TRUE)
  expert <- readr::read_csv(expert_csv, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  beach_sub <- beach |> filter(.data$ID_Beach %in% expert$ID_Beach)
  map_data <- left_join(beach_sub, expert, by = "ID_Beach", suffix = c("", ".expert"))

  for (col in intersect(expert_columns(), names(map_data))) {
    expert_col <- paste0(col, ".expert")
    if (expert_col %in% names(map_data)) {
      map_data[[col]] <- dplyr::coalesce(map_data[[expert_col]], map_data[[col]])
      map_data[[expert_col]] <- NULL
    }
  }
  map_data$length_km <- as.numeric(sf::st_length(map_data)) / 1000

  pal <- leaflet::colorNumeric(
    palette = grDevices::colorRampPalette(c("red", "orange", "yellow", "green", "cyan", "blue"))(256),
    domain = c(0, 10),
    na.color = "gray80"
  )
  map_data$popup_text <- beach_popup_labels(map_data)

  m <- leaflet::leaflet(map_data) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) |>
    leaflet::addPolylines(color = "transparent", weight = beach_hit_weight, opacity = 0, popup = ~popup_text) |>
    leaflet::addPolylines(color = ~pal(equitable_access_score), weight = 6, popup = ~popup_text)

  dir.create(dirname(output_html), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(m, output_html, selfcontained = TRUE)
  message("Wrote ", output_html)
  invisible(output_html)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

write_gpkg_layer <- function(obj, path) {
  if (is.null(obj) || nrow(obj) == 0) {
    message("  (empty) ", basename(path))
    return(invisible(FALSE))
  }
  sf::st_write(obj, path, delete_dsn = TRUE, quiet = TRUE)
  message("  Wrote ", path, " (n=", nrow(obj), ")")
  invisible(TRUE)
}

#' Extract full INEGI/OSM support layers for one analyst locality (no thinning).
extract_locality_support <- function(locality_nm,
                                     data_dir,
                                     access_dir,
                                     paths = list(),
                                     refetch_osm = FALSE,
                                     buffer_m = units::set_units(20000, "m")) {
  localities_csv <- access_localities_csv(data_dir)
  region <- load_locality_study_region(
    locality_nm = locality_nm,
    localities_csv = localities_csv,
    inegi_localities_path = paths$inegi_localities,
    buffer_m = buffer_m
  )

  out_dir <- locality_support_dir(locality_nm, data_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  buf_sf <- region$buf_sf
  roads_wkt <- sf::st_as_text(sf::st_geometry(buf_sf))

  roads <- sf::st_read(paths$inegi_roads, wkt_filter = roads_wkt, quiet = TRUE) |>
    sf::st_transform(sf::st_crs(buf_sf))
  site <- sf::st_read(paths$inegi_sites, wkt_filter = roads_wkt, quiet = TRUE) |>
    sf::st_transform(sf::st_crs(buf_sf))
  beach <- sf::st_read(paths$beach, quiet = TRUE) |> sf::st_make_valid()
  mpa_all <- paths$mpa
  mpa <- mpa_all |> sf::st_transform(sf::st_crs(buf_sf))

  if (refetch_osm) {
    if (!requireNamespace("osmdata", quietly = TRUE)) {
      stop("Package 'osmdata' required when refetch_osm = TRUE")
    }
    bb <- sf::st_bbox(buf_sf)
    fetch_lines <- function(tags) {
      q <- osmdata::opq(bbox = bb, timeout = 240) |>
        osmdata::add_osm_feature(key = "highway", value = tags)
      lines <- osmdata::osmdata_sf(q)$osm_lines
      if (is.null(lines)) {
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }
      lines
    }
    paths_osm <- fetch_lines(osm_path_highway_tags())
    roads_osm <- fetch_lines(osm_access_road_highway_tags())
    parking_osm <- tryCatch({
      q <- osmdata::opq(bbox = bb, timeout = 240) |>
        osmdata::add_osm_feature(key = "amenity", value = c("parking", "parking_space"))
      polys <- osmdata::osmdata_sf(q)$osm_polygons
      if (is.null(polys)) sf::st_sf(geometry = sf::st_sfc(crs = 4326)) else polys
    }, error = function(e) {
      warning("OSM parking fetch failed: ", conditionMessage(e))
      sf::st_sf(geometry = sf::st_sfc(crs = 4326))
    })
  } else {
    paths_osm <- clip_layer_to_region(paths$osm_paths, buf_sf, intersect_lines = FALSE)
    parking_osm <- clip_layer_to_region(paths$osm_parking, buf_sf)
    roads_osm <- if ("highway" %in% names(paths$osm_paths)) {
      clip_layer_to_region(
        dplyr::filter(paths$osm_paths, .data$highway %in% osm_access_road_highway_tags()),
        buf_sf,
        intersect_lines = FALSE
      )
    } else {
      sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(buf_sf)))
    }
  }

  roads_inegi <- clip_layer_to_region(roads, buf_sf, intersect_lines = FALSE)
  beaches <- clip_layer_to_region(beach, buf_sf)
  sites <- dplyr::bind_rows(
    site |> dplyr::filter(.data$CLASE %in% c("Puerto", "Muelle o Embarcadero")) |>
      dplyr::mutate(site_type = "port_pier"),
    site |> dplyr::filter(.data$CLASE %in% c("Central Camionera")) |>
      dplyr::mutate(site_type = "bus_terminal")
  ) |> clip_layer_to_region(buf_sf)
  mpa_clip <- clip_layer_to_region(mpa, buf_sf)

  polys <- region$locality_polys |> sf::st_transform(sf::st_crs(buf_sf))
  bufs <- region$locality_bufs

  write_gpkg_layer(buf_sf, file.path(out_dir, "study_region.gpkg"))
  write_gpkg_layer(polys, file.path(out_dir, "locality_footprints.gpkg"))
  write_gpkg_layer(bufs, file.path(out_dir, "locality_buffers.gpkg"))
  write_gpkg_layer(beaches, file.path(out_dir, "beaches.gpkg"))
  write_gpkg_layer(roads_inegi, file.path(out_dir, "roads_inegi.gpkg"))
  write_gpkg_layer(paths_osm, file.path(out_dir, "paths_osm.gpkg"))
  write_gpkg_layer(roads_osm, file.path(out_dir, "roads_osm.gpkg"))
  write_gpkg_layer(parking_osm, file.path(out_dir, "parking_osm.gpkg"))
  write_gpkg_layer(sites, file.path(out_dir, "sites_inegi.gpkg"))
  write_gpkg_layer(mpa_clip, file.path(out_dir, "mpa.gpkg"))

  summary <- tibble::tibble(
    city = locality_nm,
    slug = slugify_city(locality_nm),
    refetch_osm = refetch_osm,
    n_localities = nrow(polys),
    n_beaches = nrow(beaches),
    n_roads_inegi = nrow(roads_inegi),
    n_paths_osm = nrow(paths_osm),
    n_roads_osm = nrow(roads_osm),
    n_parking_osm = nrow(parking_osm),
    n_sites_inegi = nrow(sites),
    n_mpa = nrow(mpa_clip)
  )
  readr::write_csv(summary, file.path(out_dir, "extraction_summary.csv"))
  summary
}

#' Build per-locality support-layer Leaflet HTML from extracted GeoPackages.
build_locality_support_map <- function(locality_nm, data_dir, access_dir) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' required")
  }
  support_dir <- locality_support_dir(locality_nm, data_dir)
  read_gpkg <- function(path) {
    if (!file.exists(path)) return(NULL)
    sf::st_read(path, quiet = TRUE) |> sf::st_make_valid()
  }

  buf_sf <- read_gpkg(file.path(support_dir, "study_region.gpkg"))
  polys <- read_gpkg(file.path(support_dir, "locality_footprints.gpkg"))
  bufs <- read_gpkg(file.path(support_dir, "locality_buffers.gpkg"))
  beaches <- read_gpkg(file.path(support_dir, "beaches.gpkg"))
  roads_inegi <- read_gpkg(file.path(support_dir, "roads_inegi.gpkg"))
  paths_osm <- read_gpkg(file.path(support_dir, "paths_osm.gpkg"))
  roads_osm <- read_gpkg(file.path(support_dir, "roads_osm.gpkg"))
  parking_osm <- read_gpkg(file.path(support_dir, "parking_osm.gpkg"))
  sites <- read_gpkg(file.path(support_dir, "sites_inegi.gpkg"))
  mpa <- read_gpkg(file.path(support_dir, "mpa.gpkg"))

  beach_csv <- locality_beach_csv_path(locality_nm, data_dir)
  if (file.exists(beach_csv)) {
    meta <- readr::read_csv(beach_csv, show_col_types = FALSE) |>
      mutate(ID_Beach = as.integer(.data$ID_Beach)) |>
      group_by(.data$ID_Beach) |> slice(1) |> ungroup()
  } else {
    meta <- tibble::tibble(ID_Beach = integer())
  }

  if (!is.null(beaches) && nrow(beaches)) {
    beaches$seg_length_km <- as.numeric(sf::st_length(beaches)) / 1000
    beaches <- beaches |>
      mutate(ID_Beach = as.integer(.data$ID_Beach)) |>
      left_join(sf::st_drop_geometry(meta), by = "ID_Beach") |>
      mutate(length_km = dplyr::coalesce(.data$length_km, .data$seg_length_km))
    beaches$popup_text <- beach_popup_labels(beaches)
  }
  if (!is.null(polys) && nrow(polys)) {
    polys <- polys |> mutate(popup = paste0("<b>", city, "</b><br>AMBITO: ", AMBITO))
  }

  bb <- sf::st_bbox(buf_sf)
  out_html <- locality_support_map_path(locality_nm, access_dir)
  dir.create(dirname(out_html), showWarnings = FALSE, recursive = TRUE)

  sm_path <- file.path(support_dir, "extraction_summary.csv")
  map_title <- if (file.exists(sm_path)) {
    sm <- readr::read_csv(sm_path, show_col_types = FALSE)
    sprintf("%s — INEGI roads: %s | OSM paths: %s | parking: %s",
            locality_nm, sm$n_roads_inegi, sm$n_paths_osm, sm$n_parking_osm)
  } else {
    paste(locality_nm, "support layers")
  }

  add_if <- function(m, fun, data, ...) {
    if (is.null(data) || nrow(data) == 0) return(m)
    fun(m, data = data, ...)
  }

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light basemap") |>
    leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

  m <- add_if(m, leaflet::addPolygons, mpa, color = "#556b2f", weight = 2,
              fillColor = "#9acd32", fillOpacity = 0.25, group = "MPAs")
  m <- add_if(m, leaflet::addPolygons, bufs, color = "#ff00ff", weight = 2,
              fillColor = "#ff00ff", fillOpacity = 0.04, group = "20 km buffers")
  m <- add_if(m, leaflet::addPolygons, polys, color = "#ffffff", weight = 2,
              fillColor = "#00bcd4", fillOpacity = 0.22, popup = ~popup, group = "Locality footprints")
  m <- add_if(m, leaflet::addPolygons, parking_osm, color = "#9c27b0", weight = 1,
              fillColor = "#ce93d8", fillOpacity = 0.55, group = "OSM parking")
  m <- add_if(m, leaflet::addPolylines, roads_inegi, color = "#ff9800", weight = 3,
              opacity = 0.85, group = "INEGI roads")
  m <- add_if(m, leaflet::addPolylines, roads_osm, color = "#ff5722", weight = 2,
              opacity = 0.75, group = "OSM access roads")
  m <- add_if(m, leaflet::addPolylines, paths_osm, color = "#00e676", weight = 2,
              opacity = 0.9, group = "OSM paths")
  m <- add_if(m, leaflet::addCircleMarkers, sites, radius = 6, color = "#0277bd",
              fillColor = "#4fc3f7", fillOpacity = 0.9, weight = 2, group = "INEGI sites")
  if (!is.null(beaches) && nrow(beaches)) {
    m <- m |>
      leaflet::addPolylines(data = beaches, color = "transparent", weight = beach_hit_weight,
                            opacity = 0, popup = ~popup_text, group = "Beaches") |>
      leaflet::addPolylines(data = beaches, color = "#ffd600", weight = 5,
                            opacity = 1, popup = ~popup_text, group = "Beaches")
  }

  overlays <- c("20 km buffers", "Locality footprints", "MPAs", "INEGI roads",
                "OSM access roads", "OSM paths", "OSM parking", "INEGI sites", "Beaches")
  overlays <- overlays[overlays %in% names(m$x$groups)]

  m <- m |>
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Light basemap"),
      overlayGroups = overlays,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addControl(map_title, position = "topright")

  htmlwidgets::saveWidget(m, out_html, selfcontained = TRUE)
  message("Wrote ", out_html)
  invisible(out_html)
}
