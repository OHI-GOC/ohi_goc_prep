# Build capped-raster GIF timelapses and HTML gallery for GoC vessel pressures.
#
# Outputs:
#   _pressures/vessels/figures/{pressure}_capped_timelapse.gif
#   _pressures/vessels/figures/capped_timelapses.html
#
# Run from repo root:
#   Rscript _pressures/vessels/scripts/build_pressure_timelapses.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(sf)
  library(terra)
  library(magick)
})

PRESSURE_ROOT <- "/home/shares/ohi/OHI_GOC/goal_prep/pressures/vessels"
FIGURES_DIR <- here("_pressures/vessels/figures")
FRAMES_ROOT <- here("_pressures/vessels/figures/_timelapse_frames")

pressures <- tibble(
  pressure_name = c(
    "vessel_traffic",
    "benthic_destructive_fishing",
    "ghost_gear"
  ),
  subdir = c(
    "vessel_traffic",
    "benthic_destructive_fishing",
    "ghost_gear"
  ),
  file_stub = c(
    "vms_hours",
    "destructive_fishing_hours",
    "ghost_gear_risk"
  ),
  plot_title = c(
    "Vessel traffic hours (capped)",
    "Bottom-destructive fishing hours (capped)",
    "Ghost gear risk (capped)"
  )
)

pressure_tif_path <- function(subdir, file_stub, year, capped = FALSE) {
  dir_path <- file.path(PRESSURE_ROOT, subdir)
  suffix <- if (capped) "_1km_capped.tif" else "_1km.tif"
  path <- file.path(dir_path, sprintf("%s_%s%s", file_stub, year, suffix))
  if (file.exists(path)) path else NA_character_
}

available_years <- function(subdir, file_stub, capped = FALSE) {
  dir_path <- file.path(PRESSURE_ROOT, subdir)
  pattern <- if (capped) {
    paste0("^", file_stub, "_[0-9]{4}_1km_capped\\.tif$")
  } else {
    paste0("^", file_stub, "_[0-9]{4}_1km\\.tif$")
  }
  files <- list.files(dir_path, pattern = pattern)
  as.integer(str_extract(files, "[0-9]{4}"))
}

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

ohi_rgns_marine <- st_read(here("spatial/ohi_regions/goc_ohi_rgns.shp"), quiet = TRUE) %>%
  filter(location == "marine") %>%
  arrange(rgn_id)

map_colors <- hcl.colors(55, "YlOrRd", rev = TRUE)
rgn_vect <- vect(ohi_rgns_marine)

write_timelapse_frame <- function(r, year, title, breaks, out_path) {
  png(out_path, width = 960, height = 720, res = 120, bg = "white")
  on.exit(dev.off(), add = TRUE)

  par(mar = c(0.5, 0.5, 2.8, 5), bg = "white")
  plot(
    r,
    col = map_colors,
    breaks = breaks,
    axes = FALSE,
    legend = "right",
    main = paste0(title, " — ", year),
    plg = list(title = "log1p(value)")
  )
  plot(rgn_vect, border = rgb(1, 1, 1, 0.85), lwd = 0.7, add = TRUE)
  plot(rgn_vect, border = "grey35", lwd = 0.25, add = TRUE)
}

animate_capped_pressure <- function(p) {
  years <- sort(available_years(p$subdir, p$file_stub, capped = TRUE))
  if (length(years) == 0) {
    warning("No capped rasters for ", p$pressure_name)
    return(invisible(NULL))
  }

  zmax <- max(
    map_dbl(years, function(year) {
      tif <- pressure_tif_path(p$subdir, p$file_stub, year, capped = TRUE)
      global(log1p(rast(tif)), "max", na.rm = TRUE)[[1]]
    }),
    na.rm = TRUE
  )
  breaks <- seq(0, zmax, length.out = length(map_colors) + 1)

  frame_dir <- file.path(FRAMES_ROOT, p$pressure_name)
  dir.create(frame_dir, recursive = TRUE, showWarnings = FALSE)

  frame_paths <- map_chr(years, function(year) {
    tif <- pressure_tif_path(p$subdir, p$file_stub, year, capped = TRUE)
    out_path <- file.path(frame_dir, sprintf("frame_%s.png", year))
    write_timelapse_frame(
      r = log1p(rast(tif)),
      year = year,
      title = p$plot_title,
      breaks = breaks,
      out_path = out_path
    )
    out_path
  })

  gif_path <- file.path(FIGURES_DIR, paste0(p$pressure_name, "_capped_timelapse.gif"))
  image_read(frame_paths) %>%
    image_animate(fps = 2, dispose = "previous") %>%
    image_write(gif_path)

  message("Wrote timelapse → ", gif_path)
  invisible(gif_path)
}

write_timelapse_gallery <- function(pressures_tbl, figures_dir) {
  gallery_path <- file.path(figures_dir, "capped_timelapses.html")

  gallery_body <- pmap_chr(pressures_tbl, function(pressure_name, plot_title, ...) {
    gif_file <- paste0(pressure_name, "_capped_timelapse.gif")
    sprintf(
      "<section><h2>%s</h2><p>Capped 1 km raster, 2007–2025 (2 frames/s)</p><img src=\"%s\" alt=\"%s timelapse\"></section>",
      plot_title,
      gif_file,
      pressure_name
    )
  }) %>%
    paste(collapse = "\n")

  writeLines(
    c(
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "  <meta charset=\"utf-8\">",
      "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      "  <title>GoC vessel pressure timelapses</title>",
      "  <style>",
      "    body { font-family: system-ui, sans-serif; max-width: 980px; margin: 0 auto; padding: 1.5rem; color: #222; background: #fafafa; }",
      "    h1 { margin-bottom: 0.25rem; }",
      "    .lead { color: #555; margin-top: 0; }",
      "    section { margin: 2.5rem 0; padding: 1rem; background: #fff; border: 1px solid #ddd; border-radius: 8px; }",
      "    img { display: block; width: 100%; height: auto; margin-top: 0.75rem; border: 1px solid #ccc; border-radius: 4px; }",
      "  </style>",
      "</head>",
      "<body>",
      "  <h1>Gulf of California vessel pressures</h1>",
      "  <p class=\"lead\">Capped raster timelapses (log1p scale, fixed legend per pressure).</p>",
      gallery_body,
      "</body>",
      "</html>"
    ),
    gallery_path
  )

  message("Wrote gallery → ", gallery_path)
  invisible(gallery_path)
}

timelapse_paths <- map(seq_len(nrow(pressures)), function(i) {
  animate_capped_pressure(pressures[i, ])
})
names(timelapse_paths) <- pressures$pressure_name

write_timelapse_gallery(pressures, FIGURES_DIR)
