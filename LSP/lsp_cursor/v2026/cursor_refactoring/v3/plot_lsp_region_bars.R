#!/usr/bin/env Rscript
#' Bar charts for LSP regional status scores (and optional LSP-level panels).
#'
#' Defaults to Mazu `small_updates_050826` folder; falls back to `lsp/v2025/output`
#' relative to repo root when that path is missing.
#'
#' Env override: `LSP_BAR_DATA_DIR` = directory containing the CSV exports.
#'
#' Outputs PNG (+ PDF) under `<DATA_DIR>/figures_region_bars/`.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(rlang)
})

normalize_rgn_id <- function(x) {
  str_replace_all(as.character(x), " ", "_")
}

rgn_levels <- paste0("Region_", 1:9)
rgn_colors <- c(
  Region_1 = "#C03729",
  Region_2 = "#E68C7C",
  Region_3 = "#FC8F24",
  Region_4 = "#dcaa38",
  Region_5 = "#cbd2a0",
  Region_6 = "#98a25a",
  Region_7 = "#747428",
  Region_8 = "#627391",
  Region_9 = "#8d61a9"
)

default_mazu <- "/home/shares/ohi/OHI_GOC/goal_prep/sp/lsp/v2025/output/small_updates_050826"
ca <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", ca, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}
repo_fallback <- normalizePath(
  file.path(script_dir, "..", "lsp", "v2025", "output"),
  mustWork = FALSE
)
if (!dir.exists(repo_fallback) && requireNamespace("here", quietly = TRUE)) {
  repo_fallback <- suppressWarnings(
    normalizePath(here::here("lsp", "v2025", "output"), mustWork = FALSE)
  )
}

DATA_DIR <- Sys.getenv("LSP_BAR_DATA_DIR", unset = default_mazu)
if (!nzchar(DATA_DIR) || !dir.exists(DATA_DIR)) {
  if (dir.exists(repo_fallback)) {
    DATA_DIR <- repo_fallback
    message("Using fallback DATA_DIR: ", DATA_DIR)
  } else {
    stop(
      "DATA_DIR does not exist: ", Sys.getenv("LSP_BAR_DATA_DIR", unset = default_mazu),
      "\nSet LSP_BAR_DATA_DIR or sync CSVs locally."
    )
  }
}

FIG_DIR <- file.path(DATA_DIR, "figures_region_bars")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
message("Reading CSVs from: ", DATA_DIR)
message("Writing figures to:  ", FIG_DIR)

theme_lsp_bars <- function() {
  list(
    theme_minimal(base_size = 11),
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  )
}

plot_regional_summary <- function(dat, value_col, title, ylab = "current status (0-100)") {
  if (!value_col %in% names(dat)) {
    stop("Missing column ", value_col)
  }
  y <- sym(value_col)
  dat <- dat %>%
    mutate(
      rgn_id = normalize_rgn_id(rgn_id),
      rgn_id = factor(rgn_id, levels = rgn_levels)
    ) %>%
    filter(!is.na(rgn_id))
  ggplot(dat, aes(rgn_id, !!y)) +
    geom_col(
      aes(fill = rgn_id),
      width = 0.82,
      colour = "white",
      linewidth = 0.35
    ) +
    scale_fill_manual(values = rgn_colors, drop = FALSE, guide = "none") +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    labs(x = NULL, y = ylab, title = title) +
    theme_lsp_bars()
}

save_plot <- function(plot, stem) {
  png <- file.path(FIG_DIR, paste0(stem, ".png"))
  pdf <- file.path(FIG_DIR, paste0(stem, ".pdf"))
  ggsave(png, plot, width = 7.5, height = 4.25, dpi = 300, bg = "white")
  ggsave(pdf, plot, width = 7.5, height = 4.25, bg = "white")
  message("Wrote ", png)
}

read_region_condition <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(rgn_id = normalize_rgn_id(rgn_id)) %>%
    dplyr::select(rgn_id, dplyr::any_of(c("condition", "current_status", "year", "n_features")))
}

# --- Regional summary charts -------------------------------------------------

regional_specs <- data.frame(
  basename_csv = c(
    "lsp_goal_current_status_by_region.csv",
    "condition_region_special_habitats.csv",
    "condition_region_protected_areas.csv",
    "condition_region_fishing_sites.csv",
    "condition_region_dive_sites.csv",
    "condition_region_archaeological_sites.csv"
  ),
  value_col = c("current_status", rep("condition", 5L)),
  stem = c(
    "lsp_goal_current_status_by_region",
    "condition_region_special_habitats",
    "condition_region_protected_areas",
    "condition_region_fishing_sites",
    "condition_region_dive_sites",
    "condition_region_archaeological_sites"
  ),
  title = c(
    "LSP goal: current status (mean across categories)",
    "Special habitats: regional mean condition",
    "Protected areas: regional mean condition",
    "Fishing sites: regional mean condition",
    "Dive sites: regional mean condition",
    "Archaeological sites: regional mean condition"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(regional_specs))) {
  bn <- regional_specs$basename_csv[i]
  vc <- regional_specs$value_col[i]
  stem <- regional_specs$stem[i]
  title <- regional_specs$title[i]
  fp <- file.path(DATA_DIR, bn)
  if (!file.exists(fp)) {
    warning("Skipping missing file: ", fp)
    next
  }
  raw <- read_region_condition(fp)
  if (!vc %in% names(raw)) {
    warning("Column ", vc, " not in ", bn)
    next
  }
  p <- plot_regional_summary(raw, vc, title)
  save_plot(p, stem)
}

optional_region <- file.path(DATA_DIR, "condition_region_other_marine_places.csv")
if (file.exists(optional_region)) {
  raw <- read_region_condition(optional_region)
  p <- plot_regional_summary(raw, "condition", "Other marine places: regional mean condition")
  save_plot(p, "condition_region_other_marine_places")
}

# --- LSP-level (one row per place), faceted by region -------------------------

plot_lsp_facets <- function(dat, label_col, title) {
  vc <- "overall_condition"
  if (!vc %in% names(dat)) {
    stop("Need overall_condition column")
  }
  xv <- sym(vc)
  dat <- dat %>%
    mutate(!!xv := suppressWarnings(as.numeric(.data[[vc]]))) %>%
    filter(!is.na(normalize_rgn_id(rgn_id)), !is.na(.data[[vc]])) %>%
    mutate(
      rgn_id = factor(normalize_rgn_id(rgn_id), levels = rgn_levels),
      lab_chr = str_trunc(as.character(.data[[label_col]]), w = 72),
      lab_plot = paste0(sprintf("%.0f  ", .data[[vc]]), lab_chr)
    ) %>%
    group_by(rgn_id) %>%
    arrange(desc(.data[[vc]])) %>%
    mutate(lab_plot = fct_inorder(lab_plot)) %>%
    ungroup()

  ggplot(dat, aes(!!xv, lab_plot)) +
    geom_col(aes(fill = rgn_id), width = 0.85, colour = "white", linewidth = 0.25) +
    scale_fill_manual(values = rgn_colors, drop = FALSE, guide = "none") +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    facet_wrap(~rgn_id, ncol = 3, scales = "free_y") +
    labs(x = "Overall condition (0–100)", y = NULL, title = title) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 11)
    )
}

lsp_specs <- data.frame(
  basename_csv = c(
    "condition_lsp_protected_areas.csv",
    "condition_lsp_fishing_sites.csv",
    "condition_lsp_dive_sites.csv",
    "condition_lsp_archaeological_sites.csv"
  ),
  label_col = c(
    "lasting_special_place_description",
    "lasting_special_place_lsp",
    "nombre_sitio_de_buceo",
    "lasting_special_place_lsp"
  ),
  stem = c(
    "condition_lsp_protected_areas_bars",
    "condition_lsp_fishing_sites_bars",
    "condition_lsp_dive_sites_bars",
    "condition_lsp_archaeological_sites_bars"
  ),
  title = c(
    "Protected areas: scores by listed LSP",
    "Fishing sites: scores by listed LSP",
    "Dive sites: scores by priority site",
    "Archaeological sites: scores by listed LSP"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(lsp_specs))) {
  bn <- lsp_specs$basename_csv[i]
  lc <- lsp_specs$label_col[i]
  stem <- lsp_specs$stem[i]
  title <- lsp_specs$title[i]
  fp <- file.path(DATA_DIR, bn)
  if (!file.exists(fp)) {
    warning("Skipping missing LSP file: ", fp)
    next
  }
  raw <- read_csv(fp, show_col_types = FALSE)
  if (!all(c("rgn_id", lc, "overall_condition") %in% names(raw))) {
    warning("Unexpected columns in ", bn)
    next
  }
  p <- plot_lsp_facets(raw, lc, title)
  n_rows <- nrow(raw)
  h <- max(5.5, min(28, 2 + 0.18 * n_rows))
  png <- file.path(FIG_DIR, paste0(stem, ".png"))
  pdf <- file.path(FIG_DIR, paste0(stem, ".pdf"))
  ggsave(png, p, width = 10, height = h, dpi = 300, bg = "white", limitsize = FALSE)
  ggsave(pdf, p, width = 10, height = h, limitsize = FALSE)
  message("Wrote ", png)
}

message("Done.")
