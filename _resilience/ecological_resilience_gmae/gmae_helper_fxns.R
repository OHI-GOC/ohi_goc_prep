# Helpers for exploring manually downloaded GMAE / CNGE topic packages.
# Packages live as unzipped folders under:
#   /home/shares/ohi/OHI_GOC/_raw_data/GMAE_environmental_state_data/d2026

goc_state_codes <- c("02", "03", "26", "25", "18", "14")

goc_state_names <- c(
  "02" = "Baja California",
  "03" = "Baja California Sur",
  "26" = "Sonora",
  "25" = "Sinaloa",
  "18" = "Nayarit",
  "14" = "Jalisco"
)

read_csv_flexible <- function(path, ...) {
  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...),
    error = function(e) NULL
  )
  if (!is.null(out)) {
    return(out)
  }
  readr::read_csv(
    path,
    locale = readr::locale(encoding = "Latin1"),
    show_col_types = FALSE,
    progress = FALSE,
    ...
  )
}

# Strip macOS duplicate download suffixes like " (1)" or " (2)".
clean_folder_name <- function(x) {
  stringr::str_replace(x, " \\([0-9]+\\)$", "")
}

parse_package_folder <- function(folder_name) {
  clean <- clean_folder_name(folder_name)
  clean2 <- stringr::str_replace(clean, "^conjunto_de_datos_", "")
  m <- stringr::str_match(clean2, "(cnge|cngspspe)([0-9]{4})_csv$")
  if (any(is.na(m))) {
    return(NULL)
  }
  topic <- stringr::str_replace(clean2, "(cnge|cngspspe)[0-9]{4}_csv$", "")
  topic <- stringr::str_replace(topic, "_$", "")
  list(
    folder_name = folder_name,
    topic_slug = topic,
    census = m[, 2],
    edition = as.integer(m[, 3]),
    # CNGE / CNGSPSPE edition year is usually one year after the reference year in the tables
    year = as.integer(m[, 3]) - 1L
  )
}

# topic_patterns: character vector of folder topic slug pieces to keep
# (matched against the parsed topic_slug).
find_topic_packages <- function(raw_dir, topic_patterns) {
  dirs <- list.dirs(raw_dir, full.names = TRUE, recursive = FALSE)
  rows <- list()
  for (d in dirs) {
    parsed <- parse_package_folder(basename(d))
    if (is.null(parsed)) {
      next
    }
    hit <- any(vapply(
      topic_patterns,
      function(pat) grepl(pat, parsed$topic_slug, ignore.case = TRUE),
      logical(1)
    ))
    if (!hit) {
      next
    }
    data_dir <- file.path(d, "conjunto_de_datos")
    dict_dir <- file.path(d, "diccionario_de_datos")
    rows[[length(rows) + 1]] <- tibble::tibble(
      path = d,
      folder = basename(d),
      topic_slug = parsed$topic_slug,
      census = parsed$census,
      edition = parsed$edition,
      year = parsed$year,
      has_data = dir.exists(data_dir),
      data_dir = data_dir,
      dict_dir = dict_dir
    )
  }
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) {
    return(out)
  }
  
  # Prefer a clean folder name when macOS left " (1)" duplicates for the same edition.
  out %>%
    dplyr::mutate(is_duplicate_name = grepl(" \\([0-9]+\\)$", folder)) %>%
    dplyr::group_by(topic_slug, edition) %>%
    dplyr::arrange(is_duplicate_name, folder, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic_slug, edition)
}

read_package_index <- function(pkg_row) {
  idx <- list.files(pkg_row$data_dir, pattern = "^0_indice.*\\.csv$", full.names = TRUE)
  if (length(idx) == 0) {
    return(tibble::tibble(
      edition = pkg_row$edition,
      year = pkg_row$year,
      archivo = NA_character_,
      contenido = "No index file found"
    ))
  }
  read_csv_flexible(idx[[1]]) %>%
    dplyr::mutate(
      edition = pkg_row$edition,
      year = pkg_row$year
    )
}

list_data_tables <- function(pkg_row) {
  files <- list.files(pkg_row$data_dir, pattern = "\\.csv$", full.names = FALSE)
  files <- files[!grepl("^0_indice", files)]
  tibble::tibble(
    edition = pkg_row$edition,
    year = pkg_row$year,
    table_file = files
  )
}

peek_table <- function(pkg_row, table_file, n = 5) {
  path <- file.path(pkg_row$data_dir, table_file)
  df <- read_csv_flexible(path, n_max = n)
  list(
    path = path,
    n_cols = ncol(df),
    columns = names(df),
    head = df
  )
}

read_full_table <- function(pkg_row, table_file) {
  read_csv_flexible(
    file.path(pkg_row$data_dir, table_file),
    col_types = readr::cols(.default = "c")
  )
}

# Keep GoC states when a cve_ent column exists.
filter_goc <- function(df) {
  if (!"cve_ent" %in% names(df)) {
    return(df)
  }
  df %>%
    dplyr::mutate(cve_ent = stringr::str_pad(as.character(cve_ent), 2, pad = "0")) %>%
    dplyr::filter(cve_ent %in% goc_state_codes) %>%
    dplyr::mutate(estado = unname(goc_state_names[cve_ent]))
}

summarize_yes_no_cols <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) {
    return(tibble::tibble(note = "No candidate yes or no columns found in this table"))
  }
  purrr::map_dfr(cols, function(col) {
    df %>%
      dplyr::count(value = .data[[col]], name = "n") %>%
      dplyr::mutate(column = col, .before = 1)
  })
}
