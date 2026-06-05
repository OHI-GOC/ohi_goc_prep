# BigQuery dry-run helpers for Mexican VMS (mex-fisheries.mex_vms).
# Requires package: bigrquery (loaded by caller).

#' Dry-run a query and return estimated bytes processed (NA if unknown).
bq_dry_run_bytes <- function(
  query,
  billing_project,
  default_dataset = bq_dataset("mex-fisheries", "mex_vms")
) {
  ## dbplyr SQL often uses unqualified table names; dry-run needs default_dataset (unlike collect() via con).
  x <- bq_perform_query_dry_run(
    query,
    billing = billing_project,
    default_dataset = default_dataset
  )
  if (inherits(x, "bq_job")) {
    meta <- bq_job_meta(x)
    qstat <- meta$statistics$query
    raw <- if (is.null(qstat)) NULL else qstat$totalBytesProcessed
    return(if (is.null(raw)) NA_real_ else as.numeric(raw))
  }
  as.numeric(x)
}

#' Print human-readable dry-run summary (informational; free tier is approximate).
bq_print_dry_run <- function(
  query,
  billing_project,
  label = "Query",
  default_dataset = bq_dataset("mex-fisheries", "mex_vms")
) {
  bytes <- bq_dry_run_bytes(query, billing_project, default_dataset = default_dataset)
  if (is.na(bytes)) {
    message(label, ": dry run did not return totalBytesProcessed (inspect job metadata).")
    return(invisible(bytes))
  }
  gib <- bytes / 1024^3
  free_tier_bytes <- 1024^4
  pct_free <- 100 * bytes / free_tier_bytes
  usd_tb <- 6.25
  usd_est <- (bytes / 1e12) * usd_tb
  message(
    label, " — estimated bytes processed: ",
    prettyNum(bytes, big.mark = ","), " (~",
    format(round(gib, 2), nsmall = 2), " GiB)\n",
    "Approx. share of a 1 TiB monthly free-analysis allowance (if your account has it): ",
    format(round(pct_free, 3), nsmall = 3), "%\n",
    "Order-of-magnitude on-demand cost if not free: ~$",
    format(round(usd_est, 4), nsmall = 4), " at ~$", usd_tb, "/TB scanned (list price; region/account may differ)."
  )
  invisible(bytes)
}

#' Build a fully-qualified BigQuery table id for SQL strings.
fully_qualified_vms_table <- function(table) {
  paste0("`mex-fisheries.mex_vms.", table, "`")
}
