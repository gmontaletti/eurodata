# _targets.R -- eurodata pipeline
# Run with targets::tar_make()

library(targets)
library(tarchetypes)

# 1. Configuration -----
TARGET_YEAR <- 2024L
COMPARE_YEAR <- 2022L
REF_REGION <- "IT Veneto"
REFRESH_DAYS <- 7L
OUT_DIR_RDS <- "dati"
OUT_DIR_XLSX <- "xlsx"

# Dataset IDs from the package specs
DATASET_IDS <- names(eurodata::dataset_specs)

# 2. Target options -----
tar_option_set(
  packages = "eurodata",
  format = "qs"
)

# 3. Pipeline -----
list(
  # Collect raw data from Eurostat (re-fetch after REFRESH_DAYS)
  tarchetypes::tar_age(
    raw_data,
    ed_collect(DATASET_IDS),
    age = as.difftime(REFRESH_DAYS, units = "days")
  ),

  # Transform into unified schema
  tar_target(
    unified,
    ed_transform(raw_data)
  ),

  # Export all years to RDS
  tar_target(
    export_all_years,
    {
      path <- file.path(OUT_DIR_RDS, "indici_all_years")
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      ed_export(unified, path, "rds")
      paste0(path, ".rds")
    },
    format = "file"
  ),

  # Select peer group
  tar_target(
    peer_group,
    ed_peergroup(
      unified,
      reference_region = REF_REGION,
      year = COMPARE_YEAR,
      target_year = TARGET_YEAR
    )
  ),

  # Export peer group to RDS
  tar_target(
    export_peer_rds,
    {
      path <- file.path(OUT_DIR_RDS, paste0("indici_", TARGET_YEAR))
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      ed_export(peer_group, path, "rds")
      paste0(path, ".rds")
    },
    format = "file"
  ),

  # Export peer group to Excel
  tar_target(
    export_peer_xlsx,
    {
      path <- file.path(OUT_DIR_XLSX, paste0("indici_", TARGET_YEAR))
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      ed_export(peer_group, path, "xlsx")
      paste0(path, ".xlsx")
    },
    format = "file"
  )
)
