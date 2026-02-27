#' Transform and export Eurostat datasets into the eurodata unified schema
#'
#' Exported functions for applying dataset specs to raw Eurostat data and
#' writing the results to disk.

# 1. ed_transform -----

#' Transform raw Eurostat datasets into a unified indicator table
#'
#' Iterates over the transformation specifications in `specs`, looks up each
#' corresponding dataset in `data_list`, applies the full transformation
#' pipeline (filtering, remapping, aggregation), and combines all results
#' into a single `data.table` with the eurodata schema.
#'
#' @param data_list A named list of data.frames or data.tables, as returned
#'   by the collection step (e.g. `readRDS("EURO_indicatori.rds")`). Names
#'   must correspond to Eurostat dataset identifiers.
#' @param specs A list of spec lists describing the transformations. Defaults
#'   to `eurodata::dataset_specs` defined in `R/config.R`.
#'
#' @return A `data.table` with columns:
#'   \describe{
#'     \item{iCode}{Composite indicator code (iCode + abbreviated gruppo).}
#'     \item{iName}{Human-readable indicator name (indice + gruppo).}
#'     \item{uCode}{NUTS-2 geo code.}
#'     \item{uName}{Region label (paese + regione or geo).}
#'     \item{TIME_PERIOD}{Reference year.}
#'     \item{values}{Numeric indicator value.}
#'   }
#'
#' @examples
#' \dontrun{
#' raw <- readRDS("../datasets/EURO_indicatori.rds")
#' unified <- ed_transform(raw)
#' }
#'
#' @importFrom data.table rbindlist
#' @export
ed_transform <- function(data_list, specs = eurodata::dataset_specs) {
  stopifnot(
    "data_list must be a named list" = is.list(data_list) &&
      !is.null(names(data_list)),
    "specs must be a list" = is.list(specs)
  )

  # apply each spec, collecting transformed data.tables
  results <- vector("list", length(specs))

  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    ds_id <- spec$dataset_id

    if (!ds_id %in% names(data_list)) {
      message("Skipping spec '", ds_id, "': not found in data_list")
      next
    }

    results[[i]] <- .apply_spec(data_list[[ds_id]], spec)
  }

  # drop NULL entries from skipped specs
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0L) {
    stop("No specs matched any dataset in data_list. Check dataset names.")
  }

  # combine all transformed tables
  dt <- data.table::rbindlist(results, fill = TRUE, use.names = TRUE)

  # post-processing: remove uninformative gruppo values
  dt <- dt[!gruppo %in% c("No response", "Unknown")]

  # compute composite iCode from iCode + abbreviated gruppo
  dt[, iCode2 := .make_icode2(iCode, gruppo)]

  # reshape to final schema
  dt <- dt[, .(
    iCode = iCode2,
    iName = paste(indice, gruppo),
    uCode = geo,
    uName = area,
    TIME_PERIOD = TIME_PERIOD,
    values = values
  )]

  # remove UK regions (no longer in EU27)
  dt <- dt[!startsWith(uName, "UK")]

  dt
}


# 2. ed_export -----

#' Export a data.table to RDS and/or Excel
#'
#' Writes the unified indicator table to disk in one or both formats.
#' The file extension is appended automatically based on `format`.
#'
#' @param dt A `data.table` (or data.frame) to export.
#' @param path File path **without extension**. The appropriate suffix
#'   (`.rds` or `.xlsx`) is added by the function.
#' @param format One of `"rds"`, `"xlsx"`, or `"both"`. Defaults to `"rds"`.
#'
#' @return `dt`, returned invisibly for use in pipelines.
#'
#' @examples
#' \dontrun{
#' unified <- ed_transform(raw)
#' ed_export(unified, "dati/indici_2024", format = "both")
#' }
#'
#' @export
ed_export <- function(dt, path, format = c("rds", "xlsx", "both")) {
  format <- match.arg(format)

  stopifnot(
    "dt must be a data.frame or data.table" = is.data.frame(dt),
    "path must be a single character string" = is.character(path) &&
      length(path) == 1L
  )

  if (format %in% c("rds", "both")) {
    saveRDS(dt, paste0(path, ".rds"))
  }

  if (format %in% c("xlsx", "both")) {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop(
        "Package 'writexl' is required for xlsx export. ",
        "Install it with install.packages('writexl')."
      )
    }
    writexl::write_xlsx(dt, paste0(path, ".xlsx"))
  }

  invisible(dt)
}
