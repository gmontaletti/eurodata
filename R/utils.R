# Internal helper functions for eurodata
#
# All functions in this file are prefixed with `.` and are NOT exported.
# They implement the low-level transformation steps used by `ed_transform()`.

# Suppress R CMD check notes for data.table NSE variables
utils::globalVariables(c(
  ".",
  ".N",
  ":=",
  "..cols",
  "values",
  "iCode",
  "iCode2",
  "indice",
  "gruppo",
  "area",
  "geo",
  "TIME_PERIOD",
  "paese",
  "uName",
  "uCode",
  "pop",
  "PPS",
  "N"
))

# 1. apply_spec -----

#' Apply a full spec transformation to a single dataset
#'
#' Main orchestrator that sequences all transformation steps according to
#' the fields present in a spec list.
#'
#' @param dt A data.frame or data.table with labelled Eurostat data.
#' @param spec A list describing the transformation (see `dataset_specs`).
#' @return A data.table with the standard schema:
#'   indice, gruppo, iCode, area, geo, TIME_PERIOD, values.
#' @noRd
.apply_spec <- function(dt, spec) {
  # work on a copy so the caller's data is never modified

  dt <- data.table::copy(data.table::as.data.table(dt))

  # optional substring transform (must run before filters that may depend on
  # the transformed column)
  if (!is.null(spec$substring_transform)) {
    dt <- .apply_substring(dt, spec$substring_transform)
  }

  # exact-match filters
  if (!is.null(spec$filters)) {
    dt <- .apply_filters(dt, spec$filters)
  }

  # inclusion filters (%in%)
  if (!is.null(spec$filter_in)) {
    dt <- .apply_filter_in(dt, spec$filter_in)
  }

  # exclusion filters (!%in%)
  if (!is.null(spec$filter_not_in)) {
    dt <- .apply_filter_not_in(dt, spec$filter_not_in)
  }

  # remap column values via case_match
  if (!is.null(spec$remap)) {
    dt <- .apply_remap(dt, spec$remap)
  }

  # remove rows with NA values if requested
  if (isTRUE(spec$filter_na)) {
    dt <- dt[!is.na(values)]
  }

  # add indicator metadata columns
  data.table::set(dt, j = "indice", value = spec$indice)
  data.table::set(dt, j = "iCode", value = spec$iCode)

  # handle gruppo: literal string or rename from an existing column
  if (!is.null(spec$gruppo_literal)) {
    data.table::set(dt, j = "gruppo", value = spec$gruppo_literal)
  } else if (!is.null(spec$gruppo_col)) {
    data.table::setnames(dt, old = spec$gruppo_col, new = "gruppo")
  }

  # handle area: either copy geo or paste paese + regione
  if (!is.null(spec$area_formula) && spec$area_formula == "geo") {
    data.table::set(dt, j = "area", value = dt[["geo"]])
  } else {
    data.table::set(
      dt,
      j = "area",
      value = paste(dt[["paese"]], dt[["regione"]])
    )
  }

  # aggregate (mean) when the spec requests it (e.g. quarterly -> annual)
  if (isTRUE(spec$aggregate)) {
    dt <- dt[,
      .(values = mean(values, na.rm = TRUE), iCode = iCode[1L]),
      by = .(indice, gruppo, area, geo, TIME_PERIOD)
    ]
  }

  # select final columns in canonical order
  cols <- c("indice", "gruppo", "iCode", "area", "geo", "TIME_PERIOD", "values")
  dt <- dt[, ..cols]

  dt
}


# 2. apply_filters -----

#' Filter rows by exact column == value matches
#'
#' @param dt A data.table.
#' @param filters A named list where names are column names and values are
#'   the required value for that column.
#' @return Filtered data.table.
#' @noRd
.apply_filters <- function(dt, filters) {
  for (col_name in names(filters)) {
    dt <- dt[dt[[col_name]] == filters[[col_name]], ]
  }
  dt
}


# 3. apply_filter_in -----

#' Filter rows where column values are within a set
#'
#' @param dt A data.table.
#' @param filter_in A named list where names are column names and values are
#'   character vectors of allowed values.
#' @return Filtered data.table.
#' @noRd
.apply_filter_in <- function(dt, filter_in) {
  for (col_name in names(filter_in)) {
    dt <- dt[dt[[col_name]] %in% filter_in[[col_name]], ]
  }
  dt
}


# 4. apply_filter_not_in -----

#' Filter rows where column values are NOT in a set
#'
#' @param dt A data.table.
#' @param filter_not_in A named list where names are column names and values
#'   are character vectors of excluded values.
#' @return Filtered data.table.
#' @noRd
.apply_filter_not_in <- function(dt, filter_not_in) {
  for (col_name in names(filter_not_in)) {
    dt <- dt[!dt[[col_name]] %in% filter_not_in[[col_name]], ]
  }
  dt
}


# 5. apply_remap -----

#' Remap values in a column using case_match
#'
#' @param dt A data.table.
#' @param remap A list with elements `col` (column name) and `mapping`
#'   (a named list where names are old values and values are new values).
#'   Unmatched values are kept as-is (`.default = current value`).
#' @return data.table with the remapped column.
#' @noRd
.apply_remap <- function(dt, remap) {
  col_name <- remap$col
  mapping <- remap$mapping

  old_vals <- names(mapping)
  new_vals <- unlist(mapping, use.names = FALSE)
  original <- dt[[col_name]]

  # Build the remap dynamically so that the number of old -> new
  # pairs is not hard-coded.
  matched <- original
  for (i in seq_along(old_vals)) {
    matched[original == old_vals[i]] <- new_vals[i]
  }

  data.table::set(dt, j = col_name, value = matched)
  dt
}


# 6. apply_substring -----

#' Substring-transform a column in place
#'
#' @param dt A data.table.
#' @param transforms A list with `col`, `start`, and `stop`.
#' @return data.table with the transformed column.
#' @noRd
.apply_substring <- function(dt, transforms) {
  col_name <- transforms$col
  data.table::set(
    dt,
    j = col_name,
    value = substring(dt[[col_name]], transforms$start, transforms$stop)
  )
  dt
}


# 7. make_icode2 -----

#' Compute composite indicator code from iCode and gruppo
#'
#' Strips certain words and punctuation from `gruppo`, lowercases,
#' takes the first 10 characters, and pastes onto `iCode`.
#'
#' @param iCode Character vector of indicator codes.
#' @param gruppo Character vector of group labels.
#' @return Character vector of composite codes.
#' @noRd
.make_icode2 <- function(iCode, gruppo) {
  cleaned <- gsub("persons|with|\\,| |-", "", gruppo)
  paste0(iCode, tolower(substring(cleaned, 1L, 10L)))
}
