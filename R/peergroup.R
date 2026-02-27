#' Select a peer group of regions comparable to a reference region
#'
#' Identifies NUTS-2 regions that are comparable to a reference region based
#' on population size and GDP per capita in purchasing power standards (PPS).
#' The function filters the full indicator dataset to keep only the selected
#' peer group for the target year, and optionally removes indicators that have
#' too many missing values.
#'
#' @details
#' The peer group selection proceeds as follows:
#' 1. Extract population and PPS values for the comparison year.
#' 2. Pivot to wide format so each region has one PPS and one population value.
#' 3. Retrieve the reference region's PPS and population.
#' 4. Keep regions where population exceeds a fraction of the reference
#'    region's population AND PPS falls between a lower bound and an absolute
#'    upper bound.
#' 5. Filter the original dataset to the peer group regions and the target year.
#' 6. Optionally drop indicators with insufficient non-missing observations.
#'
#' @param dt A `data.table` with the unified eurodata schema. Expected
#'   columns: `iCode`, `iName`, `uCode`, `uName`, `TIME_PERIOD`, `values`.
#'   Typically the output of [ed_transform()].
#' @param reference_region Character string identifying the reference region
#'   in the `uName` column. Defaults to `"IT Veneto"`.
#' @param year Integer. The year used to compute PPS and population for the
#'   peer group comparison. Defaults to `2022`.
#' @param pop_icode Character string. The `iCode` value that identifies
#'   the population indicator. Defaults to `"Popolaziototal"`.
#' @param pps_icode Character string. The `iCode` value that identifies
#'   the PPS per capita indicator. Defaults to `"PPSprcptPpurchasing"`.
#' @param pop_ratio Numeric between 0 and 1. Minimum ratio of the reference
#'   region's population that a peer region must exceed. Defaults to `0.3`.
#' @param pps_low_ratio Numeric between 0 and 1. Lower bound for PPS
#'   expressed as a fraction of the reference region's PPS. Defaults to `0.67`.
#' @param pps_upper Numeric. Absolute upper bound for PPS. Regions with PPS
#'   above this value are excluded. Defaults to `200`.
#' @param target_year Integer. The year for which the final indicator data
#'   is returned. Defaults to `2024`.
#' @param exclude_incomplete Logical. If `TRUE`, indicators with fewer
#'   non-missing observations than `min_nonmissing` are dropped. Defaults to
#'   `TRUE`.
#' @param min_nonmissing Integer or `NULL`. Minimum number of non-missing
#'   values an indicator must have to be retained. If `NULL` (the default),
#'   the threshold is set to half the number of regions in the peer group.
#'
#' @return A `data.table` with the same columns as `dt`, filtered to the peer
#'   group regions for the target year. Returned invisibly.
#'
#' @export
#'
#' @importFrom data.table dcast data.table setnames
#'
#' @examples
#' \dontrun{
#' peer <- ed_peergroup(pgr)
#'
#' peer <- ed_peergroup(
#'   pgr,
#'   reference_region = "DE Bayern",
#'   pop_ratio = 0.5,
#'   pps_low_ratio = 0.75,
#'   target_year = 2023
#' )
#' }
ed_peergroup <- function(
  dt,
  reference_region = "IT Veneto",
  year = 2022,
  pop_icode = "Popolaziototal",
  pps_icode = "PPSprcptPpurchasing",
  pop_ratio = 0.3,
  pps_low_ratio = 0.67,
  pps_upper = 200,
  target_year = 2024,
  exclude_incomplete = TRUE,
  min_nonmissing = NULL
) {
  # 1. Input validation -----
  stopifnot(
    "`dt` must be a data.frame or data.table" = is.data.frame(dt),
    "`dt` must contain columns: iCode, iName, uCode, uName, TIME_PERIOD, values" = all(
      c("iCode", "iName", "uCode", "uName", "TIME_PERIOD", "values") %in%
        names(dt)
    ),
    "`reference_region` must be a single character string" = is.character(
      reference_region
    ) &&
      length(reference_region) == 1L,
    "`year` must be a single numeric value" = is.numeric(year) &&
      length(year) == 1L,
    "`pop_icode` must be a single character string" = is.character(pop_icode) &&
      length(pop_icode) == 1L,
    "`pps_icode` must be a single character string" = is.character(pps_icode) &&
      length(pps_icode) == 1L,
    "`pop_ratio` must be a single numeric between 0 and 1" = is.numeric(
      pop_ratio
    ) &&
      length(pop_ratio) == 1L &&
      pop_ratio >= 0 &&
      pop_ratio <= 1,
    "`pps_low_ratio` must be a single numeric between 0 and 1" = is.numeric(
      pps_low_ratio
    ) &&
      length(pps_low_ratio) == 1L &&
      pps_low_ratio >= 0 &&
      pps_low_ratio <= 1,
    "`pps_upper` must be a single positive numeric" = is.numeric(pps_upper) &&
      length(pps_upper) == 1L &&
      pps_upper > 0,
    "`target_year` must be a single numeric value" = is.numeric(target_year) &&
      length(target_year) == 1L,
    "`exclude_incomplete` must be TRUE or FALSE" = is.logical(
      exclude_incomplete
    ) &&
      length(exclude_incomplete) == 1L,
    "`min_nonmissing` must be NULL or a single positive integer" = is.null(
      min_nonmissing
    ) ||
      (is.numeric(min_nonmissing) &&
        length(min_nonmissing) == 1L &&
        min_nonmissing > 0)
  )

  dt <- data.table::as.data.table(dt)

  # 2. Extract comparison indicators for the reference year -----
  comparison_codes <- c(pop_icode, pps_icode)
  missing_codes <- setdiff(comparison_codes, unique(dt$iCode))
  if (length(missing_codes) > 0L) {
    stop(
      "The following iCode values are not present in `dt`: ",
      paste(missing_codes, collapse = ", "),
      call. = FALSE
    )
  }

  toplot <- dt[iCode %in% comparison_codes & TIME_PERIOD == year]

  if (nrow(toplot) == 0L) {
    stop(
      "No rows found for iCode in c(\"",
      pop_icode,
      "\", \"",
      pps_icode,
      "\") with TIME_PERIOD == ",
      year,
      ". ",
      "Check the `year`, `pop_icode`, and `pps_icode` arguments.",
      call. = FALSE
    )
  }

  # 3. Pivot to wide format -----
  tpp <- data.table::dcast(
    toplot,
    uCode + uName ~ iCode,
    value.var = "values"
  )
  data.table::setnames(
    tpp,
    old = c("uCode", "uName", pps_icode, pop_icode),
    new = c("uCode", "uName", "PPS", "pop")
  )

  # 4. Retrieve reference region values -----
  ref_row <- tpp[uName == reference_region]
  if (nrow(ref_row) == 0L) {
    stop(
      "Reference region \"",
      reference_region,
      "\" not found in the data for year ",
      year,
      ". ",
      "Available uName values include: ",
      paste(utils::head(sort(unique(tpp$uName)), 10L), collapse = ", "),
      ", ...",
      call. = FALSE
    )
  }

  ref_pps <- ref_row$PPS[1L]
  ref_pop <- ref_row$pop[1L]

  if (is.na(ref_pps) || is.na(ref_pop)) {
    stop(
      "Reference region \"",
      reference_region,
      "\" has missing PPS or population data for year ",
      year,
      ".",
      call. = FALSE
    )
  }

  message(
    "Reference region: ",
    reference_region,
    " (PPS = ",
    round(ref_pps, 1),
    ", pop = ",
    round(ref_pop, 0),
    ")"
  )

  # 5. Select peer group -----
  peer <- tpp[
    pop > ref_pop * pop_ratio &
      PPS >= ref_pps * pps_low_ratio &
      PPS <= pps_upper
  ]

  n_peers <- nrow(peer)
  if (n_peers == 0L) {
    warning(
      "No regions meet the peer group criteria. ",
      "Consider relaxing `pop_ratio`, `pps_low_ratio`, or `pps_upper`.",
      call. = FALSE
    )
    return(data.table::data.table(
      iCode = character(0L),
      iName = character(0L),
      uCode = character(0L),
      uName = character(0L),
      TIME_PERIOD = numeric(0L),
      values = numeric(0L)
    ))
  }

  peer_names <- peer$uName
  message("Peer group: ", n_peers, " regions selected")

  # 6. Filter to peer group and target year -----
  result <- dt[uName %in% peer_names & TIME_PERIOD == target_year]

  if (nrow(result) == 0L) {
    warning(
      "No data for the peer group in target_year = ",
      target_year,
      ". ",
      "The peer group was selected using year = ",
      year,
      ".",
      call. = FALSE
    )
    return(result)
  }

  # 7. Exclude incomplete indicators -----
  if (isTRUE(exclude_incomplete)) {
    if (is.null(min_nonmissing)) {
      min_nonmissing <- floor(n_peers / 2)
    }

    completeness <- result[!is.na(values), .N, by = iCode]
    incomplete_codes <- completeness[N < min_nonmissing, iCode]

    if (length(incomplete_codes) > 0L) {
      message(
        "Excluding ",
        length(incomplete_codes),
        " indicator(s) with fewer than ",
        min_nonmissing,
        " non-missing values: ",
        paste(incomplete_codes, collapse = ", ")
      )
      result <- result[!iCode %in% incomplete_codes]
    }
  }

  message(
    "Result: ",
    data.table::uniqueN(result$iCode),
    " indicators, ",
    data.table::uniqueN(result$uName),
    " regions, ",
    nrow(result),
    " rows"
  )

  invisible(result)
}
