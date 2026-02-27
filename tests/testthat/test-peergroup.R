# Unit tests for ed_peergroup() (R/peergroup.R).

# 1. Helper to build synthetic unified data -----

# Creates a data.table mimicking ed_transform() output, with population, PPS,
# and additional indicator rows for a set of fake regions across two years
# (comparison year and target year).
.make_peer_data <- function() {
  regions <- data.table::data.table(
    uCode = c("ITH3", "ITC4", "DE21", "ES51", "FR10"),
    uName = c(
      "IT Veneto",
      "IT Lombardia",
      "DE Bayern",
      "ES Cataluna",
      "FR Ile-de-France"
    ),
    pop = c(5000, 10000, 13000, 7500, 12000),
    pps = c(120, 130, 150, 95, 170)
  )

  rows <- list()

  for (yr in c(2022, 2024)) {
    for (i in seq_len(nrow(regions))) {
      r <- regions[i]

      # Population indicator
      rows[[length(rows) + 1L]] <- data.table::data.table(
        iCode = "Popolaziototal",
        iName = "Popolazione 1 gennaio Total",
        uCode = r$uCode,
        uName = r$uName,
        TIME_PERIOD = yr,
        values = r$pop + (yr - 2022) * 50
      )

      # PPS indicator
      rows[[length(rows) + 1L]] <- data.table::data.table(
        iCode = "PPSprcptPpurchasing",
        iName = "PPS procapite indice purchasing",
        uCode = r$uCode,
        uName = r$uName,
        TIME_PERIOD = yr,
        values = r$pps + (yr - 2022) * 2
      )

      # Extra indicator A
      rows[[length(rows) + 1L]] <- data.table::data.table(
        iCode = "EmpRatetotal",
        iName = "Employment rate Total",
        uCode = r$uCode,
        uName = r$uName,
        TIME_PERIOD = yr,
        values = 60 + i * 3 + (yr - 2022)
      )

      # Extra indicator B
      rows[[length(rows) + 1L]] <- data.table::data.table(
        iCode = "DisTasstotal",
        iName = "Tasso di disoccupazione Total",
        uCode = r$uCode,
        uName = r$uName,
        TIME_PERIOD = yr,
        values = 10 - i + (yr - 2022) * 0.5
      )
    }
  }

  data.table::rbindlist(rows)
}


# 2. Basic peer group selection -----

test_that("ed_peergroup selects comparable regions and returns correct schema", {
  dt <- .make_peer_data()

  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  expected_cols <- c(
    "iCode",
    "iName",
    "uCode",
    "uName",
    "TIME_PERIOD",
    "values"
  )
  expect_equal(names(result), expected_cols)

  # All returned rows should be for target_year 2024
  expect_true(all(result$TIME_PERIOD == 2024))

  # Reference region (IT Veneto) should be in the result because it should
  # pass its own comparison thresholds (pop > pop*0.3, PPS within bounds)
  expect_true("IT Veneto" %in% result$uName)
})

test_that("ed_peergroup excludes regions below population threshold", {
  dt <- .make_peer_data()

  # With pop_ratio = 0.99, only regions with pop > 5000 * 0.99 = 4950 qualify.
  # All five regions have pop >= 5000, so all should pass.
  result_loose <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      pop_ratio = 0.99,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  # With pop_ratio = 0.3 (default), regions need pop > 5000 * 0.3 = 1500,
  # so all five qualify.
  result_default <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  # Both should include the reference region
  expect_true("IT Veneto" %in% result_loose$uName)
  expect_true("IT Veneto" %in% result_default$uName)
})

test_that("ed_peergroup excludes regions above PPS upper bound", {
  dt <- .make_peer_data()

  # FR Ile-de-France has PPS = 170. Setting pps_upper = 160 should exclude it.
  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      pps_upper = 160,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  expect_false("FR Ile-de-France" %in% result$uName)
})

test_that("ed_peergroup excludes regions below PPS low ratio", {
  dt <- .make_peer_data()

  # IT Veneto PPS = 120. With pps_low_ratio = 0.9, lower bound is 108.
  # ES Cataluna has PPS = 95, which is below 108.
  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      pps_low_ratio = 0.9,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  expect_false("ES Cataluna" %in% result$uName)
})


# 3. exclude_incomplete parameter -----

test_that("ed_peergroup drops indicators with too many missing values", {
  dt <- .make_peer_data()

  # Add an indicator with mostly missing values in the target year
  sparse_rows <- data.table::data.table(
    iCode = rep("SparseInd", 5),
    iName = rep("Sparse indicator", 5),
    uCode = c("ITH3", "ITC4", "DE21", "ES51", "FR10"),
    uName = c(
      "IT Veneto",
      "IT Lombardia",
      "DE Bayern",
      "ES Cataluna",
      "FR Ile-de-France"
    ),
    TIME_PERIOD = rep(2024, 5),
    values = c(1, NA, NA, NA, NA)
  )
  dt <- data.table::rbindlist(list(dt, sparse_rows))

  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      target_year = 2024,
      exclude_incomplete = TRUE,
      min_nonmissing = 3
    )
  )

  # SparseInd has only 1 non-missing value, below threshold of 3
  expect_false("SparseInd" %in% result$iCode)
  # Other indicators should still be present
  expect_true("EmpRatetotal" %in% result$iCode)
})

test_that("ed_peergroup keeps all indicators when exclude_incomplete = FALSE", {
  dt <- .make_peer_data()

  sparse_rows <- data.table::data.table(
    iCode = rep("SparseInd", 5),
    iName = rep("Sparse indicator", 5),
    uCode = c("ITH3", "ITC4", "DE21", "ES51", "FR10"),
    uName = c(
      "IT Veneto",
      "IT Lombardia",
      "DE Bayern",
      "ES Cataluna",
      "FR Ile-de-France"
    ),
    TIME_PERIOD = rep(2024, 5),
    values = c(1, NA, NA, NA, NA)
  )
  dt <- data.table::rbindlist(list(dt, sparse_rows))

  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  expect_true("SparseInd" %in% result$iCode)
})


# 4. Edge cases -----

test_that("ed_peergroup errors when reference region is not found", {
  dt <- .make_peer_data()

  expect_error(
    suppressMessages(
      ed_peergroup(
        dt,
        reference_region = "XX Nonexistent",
        year = 2022
      )
    ),
    "Reference region.*not found"
  )
})

test_that("ed_peergroup warns when no regions meet peer group criteria", {
  dt <- .make_peer_data()

  # pps_upper = 1 is impossibly low: no region can have PPS <= 1
  expect_warning(
    suppressMessages(
      ed_peergroup(
        dt,
        reference_region = "IT Veneto",
        year = 2022,
        pps_upper = 1
      )
    ),
    "No regions meet the peer group criteria"
  )
})

test_that("ed_peergroup returns empty data.table when no peers found", {
  dt <- .make_peer_data()

  result <- suppressWarnings(suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      pps_upper = 1
    )
  ))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_equal(
    names(result),
    c("iCode", "iName", "uCode", "uName", "TIME_PERIOD", "values")
  )
})

test_that("ed_peergroup errors when required iCodes are missing from data", {
  dt <- .make_peer_data()
  dt <- dt[!iCode %in% "Popolaziototal"]

  expect_error(
    suppressMessages(
      ed_peergroup(dt, reference_region = "IT Veneto", year = 2022)
    ),
    "iCode values are not present"
  )
})

test_that("ed_peergroup errors when reference region has NA PPS or population", {
  dt <- .make_peer_data()

  # Set Veneto population to NA for year 2022
  dt[
    uName == "IT Veneto" & iCode == "Popolaziototal" & TIME_PERIOD == 2022,
    values := NA
  ]

  expect_error(
    suppressMessages(
      ed_peergroup(dt, reference_region = "IT Veneto", year = 2022)
    ),
    "missing PPS or population"
  )
})


# 5. Custom parameters -----

test_that("ed_peergroup works with a different reference region", {
  dt <- .make_peer_data()

  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "DE Bayern",
      year = 2022,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  # DE Bayern should be in its own peer group
  expect_true("DE Bayern" %in% result$uName)
  expect_true(all(result$TIME_PERIOD == 2024))
})

test_that("ed_peergroup respects custom pop_ratio and pps thresholds", {
  dt <- .make_peer_data()

  # Very strict: pop_ratio = 0.99 means peer pop must exceed 99% of reference.
  # Reference (Veneto) pop = 5000, so peers need pop > 4950.
  # All regions have pop >= 5000, so all pass the population filter.
  # pps_low_ratio = 0.95 means PPS >= 120 * 0.95 = 114.
  # ES Cataluna PPS = 95, below 114 => excluded.
  # pps_upper = 155 => DE Bayern PPS = 150 passes, FR Ile-de-France PPS = 170 excluded.
  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      pop_ratio = 0.99,
      pps_low_ratio = 0.95,
      pps_upper = 155,
      target_year = 2024,
      exclude_incomplete = FALSE
    )
  )

  peer_names <- unique(result$uName)
  expect_false("ES Cataluna" %in% peer_names)
  expect_false("FR Ile-de-France" %in% peer_names)
  expect_true("DE Bayern" %in% peer_names)
})

test_that("ed_peergroup uses automatic min_nonmissing when NULL", {
  dt <- .make_peer_data()

  # With default parameters, 5 regions qualify as peers.
  # min_nonmissing = floor(5 / 2) = 2.
  # Add an indicator with exactly 1 non-missing value in target year.
  sparse_rows <- data.table::data.table(
    iCode = rep("AlmostEmpty", 5),
    iName = rep("Almost empty indicator", 5),
    uCode = c("ITH3", "ITC4", "DE21", "ES51", "FR10"),
    uName = c(
      "IT Veneto",
      "IT Lombardia",
      "DE Bayern",
      "ES Cataluna",
      "FR Ile-de-France"
    ),
    TIME_PERIOD = rep(2024, 5),
    values = c(42, NA, NA, NA, NA)
  )
  dt <- data.table::rbindlist(list(dt, sparse_rows))

  result <- suppressMessages(
    ed_peergroup(
      dt,
      reference_region = "IT Veneto",
      year = 2022,
      target_year = 2024,
      exclude_incomplete = TRUE,
      min_nonmissing = NULL
    )
  )

  # AlmostEmpty has 1 non-missing, threshold is floor(n_peers / 2) >= 2
  expect_false("AlmostEmpty" %in% result$iCode)
})


# 6. Input validation -----

test_that("ed_peergroup rejects invalid inputs", {
  dt <- .make_peer_data()

  expect_error(
    ed_peergroup("not a data.frame"),
    "must be a data.frame"
  )

  bad_dt <- data.table::data.table(wrong_col = 1)
  expect_error(
    ed_peergroup(bad_dt),
    "must contain columns"
  )

  expect_error(
    ed_peergroup(dt, reference_region = 123),
    "must be a single character"
  )

  expect_error(
    ed_peergroup(dt, pop_ratio = 1.5),
    "must be a single numeric between 0 and 1"
  )

  expect_error(
    ed_peergroup(dt, pps_upper = -10),
    "must be a single positive numeric"
  )
})
