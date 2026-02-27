# Default EU27 country codes (Eurostat 2-character codes)
.eu27_codes <- c(
  "AT",
  "BE",
  "BG",
  "CY",
  "CZ",
  "DE",
  "DK",
  "EE",
  "EL",
  "ES",
  "FI",
  "FR",
  "HR",
  "HU",
  "IE",
  "IT",
  "LT",
  "LU",
  "LV",
  "MT",
  "NL",
  "PL",
  "PT",
  "RO",
  "SE",
  "SI",
  "SK"
)


#' Build a country/region lookup table from a raw Eurostat data.table
#'
#' Extracts the 2-character country code from \code{geo}, creates a unique
#' geo/values/time frame, labels it with \code{eurostat::label_eurostat()},
#' and renames columns to \code{regione}, \code{paese}, and \code{geo}.
#'
#' @param dt A \code{data.table} with a \code{geo} column (raw Eurostat codes).
#'
#' @return A \code{data.table} with columns \code{geo}, \code{regione}, and
#'   \code{paese}.
#'
#' @importFrom data.table setDT setnames
#' @importFrom dplyr mutate select
#' @importFrom eurostat label_eurostat
#'
#' @keywords internal
.build_geo_lookup <- function(dt) {
  # Create a minimal frame: geo label, country code, and raw geo code

  lookup <- dt[, .(geog = geo, values = substring(geo, 1, 2), time = geo)]
  lookup <- unique(lookup)
  data.table::setnames(lookup, "geog", "geo")

  # Label geo and country code using eurostat dictionaries

  lookup <- eurostat::label_eurostat(lookup, fix_duplicated = TRUE)
  data.table::setnames(lookup, "geo", "regione")
  data.table::setnames(lookup, "values", "paese")
  data.table::setnames(lookup, "time", "geo")
  data.table::setDT(lookup)

  lookup
}


#' Identify valid NUTS-2 region codes for a given NUTS vintage
#'
#' Summarises the dataset by \code{geo}, applies
#' \code{regions::recode_nuts()} and \code{regions::validate_nuts_regions()},
#' and returns only those codes classified as \code{nuts_level_2} that are
#' valid for the requested \code{nuts_year}.
#'
#' @param dt A \code{data.table} with a \code{geo} column.
#' @param nuts_year Integer indicating the NUTS vintage year (e.g., 2016).
#'
#' @return A character vector of valid NUTS-2 geo codes.
#'
#' @importFrom dplyr group_by summarise filter select
#' @importFrom regions recode_nuts validate_nuts_regions
#'
#' @keywords internal
.valid_nuts2_codes <- function(dt, nuts_year) {
  valid_col <- paste0("valid_", nuts_year)

  df_regioni <- dt[, .(n = .N), by = geo]
  df_regioni <- regions::recode_nuts(df_regioni, nuts_year = nuts_year)
  df_regioni <- regions::validate_nuts_regions(df_regioni)

  # Filter to NUTS level 2 regions that are valid for the specified year

  valid <- df_regioni[
    df_regioni$typology == "nuts_level_2" &
      df_regioni[[valid_col]] == TRUE,
    "geo"
  ]

  valid$geo
}


#' Collect and label a single Eurostat dataset for valid NUTS-2 regions
#'
#' Downloads one Eurostat dataset, builds a geo/country lookup, validates
#' NUTS-2 codes, filters to the requested countries, and returns a labelled
#' \code{data.table}.
#'
#' @param dataset_id A single character string with the Eurostat dataset ID.
#' @param nuts_year Integer indicating the NUTS vintage year.
#' @param countries Character vector of 2-character Eurostat country codes.
#'
#' @return A \code{data.table} with labelled columns plus \code{regione} and
#'   \code{paese}.
#'
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom data.table setDT
#'
#' @keywords internal
.collect_one <- function(dataset_id, nuts_year, countries) {
  message("*** ", dataset_id, " ***")

  # 1. Download raw data -----
  dt <- eurostat::get_eurostat(
    id = dataset_id,
    time_format = "raw",
    use.data.table = TRUE
  )

  # 2. Build country/region lookup -----
  lookup <- .build_geo_lookup(dt)

  # 3. Identify valid NUTS-2 codes -----
  valid_geo <- .valid_nuts2_codes(dt, nuts_year)

  # 4. Filter to valid NUTS-2 regions -----
  dt <- dt[geo %in% valid_geo]

  # 5. Label the dataset -----
  dt <- eurostat::label_eurostat(
    dt,
    fix_duplicated = TRUE,
    countrycode = "eurostat"
  )
  data.table::setDT(dt)

  # 6. Merge with geo lookup -----
  data.table::setDT(lookup)
  dt <- merge(dt, lookup, by = "geo", all.x = TRUE, all.y = FALSE)

  # 7. Filter to requested countries -----
  dt <- dt[paese %in% countries]

  dt
}


#' Collect Eurostat datasets for EU27 NUTS-2 regions
#'
#' Downloads one or more Eurostat datasets, filters to valid NUTS-2 regions
#' within the specified countries, labels all categorical columns, and returns
#' a named list of \code{data.table}s ready for further processing.
#'
#' The function reproduces and generalises the original \code{regioni()}
#' workflow from the collection script. For each dataset it:
#' \enumerate{
#'   \item Downloads raw data via \code{eurostat::get_eurostat()}.
#'   \item Builds a geo/country lookup by extracting 2-character country codes
#'         from \code{geo} and labelling them.
#'   \item Validates NUTS-2 codes with \code{regions::recode_nuts()} and
#'         \code{regions::validate_nuts_regions()}.
#'   \item Filters to valid NUTS-2 regions for the requested \code{nuts_year}.
#'   \item Labels all columns with \code{eurostat::label_eurostat()}.
#'   \item Merges with the country/region lookup and filters to the requested
#'         countries.
#' }
#'
#' @param dataset_ids Character vector of Eurostat dataset IDs (e.g.,
#'   \code{c("nama_10r_2gdp", "lfst_r_lfe2emprt")}).
#' @param nuts_year Integer indicating the NUTS classification vintage to
#'   validate against. Defaults to \code{2016}.
#' @param countries Character vector of 2-character Eurostat country codes to
#'   retain. Defaults to \code{NULL}, which uses the EU27 member states:
#'   AT, BE, BG, CY, CZ, DE, DK, EE, EL, ES, FI, FR, HR, HU, IE, IT,
#'   LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK.
#'
#' @return A named list of \code{data.table}s, one per dataset ID. Each table
#'   contains the labelled Eurostat variables plus two additional columns:
#'   \code{regione} (labelled region name) and \code{paese} (2-character
#'   country code). Names of the list correspond to the input
#'   \code{dataset_ids}.
#'
#' @export
#'
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom data.table setDT setnames
#' @importFrom dplyr mutate select group_by summarise filter
#' @importFrom regions recode_nuts validate_nuts_regions
#'
#' @examples
#' \dontrun{
#' # Collect two datasets using EU27 defaults
#' results <- ed_collect(c("nama_10r_2gdp", "lfst_r_lfe2emprt"))
#'
#' # Inspect the GDP dataset
#' str(results[["nama_10r_2gdp"]])
#'
#' # Use a different NUTS vintage and a custom set of countries
#' results <- ed_collect(
#'   dataset_ids = "demo_r_d2jan",
#'   nuts_year   = 2021,
#'   countries   = c("IT", "DE", "FR", "ES")
#' )
#' }
ed_collect <- function(dataset_ids, nuts_year = 2016, countries = NULL) {
  # 1. Input validation -----
  stopifnot(
    "`dataset_ids` must be a character vector" = is.character(dataset_ids),
    "`dataset_ids` must have at least one element" = length(dataset_ids) >= 1L,
    "`nuts_year` must be a single numeric value" = is.numeric(nuts_year) &&
      length(nuts_year) == 1L,
    "`countries` must be NULL or a character vector" = is.null(countries) ||
      is.character(countries)
  )

  # 2. Resolve country list -----
  if (is.null(countries)) {
    countries <- .eu27_codes
  }

  # 3. Collect each dataset -----
  results <- lapply(
    dataset_ids,
    .collect_one,
    nuts_year = nuts_year,
    countries = countries
  )
  names(results) <- dataset_ids

  results
}
