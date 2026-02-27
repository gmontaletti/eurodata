# Unit tests for internal helpers (R/utils.R) and ed_transform / ed_export
# (R/transform.R).

# 1. .apply_filters -----

test_that(".apply_filters keeps only rows matching exact column values", {
  dt <- data.table::data.table(
    sex = c("Total", "Male", "Female", "Total"),
    age = c("20-64", "20-64", "20-64", "15-24"),
    values = c(10, 20, 30, 40)
  )

  result <- eurodata:::.apply_filters(dt, list(sex = "Total"))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$sex == "Total"))
})

test_that(".apply_filters chains multiple filters", {
  dt <- data.table::data.table(
    sex = c("Total", "Total", "Male", "Total"),
    age = c("20-64", "15-24", "20-64", "20-64"),
    values = 1:4
  )

  result <- eurodata:::.apply_filters(dt, list(sex = "Total", age = "20-64"))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$sex == "Total" & result$age == "20-64"))
})

test_that(".apply_filters returns zero rows when no match", {
  dt <- data.table::data.table(
    sex = c("Male", "Female"),
    values = 1:2
  )
  result <- eurodata:::.apply_filters(dt, list(sex = "Total"))
  expect_equal(nrow(result), 0L)
})


# 2. .apply_filter_in -----

test_that(".apply_filter_in keeps rows where column value is in the set", {
  dt <- data.table::data.table(
    isced11 = c("Tertiary", "Secondary", "Primary", "Tertiary"),
    values = c(1, 2, 3, 4)
  )

  result <- eurodata:::.apply_filter_in(
    dt,
    list(isced11 = c("Tertiary", "Secondary"))
  )
  expect_equal(nrow(result), 3L)
  expect_true(all(result$isced11 %in% c("Tertiary", "Secondary")))
})

test_that(".apply_filter_in works with multiple column filters", {
  dt <- data.table::data.table(
    sex = c("Total", "Male", "Female", "Total"),
    unit = c("Pct", "Pct", "Abs", "Abs"),
    values = 1:4
  )

  result <- eurodata:::.apply_filter_in(
    dt,
    list(sex = c("Total", "Male"), unit = "Pct")
  )
  expect_equal(nrow(result), 2L)
})


# 3. .apply_filter_not_in -----

test_that(".apply_filter_not_in excludes rows where column value is in the set", {
  dt <- data.table::data.table(
    nace = c("Manufacturing", "Unknown", "Finance", "Other"),
    values = 1:4
  )

  result <- eurodata:::.apply_filter_not_in(
    dt,
    list(nace = c("Unknown", "Other"))
  )
  expect_equal(nrow(result), 2L)
  expect_true(all(result$nace %in% c("Manufacturing", "Finance")))
})

test_that(".apply_filter_not_in keeps everything when exclusion set is empty", {
  dt <- data.table::data.table(x = c("a", "b", "c"), values = 1:3)
  result <- eurodata:::.apply_filter_not_in(dt, list(x = character(0)))
  expect_equal(nrow(result), 3L)
})


# 4. .apply_remap -----

test_that(".apply_remap replaces known values and keeps unmatched as-is", {
  dt <- data.table::data.table(
    category = c("Persons employed in science", "Scientists", "Other category"),
    values = 1:3
  )

  remap_spec <- list(
    col = "category",
    mapping = list(
      "Persons employed in science" = "SciEmp",
      "Scientists" = "Sci"
    )
  )

  result <- eurodata:::.apply_remap(dt, remap_spec)
  expect_equal(result$category, c("SciEmp", "Sci", "Other category"))
})

test_that(".apply_remap does not modify rows outside the mapping", {
  dt <- data.table::data.table(x = c("a", "b"), values = 1:2)
  remap_spec <- list(col = "x", mapping = list("z" = "Z"))
  result <- eurodata:::.apply_remap(dt, remap_spec)
  expect_equal(result$x, c("a", "b"))
})


# 5. .apply_substring -----

test_that(".apply_substring extracts the expected portion", {
  dt <- data.table::data.table(
    na_item = c("abcdefghijklmnopqrstuvwxyz", "12345678901234567890"),
    values = 1:2
  )

  result <- eurodata:::.apply_substring(
    dt,
    list(col = "na_item", start = 3L, stop = 7L)
  )
  expect_equal(result$na_item, c("cdefg", "34567"))
})

test_that(".apply_substring handles single-character extraction", {
  dt <- data.table::data.table(code = c("ABC", "DEF"), values = 1:2)
  result <- eurodata:::.apply_substring(
    dt,
    list(col = "code", start = 1L, stop = 1L)
  )
  expect_equal(result$code, c("A", "D"))
})


# 6. .make_icode2 -----

test_that(".make_icode2 strips keywords, lowercases, and truncates to 10 chars", {
  iCode <- "TestCode"
  gruppo <- "persons with High Education"

  result <- eurodata:::.make_icode2(iCode, gruppo)

  # "persons" and "with" are stripped, spaces are stripped, commas stripped
  # remaining: "HighEducation" -> lowercase -> "higheducat" (first 10)
  expect_equal(result, "TestCodehigheducat")
})

test_that(".make_icode2 is vectorised", {
  iCode <- c("A", "B")
  gruppo <- c("Total", "persons Female")

  result <- eurodata:::.make_icode2(iCode, gruppo)
  expect_length(result, 2L)
  # "Total" -> "total" -> first 10 -> "total"
  expect_equal(result[1], "Atotal")
  # "persons Female" -> strip "persons" and space -> "Female" -> "female"
  expect_equal(result[2], "Bfemale")
})

test_that(".make_icode2 handles commas and hyphens", {
  result <- eurodata:::.make_icode2("X", "part-time, workers")
  # gsub removes commas, spaces, hyphens: "parttimeworkers"
  # lowercase first 10: "parttimewo"
  expect_equal(result, "Xparttimewo")
})


# 7. .apply_spec -----

test_that(".apply_spec produces canonical output columns with a minimal spec", {
  dt <- data.table::data.table(
    geo = c("ITH3", "ITH3", "DE21", "DE21"),
    paese = c("IT", "IT", "DE", "DE"),
    regione = c("Veneto", "Veneto", "Oberbayern", "Oberbayern"),
    sex = c("Total", "Male", "Total", "Male"),
    values = c(10.1, 20.2, 30.3, 40.4),
    TIME_PERIOD = c(2022, 2022, 2022, 2022)
  )

  spec <- list(
    dataset_id = "test_ds",
    indice = "Test indicator",
    iCode = "TestInd",
    gruppo_col = "sex",
    filters = list(sex = "Total")
  )

  result <- eurodata:::.apply_spec(dt, spec)

  expected_cols <- c(
    "indice",
    "gruppo",
    "iCode",
    "area",
    "geo",
    "TIME_PERIOD",
    "values"
  )
  expect_equal(names(result), expected_cols)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$indice == "Test indicator"))
  expect_true(all(result$iCode == "TestInd"))
  # area should be paste(paese, regione) by default
  expect_true("IT Veneto" %in% result$area)
  expect_true("DE Oberbayern" %in% result$area)
})

test_that(".apply_spec applies gruppo_literal instead of gruppo_col", {
  dt <- data.table::data.table(
    geo = "ITH3",
    paese = "IT",
    regione = "Veneto",
    values = 5.5,
    TIME_PERIOD = 2022
  )

  spec <- list(
    dataset_id = "test_ds",
    indice = "Income ratio",
    iCode = "Income",
    gruppo_literal = "Rat8020"
  )

  result <- eurodata:::.apply_spec(dt, spec)
  expect_equal(result$gruppo, "Rat8020")
})

test_that(".apply_spec uses area_formula = 'geo' when requested", {
  dt <- data.table::data.table(
    geo = c("ITH3", "DE21"),
    paese = c("IT", "DE"),
    regione = c("Veneto", "Oberbayern"),
    values = c(1, 2),
    TIME_PERIOD = c(2022, 2022)
  )

  spec <- list(
    dataset_id = "test_ds",
    indice = "Geo-based",
    iCode = "GeoInd",
    gruppo_literal = "all",
    area_formula = "geo"
  )

  result <- eurodata:::.apply_spec(dt, spec)
  expect_equal(result$area, c("ITH3", "DE21"))
})

test_that(".apply_spec removes NA values when filter_na is TRUE", {
  dt <- data.table::data.table(
    geo = c("ITH3", "DE21", "FR10"),
    paese = c("IT", "DE", "FR"),
    regione = c("Veneto", "Bayern", "IleDeFrance"),
    values = c(10, NA, 30),
    TIME_PERIOD = c(2022, 2022, 2022)
  )

  spec <- list(
    dataset_id = "test_ds",
    indice = "Population index",
    iCode = "PopIdx",
    gruppo_literal = "Total",
    filter_na = TRUE
  )

  result <- eurodata:::.apply_spec(dt, spec)
  expect_equal(nrow(result), 2L)
  expect_false(anyNA(result$values))
})

test_that(".apply_spec handles aggregate = TRUE", {
  # Simulate quarterly data that gets averaged to one annual value per region
  dt <- data.table::data.table(
    geo = rep("ITH3", 4),
    paese = rep("IT", 4),
    regione = rep("Veneto", 4),
    quarter = c("Q1", "Q2", "Q3", "Q4"),
    values = c(10, 20, 30, 40),
    TIME_PERIOD = rep(2022, 4)
  )

  spec <- list(
    dataset_id = "test_ds",
    indice = "Quarterly avg",
    iCode = "QAvg",
    gruppo_literal = "all",
    aggregate = TRUE
  )

  result <- eurodata:::.apply_spec(dt, spec)
  expect_equal(nrow(result), 1L)
  expect_equal(result$values, 25)
})


# 8. ed_transform -----

test_that("ed_transform combines results from multiple specs", {
  ds1 <- data.table::data.table(
    geo = c("ITH3", "DE21"),
    paese = c("IT", "DE"),
    regione = c("Veneto", "Bayern"),
    sex = c("Total", "Total"),
    values = c(70.5, 80.3),
    TIME_PERIOD = c(2022, 2022)
  )

  ds2 <- data.table::data.table(
    geo = c("ITH3", "DE21"),
    paese = c("IT", "DE"),
    regione = c("Veneto", "Bayern"),
    values = c(110, 125),
    TIME_PERIOD = c(2022, 2022)
  )

  data_list <- list(ds_alpha = ds1, ds_beta = ds2)

  specs <- list(
    list(
      dataset_id = "ds_alpha",
      indice = "Employment rate",
      iCode = "EmpRate",
      gruppo_col = "sex",
      filters = list(sex = "Total")
    ),
    list(
      dataset_id = "ds_beta",
      indice = "PPS index",
      iCode = "PPSidx",
      gruppo_literal = "all"
    )
  )

  result <- ed_transform(data_list, specs)

  expect_s3_class(result, "data.table")
  expected_cols <- c(
    "iCode",
    "iName",
    "uCode",
    "uName",
    "TIME_PERIOD",
    "values"
  )
  expect_equal(names(result), expected_cols)
  # 2 regions x 2 specs = 4 rows
  expect_equal(nrow(result), 4L)
  # iName should be paste(indice, gruppo)
  expect_true("Employment rate Total" %in% result$iName)
  expect_true("PPS index all" %in% result$iName)
})

test_that("ed_transform skips specs whose dataset_id is not in data_list", {
  ds1 <- data.table::data.table(
    geo = "ITH3",
    paese = "IT",
    regione = "Veneto",
    values = 50,
    TIME_PERIOD = 2022
  )

  data_list <- list(ds_found = ds1)

  specs <- list(
    list(
      dataset_id = "ds_found",
      indice = "Found",
      iCode = "Found",
      gruppo_literal = "g"
    ),
    list(
      dataset_id = "ds_missing",
      indice = "Missing",
      iCode = "Missing",
      gruppo_literal = "g"
    )
  )

  expect_message(
    result <- ed_transform(data_list, specs),
    "Skipping spec 'ds_missing'"
  )
  expect_equal(nrow(result), 1L)
})

test_that("ed_transform errors when no specs match", {
  data_list <- list(ds_real = data.table::data.table(x = 1))
  specs <- list(list(dataset_id = "ds_nonexistent", indice = "X", iCode = "X"))

  expect_error(ed_transform(data_list, specs), "No specs matched")
})

test_that("ed_transform removes UK rows from output", {
  ds <- data.table::data.table(
    geo = c("ITH3", "UKD3"),
    paese = c("IT", "UK"),
    regione = c("Veneto", "Manchester"),
    values = c(50, 60),
    TIME_PERIOD = c(2022, 2022)
  )

  data_list <- list(ds_uk = ds)
  specs <- list(list(
    dataset_id = "ds_uk",
    indice = "Ind",
    iCode = "Ind",
    gruppo_literal = "g"
  ))

  result <- ed_transform(data_list, specs)
  expect_false(any(startsWith(result$uName, "UK")))
  expect_equal(nrow(result), 1L)
})

test_that("ed_transform removes No response and Unknown gruppo values", {
  ds <- data.table::data.table(
    geo = rep("ITH3", 3),
    paese = rep("IT", 3),
    regione = rep("Veneto", 3),
    grp = c("Employed", "No response", "Unknown"),
    values = c(10, 20, 30),
    TIME_PERIOD = rep(2022, 3)
  )

  data_list <- list(ds_grp = ds)
  specs <- list(list(
    dataset_id = "ds_grp",
    indice = "Test",
    iCode = "Tst",
    gruppo_col = "grp"
  ))

  result <- ed_transform(data_list, specs)
  expect_false("No response" %in% result$iName)
  expect_false("Unknown" %in% result$iName)
  expect_equal(nrow(result), 1L)
})

test_that("ed_transform validates input types", {
  expect_error(
    ed_transform("not a list", list()),
    "data_list must be a named list"
  )
  expect_error(
    ed_transform(list(a = 1), "not a list"),
    "specs must be a list"
  )
})


# 9. ed_export -----

test_that("ed_export writes an RDS file by default", {
  dt <- data.table::data.table(x = 1:5)
  tmp <- file.path(tempdir(), "test_export_rds")

  ed_export(dt, tmp, format = "rds")

  rds_path <- paste0(tmp, ".rds")
  expect_true(file.exists(rds_path))

  loaded <- readRDS(rds_path)
  expect_equal(loaded, dt)

  unlink(rds_path)
})

test_that("ed_export writes an xlsx file when requested", {
  skip_if_not_installed("writexl")

  dt <- data.table::data.table(a = c("x", "y"), b = c(1, 2))
  tmp <- file.path(tempdir(), "test_export_xlsx")

  ed_export(dt, tmp, format = "xlsx")

  xlsx_path <- paste0(tmp, ".xlsx")
  expect_true(file.exists(xlsx_path))
  expect_gt(file.size(xlsx_path), 0)

  unlink(xlsx_path)
})

test_that("ed_export writes both formats when format = 'both'", {
  skip_if_not_installed("writexl")

  dt <- data.table::data.table(z = 1:3)
  tmp <- file.path(tempdir(), "test_export_both")

  ed_export(dt, tmp, format = "both")

  expect_true(file.exists(paste0(tmp, ".rds")))
  expect_true(file.exists(paste0(tmp, ".xlsx")))

  unlink(paste0(tmp, ".rds"))
  unlink(paste0(tmp, ".xlsx"))
})

test_that("ed_export returns dt invisibly", {
  dt <- data.table::data.table(v = 1)
  tmp <- file.path(tempdir(), "test_export_invisible")

  result <- ed_export(dt, tmp, format = "rds")
  expect_equal(result, dt)

  unlink(paste0(tmp, ".rds"))
})

test_that("ed_export validates inputs", {
  expect_error(ed_export("not a df", "path"), "dt must be a data.frame")
  expect_error(
    ed_export(data.frame(x = 1), c("a", "b")),
    "path must be a single character"
  )
})
