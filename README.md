# eurodata

Regional Indicators from Eurostat for EU27 NUTS-2 Regions

## Description

**eurodata** is an R package that collects approximately 24 Eurostat datasets at the NUTS-2 regional level for all 27 EU member states, transforms them into a unified schema suitable for comparative analysis, and selects a peer group of regions comparable to a given reference region. The package covers labour market, education, demographics, ICT, and economic indicators.

The pipeline proceeds in three stages: data collection from the Eurostat bulk download API, transformation into a standardised indicator table, and peer group selection based on population size and GDP per capita (PPS). Results can be exported to RDS and Excel formats.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("gmontaletti/eurodata")
```

## Dependencies

**eurodata** depends on the following packages (installed automatically):

- [eurostat](https://cran.r-project.org/package=eurostat) -- API access to Eurostat bulk data
- [data.table](https://cran.r-project.org/package=data.table) -- data manipulation
- [dplyr](https://cran.r-project.org/package=dplyr) -- filtering and reshaping
- [regions](https://cran.r-project.org/package=regions) -- NUTS region validation

Optional (suggested):

- [writexl](https://cran.r-project.org/package=writexl) -- Excel export
- [ggplot2](https://cran.r-project.org/package=ggplot2) -- exploratory plots

## Usage

The standard workflow consists of four steps:

```r
library(eurodata)

# 1. Collect raw data from Eurostat for all configured datasets
raw <- ed_collect(names(dataset_specs))

# 2. Transform into a unified indicator table
unified <- ed_transform(raw)

# 3. Select a peer group of regions comparable to Veneto
peer <- ed_peergroup(unified)

# 4. Export results to RDS and Excel
ed_export(peer, "output/indici_2024", format = "both")
```

Each step can be customised. For example, to collect only a subset of datasets, or to change the reference region for peer group selection:

```r
# Collect only GDP and employment data
raw <- ed_collect(c("nama_10r_2gdp", "lfst_r_lfe2emprt"))

# Use Bayern as reference region with custom thresholds
peer <- ed_peergroup(
  unified,
  reference_region = "DE Bayern",
  pop_ratio = 0.5,
  pps_low_ratio = 0.75,
  target_year = 2023
)
```

## Functions

| Function | Description |
|---|---|
| `ed_collect()` | Download and label one or more Eurostat datasets, filtering to valid NUTS-2 regions within EU27 countries. Returns a named list of `data.table`s. |
| `ed_transform()` | Apply transformation specifications to the collected datasets and combine them into a single `data.table` with a unified schema (`iCode`, `iName`, `uCode`, `uName`, `TIME_PERIOD`, `values`). |
| `ed_peergroup()` | Select a peer group of NUTS-2 regions comparable to a reference region based on population and PPS, then filter the indicator data to the peer group for a target year. |
| `ed_export()` | Write a `data.table` to RDS and/or Excel format. |
| `dataset_specs` | A named list of 24 transformation specifications defining how each Eurostat dataset is filtered, remapped, and reshaped into the common schema. |

## Data: dataset_specs

`dataset_specs` is a named list shipped with the package. Each element describes how one Eurostat dataset should be processed. The list is indexed by Eurostat dataset ID (e.g., `"nama_10r_2gdp"`, `"lfst_r_lfe2emprt"`).

Inspect the available datasets:

```r
# List all configured dataset IDs
names(dataset_specs)

# View the specification for a single dataset
str(dataset_specs[["nama_10r_2gdp"]])
```

Each specification may include:

- `dataset_id` -- Eurostat table identifier
- `indice` -- human-readable indicator name
- `iCode` -- short mnemonic code for the indicator
- `gruppo_col` or `gruppo_literal` -- column to use as grouping variable, or a fixed string
- `filters`, `filter_in`, `filter_not_in`, `filter_na` -- row filtering rules
- `remap` -- value remapping for a column
- `substring_transform` -- substring extraction on a column
- `aggregate` -- whether to aggregate (mean) across sub-periods
- `area_formula` -- how to construct the area label

You can pass a custom specs list to `ed_transform()` to process only a subset of indicators or to define new transformations.

## Unified Schema

The output of `ed_transform()` follows a standardised structure:

| Column | Description |
|---|---|
| `iCode` | Composite indicator code |
| `iName` | Human-readable indicator name |
| `uCode` | NUTS-2 geo code |
| `uName` | Region label (country + region name) |
| `TIME_PERIOD` | Reference year |
| `values` | Numeric indicator value |

## Citation

```
Montaletti, G. (2025). eurodata: Regional Indicators from Eurostat for EU27 NUTS-2 Regions.
R package version 0.1.0. https://github.com/gmontaletti/eurodata
```

## License

Apache License 2.0

## Author

Giampaolo Montaletti ([giampaolo.montaletti@gmail.com](mailto:giampaolo.montaletti@gmail.com))
ORCID: [0009-0002-5327-1122](https://orcid.org/0009-0002-5327-1122)
GitHub: [github.com/gmontaletti](https://github.com/gmontaletti)
