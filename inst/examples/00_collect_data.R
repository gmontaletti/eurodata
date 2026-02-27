library(eurostat)
library(dplyr)
library(data.table)

# clean cache if necessary
# clean_eurostat_cache()

regioni <- function(dset = dati) {
  # dset = "hrst_st_rcat"
  print(paste0("*** ", dset, " ***"))
  df1 =  get_eurostat(id = dset
                      , time_format = "raw"
                      , use.data.table = TRUE
                      # , select_time = "A"
                      # , filters = list(sinceTimePeriod = 2019)
  )
  paesi <- df1 %>% mutate(paese = substring(geo,1,2)) %>% select(geog = geo, values = paese, time = geo)
  paesi <- paesi %>% unique()
  setnames(paesi, "geog", "geo")
  paesi <- label_eurostat(paesi, fix_duplicated = T)
  setnames(paesi, "geo", "regione")
  setnames(paesi, "values", "paese")
  setnames(paesi, "time", "geo")

  df_regioni <- df1 %>% group_by(geo) %>% summarise(n = n())
  df_regioni <- regions::recode_nuts(df_regioni, nuts_year = 2016)
  df_regioni <- validate_nuts_regions(df_regioni)
  lreg <- df_regioni %>%
    filter(typology == "nuts_level_2" &
             valid_2016 == TRUE
    ) %>%
    select(geo)

  df1 <- df1 %>% filter(geo %in% lreg$geo)
  df1 <- label_eurostat(df1, fix_duplicated = T, countrycode = "eurostat")
  setDT(df1)
  setDT(paesi)
  df1 <- merge.data.table(df1, paesi, all.x = T, all.y = F
                          , by.x = "geo", by.y = "geo")

  eu27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI"
            , "FR", "HR", "HU", "IE", "IT", "LT", "LU"
            , "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  df1 <- df1 %>% filter(paese %in% eu27)
  return(df1)
}


# eurostat::clean_eurostat_cache()
#  configura nome dataset -----
dati = c(
  "nama_10r_2gdp"
  , "edat_lfse_33"
  , "lfst_r_lfp2actrtn"
  , "edat_lfse_16"
  , "lfst_r_lfe2ehour"
  , "isoc_sk_oja1"
  , "isoc_sk_oja3"
  , "hrst_st_rcat"
  , "lfst_r_lfu2ltu"
  , "yth_empl_130"
  , "lfst_r_egad"
  , "lfst_r_lfe2eftpt"
  , "lfst_r_sla_ga"
  , "trng_lfse_04"
  # , "trng_lfs_22"
  , "htec_emp_reg2"
  , "nama_10r_2rlp"
  , "edat_lfs_9917"
  , "demo_r_d2jan"
  , "demo_r_pjanind2"
  # , "tgs00005"
  , "edat_lfse_22"
  , "lfst_r_lfu3rt"
  , "lfst_r_lfe2emprt"
  , "lfst_r_lfe2ehrwa"
  , "ilc_di11_r"
) |> as.list()
names(dati) <- dati

#  scarica tutto ----
indicatori <- lapply(dati, regioni)

saveRDS(indicatori, "../datasets/EURO_indicatori.rds")

get_bibentry(names(dati))

list2env(indicatori, envir = .GlobalEnv )

demo <- regioni("ilc_di11_r")
get_bibentry("ilc_di11_r")

