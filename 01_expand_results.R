library(eurostat)
library(dplyr)
library(data.table)
# clean cache if necessary



regioni <- readRDS("../datasets/EURO_indicatori.rds")

get_bibentry(names(regioni))

# tira su -----
list2env(regioni, env = .GlobalEnv)
names(regioni) |> toString()

### edat Pct 25-64 anni per titolo di studio ----
names(edat_lfs_9917)
edat_lfs_9917 %>% group_by(isced11) %>% count()

get_bibentry("edat_lfs_9917")
# writexl::write_xlsx(edat_lfs_9917, "xlsx/edat_lfs_9917.xlsx")

edat_lfs_9917 <-  edat_lfs_9917 %>%
  filter(sex == "Total" &
           c_birth == "Total" &
           age == "From 25 to 64 years" &
           isced11 %in% c(
             "Less than primary, primary and lower secondary education (levels 0-2)"
             , "Tertiary education (levels 5-8)"
             , "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)"
           )
  ) %>%
  mutate(indice = "Pct 25-64 anni per titolo di studio") %>%
  mutate(iCode = "Educ2564P") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = isced11, iCode, area, geo, TIME_PERIOD_PERIOD, values)

## Pct 18-24 abbandono studi ----
names(edat_lfse_16)

# writexl::write_xlsx(edat_lfse_16, "xlsx/edat_lsfe_16.xksx")

edat_lfse_16 %>% select(where(~n_distinct(.) > 1)) %>% names()
edat_lfse_16 %>% group_by(sex) %>% count()
edat_lfse_16 <-  edat_lfse_16 %>%
  mutate(indice = "Pct 18-24 abbandono studi") %>%
  mutate(iCode = "AbbaStudP") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = sex, iCode, area, geo, TIME_PERIOD, values)

### Occ entro 3 anni dal titolo ----
names(edat_lfse_33)
edat_lfse_33 %>% select(where(~n_distinct(.) > 1)) %>% names()
edat_lfse_33 %>% group_by(age) %>% count()
edat_lfse_33 <-  edat_lfse_33 %>% filter(sex == "Total" &
                                           duration == "3 years or less" &
                                           age == "From 18 to 34 years" &
                                           isced11 %in% c("Tertiary education (levels 5-8)"
                                                          , "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)")
) %>%
  mutate(indice = "Occ entro 3 anni dal titolo") %>%
  mutate(iCode = "Occ3AnniP") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = isced11 , iCode, area, geo, TIME_PERIOD, values)

### NEET  ----
get_bibentry("edat_lfse_22")
names(edat_lfse_22)
edat_lfse_22 %>% select(where(~n_distinct(.) > 1)) %>% names()
edat_lfse_22 %>% group_by(age) %>% count()
edat_lfse_22 <-  edat_lfse_22 %>% filter(sex == "Total") %>%
  mutate(indice = "NEET") %>%
  mutate(iCode = "NEETP") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = age , iCode, area, geo, TIME_PERIOD, values)


### Pct sci e tech delle forze lavoro ----

names(hrst_st_rcat)
hrst_st_rcat %>% select(where(~n_distinct(.) > 1)) %>% names()
hrst_st_rcat %>% group_by(category) %>% count()
hrst_st_rcat <-  hrst_st_rcat %>%
  filter(unit == "Percentage of total population") %>%
  mutate(category = case_match(category
                               , "Persons employed in science and technology" ~ "OccSciTech"
                               , "Persons with tertiary education (ISCED)" ~ "PersTerEdu"
                               , "Persons with tertiary education (ISCED) and employed in science and technology" ~ "TeduAndStech"
                               , "Persons with tertiary education (ISCED) and/or employed in science and technology" ~ "TeduAorStech"
                               , "Scientists and engineers" ~ "ScienEngin"
  )
  ) %>%
  mutate(indice = "Pct in sci e tech") %>%
  mutate(iCode = "SCItecPOPP") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = category , iCode, area, geo, TIME_PERIOD, values)

### Pct occupazione totale ----

names(htec_emp_reg2)
htec_emp_reg2 %>% select(where(~n_distinct(.) > 1)) %>% names()
htec_emp_reg2 %>% group_by(nace_r2) %>% count() -> temp
htec_emp_reg2 <-  htec_emp_reg2 %>%
  filter(unit == "Percentage of total employment"
         & sex == "Total"
         & nace_r2 != "Total - all NACE activities"
         & nace_r2 != "Unknown NACE activity"
         & nace_r2 != "Other NACE activities"
  ) %>%
  mutate(nace_r2 = case_match(nace_r2
                              , "Financial and insurance activities" ~ "FinInscur"
                              , "Financial and insurance activities; real estate activities" ~ "FinRealEstate"
                              , "High-technology manufacturing" ~ "HiTecManif"
                              , "High-technology sectors (high-technology manufacturing and knowledge-intensive high-technology services)" ~ "HiTecAll"
                              , "Knowledge-intensive high-technology services" ~ "KiHtServices"
                              , "Knowledge-intensive market services (except financial intermediation and high-technology services)" ~ "KiMarkServices"
                              , .default = nace_r2
  )) %>%
  mutate(indice = "Pct occupazione totale") %>%
  mutate(iCode = "OcSetP") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = nace_r2 , iCode, area,  geo, TIME_PERIOD, values)

### Redditi disparità  ----
get_bibentry("ilc_di11_r")
names(ilc_di11_r)
ilc_di11_r %>% select(where(~n_distinct(.) > 1)) %>% names()

ilc_di11_r <-  ilc_di11_r %>%
  mutate(indice = "redditi ratio S80_S20") %>%
  mutate(iCode = "Income") %>%
  mutate(gruppo = "Rat8020") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo , iCode, area,  geo, TIME_PERIOD, values)


### annunci ICT trim perc ----
names(isoc_sk_oja1)
isoc_sk_oja1 %>% select(where(~n_distinct(.) > 1)) %>% names()
isoc_sk_oja1 %>% group_by(unit) %>% count()
isoc_sk_oja1 <-  isoc_sk_oja1 %>%
  mutate(indice = "ICT annunci") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = unit , area, geo, TIME_PERIOD, values)
setDT(isoc_sk_oja1)
isoc_sk_oja1 <- isoc_sk_oja1[, .(values = mean(values, na.rm = T),
                                 iCode = "ICTVacaP"
), .(indice, gruppo, area, geo, TIME_PERIOD )]


### var ict vacancy ----
names(isoc_sk_oja3)
isoc_sk_oja3 %>% select(where(~n_distinct(.) > 1)) %>% names()
isoc_sk_oja3 %>% group_by(unit) %>% count()
isoc_sk_oja3 <-  isoc_sk_oja3 %>%
  mutate(indice = "ICT var % annunci") %>%
  mutate(area = paste(geo)) %>%
  select(indice, gruppo = unit , geo, TIME_PERIOD, values)
setDT(isoc_sk_oja3)
isoc_sk_oja3 <- isoc_sk_oja3[, .(values = mean(values, na.rm = T)
                                 , iCode = "ICTVacVP"
), .(indice, gruppo, area = geo, geo, TIME_PERIOD)]

### durata dei contratti % dell'occupazione ----
names(lfst_r_egad)
lfst_r_egad %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_egad %>% group_by(duration) %>% count()
lfst_r_egad <-  lfst_r_egad %>%
  filter(unit == "Percentage"
         & sex == "Total"
         & age == "From 20 to 64 years") %>%
  mutate(indice = "Durata contratti") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = duration , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "DurContrP")


### migliaia FT e part-TIME_PERIOD ----
names(lfst_r_lfe2eftpt)
lfst_r_lfe2eftpt %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfe2eftpt %>% group_by(workTIME_PERIOD) %>% count()
lfst_r_lfe2eftpt <-  lfst_r_lfe2eftpt %>%
  filter(#unit == "Percentage"
    wstatus == "Employed persons" &
      sex == "Total"
    & age == "From 20 to 64 years") %>%
  mutate(indice = "Migliaia di addetti") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = workTIME_PERIOD , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "FTimPtimM")

### ore lavorate in una settimana ----
names(lfst_r_lfe2ehour)
get_bibentry("lfst_r_lfe2ehour")
lfst_r_lfe2ehour %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfe2ehour %>% group_by(age) %>% count()
lfst_r_lfe2ehour <-  lfst_r_lfe2ehour %>%
  filter(sex == "Total") %>%
  mutate(indice = "Ore lavorate di solito") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = age, area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "OreLavSol")

### ore attuali in unsa settimana ----
names(lfst_r_lfe2ehrwa)
get_bibentry("lfst_r_lfe2ehrwa")
lfst_r_lfe2ehrwa %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfe2ehrwa %>% group_by(age) %>% count()
lfst_r_lfe2ehrwa <-  lfst_r_lfe2ehrwa %>%
  filter(sex == "Total") %>%
  mutate(indice = "Ore attuali") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = age, area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "OreLActual")


### Disoc. lungo termine % forze lavoro ----
names(lfst_r_lfu2ltu)
get_bibentry("lfst_r_lfu2ltu")
lfst_r_lfu2ltu %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfu2ltu %>% group_by(unit) %>% count(is.na(values))
lfst_r_lfu2ltu <-  lfst_r_lfu2ltu %>%
  filter(unit == "Percentage of population in the labour force" &
           sex == "Total"
         & age == "From 20 to 64 years"
  ) %>%
  mutate(indice = "Disocc. lungo termine % forze lavoro") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = isced11 , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "DisLungaP")

###  tasso di disoccupazione ----
names(lfst_r_lfu3rt)
get_bibentry("lfst_r_lfu3rt")
lfst_r_lfu3rt %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfu3rt %>% group_by(isced11, sex) %>% count(is.na(values))
lfst_r_lfu3rt <-  lfst_r_lfu3rt %>%
  filter(sex == "Total"
         & age == "From 20 to 64 years"
  ) %>%
  mutate(indice = "Tasso di disoccupazione") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = isced11 , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "DisTass")

###  tasso di occupazione ----
names(lfst_r_lfe2emprt)
get_bibentry("lfst_r_lfe2emprt")
lfst_r_lfe2emprt %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfe2emprt %>% group_by(age) %>% count()
lfst_r_lfe2emprt <-  lfst_r_lfe2emprt %>%
  filter(sex == "Total"
  ) %>%
  mutate(indice = "Tasso di occupazione") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = age , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "OccTass")

###  tasso di attività ----
names(lfst_r_lfp2actrtn)
get_bibentry("lfst_r_lfp2actrtn")
lfst_r_lfp2actrtn %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_lfp2actrtn %>% group_by(citizen) %>% count()
lfst_r_lfp2actrtn <-  lfst_r_lfp2actrtn %>%
  filter(sex == "Total"
         & age == "From 15 to 64 years"
         & citizen == "Total"
  ) %>%
  mutate(indice = "Tasso di attivita") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = isced11 , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "AttTass")

### Slack % su fl estesa ----
names(lfst_r_sla_ga)
lfst_r_sla_ga %>% select(where(~n_distinct(.) > 1)) %>% names()
lfst_r_sla_ga %>% group_by(unit) %>% count()
lfst_r_sla_ga <-  lfst_r_sla_ga %>%
  filter(unit == "Percentage of extended labour force" &
           sex == "Total"
  ) %>%
  mutate(indice = "Slack % su fl estesa") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = age , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "SlackFLEP")

### Partecipazione % formazione u4s ----
names(trng_lfse_04)

# writexl::write_xlsx(trng_lfse_04, "xlsx/trng_lfse_04.xlsx")

trng_lfse_04 %>% select(where(~n_distinct(.) > 1)) %>% names()
trng_lfse_04 %>% group_by(unit) %>% count()
trng_lfse_04 <-  trng_lfse_04 %>%
  filter(sex == "Total"
  ) %>%
  mutate(indice = "Partecipazione % formazione u4s") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = age , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "Formz4sett")

### Partecipazione % formazione u4s ----
# names(trng_lfs_22)
#
# get_bibentry("trng_lfs_22")
# # writexl::write_xlsx(trng_lfse_04, "xlsx/trng_lfse_04.xlsx")
#
# trng_lfs_22 %>% select(where(~n_distinct(.) > 1)) %>% names()
# trng_lfs_22 %>% group_by(age) %>% count(is.na(values))
# trng_lfs_22 <-  trng_lfs_22 %>%
#   filter(sex == "Total"
#   ) %>%
#   mutate(indice = "Partecipazione % formazione 12 mesi") %>%
#   mutate(area = paste(paese, regione)) %>%
#   select(indice, gruppo = age , area, geo, TIME_PERIOD, values) %>%
#   mutate(iCode = "Formz12mesi")

### PPS procapite indice ---
names(nama_10r_2gdp)
nama_10r_2gdp %>% select(where(~n_distinct(.) > 1)) %>% names()
nama_10r_2gdp %>% group_by(unit) %>% count()
nama_10r_2gdp <-  nama_10r_2gdp %>%
  filter(#unit == "Percentage of extended labour force" &
    # wstatus == "Employed persons" &
    unit == "Purchasing power standard (PPS, EU27 from 2020), per inhabitant in percentage of the EU27 (from 2020) average"
    # & age == "From 20 to 64 years"
  ) %>%
  mutate(indice = "PPS procapite indice") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = unit , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "PPSprcptP")

### indice produttivita reale lavoro ----
names(nama_10r_2rlp)
nama_10r_2rlp %>% select(where(~n_distinct(.) > 1)) %>% names()
nama_10r_2rlp %>% group_by(na_item) %>% count()
nama_10r_2rlp <- nama_10r_2rlp %>% mutate(na_item = substring(na_item,30, 40))
nama_10r_2rlp <-  nama_10r_2rlp %>%
  filter(#unit == "Percentage of extended labour force" &
    # wstatus == "Employed persons" &
    unit == "Index, 2015=100"
    # & age == "From 20 to 64 years"
  ) %>%
  mutate(indice = "indice produttivita reale lavoro") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = na_item , area,  geo, TIME_PERIOD, values) %>%
  mutate(iCode = "ProdRealI")


### Disoccupazione giovanile lungo termine % ----
names(yth_empl_130)
yth_empl_130 %>% select(where(~n_distinct(.) > 1)) %>% names()
yth_empl_130 %>% group_by(sex) %>% count()
yth_empl_130 <-  yth_empl_130 %>%
  mutate(indice = "Disoccupazione giovanile lungo termine P") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = sex , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "DisGioLuP")

### PPs in euro ----
names(tgs00005)
get_bibentry("tgs00005")
tgs00005 %>% select(where(~n_distinct(.) > 1)) %>% names()
tgs00005 <-  tgs00005 %>%
  mutate(indice = "PPS") %>%
  mutate(gruppo = "Euro") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo, area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "EuroPPS")

### popolazione valori ----

names(demo_r_d2jan)
demo_r_d2jan %>% select(where(~n_distinct(.) > 1)) %>% names()
demo_r_d2jan %>% group_by(age) %>% count() -> alleta
demo_r_d2jan <-  demo_r_d2jan %>%
  filter(age == "Total") %>%
  mutate(indice = "Popolazione 1 gennaio") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = sex , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "Popolazio")


### popolazione indici ----

names(demo_r_pjanind2)
get_bibentry("demo_r_pjanind2")
demo_r_pjanind2 %>% select(where(~n_distinct(.) > 1)) %>% names()
demo_r_pjanind2 %>% group_by(indic_de) %>% count() -> temp
demo_r_pjanind2 <-  demo_r_pjanind2 %>%
  filter( indic_de %in% c("Median age of population"
                          , "Women per 100 men"
                          , "Age dependency ratio, 1st variant (population 0 to 14 years and 65 years or over to population 15 to 64 years)"
                          , "Old-age dependency ratio 1st variant (population 65 years or over to population 15 to 64 years)"
                          , "Young-age dependency ratio 1st variant (population 0 to 14 years to population 15 to 64 years)"
  )
  ) %>%
  filter(!is.na(values)) %>%
  mutate(indice = "Indici della popolazione") %>%
  mutate(area = paste(paese, regione)) %>%
  select(indice, gruppo = indic_de , area, geo, TIME_PERIOD, values) %>%
  mutate(iCode = "PopIndici")





#  tavola unica indici ----

pgr <- rbindlist(list(
  nama_10r_2gdp
  , edat_lfse_33
  , lfst_r_lfp2actrtn
  , edat_lfse_16
  , lfst_r_lfe2ehour
  , isoc_sk_oja1
  , isoc_sk_oja3
  , hrst_st_rcat
  , lfst_r_lfu2ltu
  , yth_empl_130
  , lfst_r_egad
  , lfst_r_lfe2eftpt
  , lfst_r_sla_ga
  , trng_lfse_04
  # , trng_lfs_22
  , htec_emp_reg2
  , nama_10r_2rlp
  , edat_lfs_9917
  , demo_r_d2jan
  , demo_r_pjanind2
  , tgs00005
  , edat_lfse_22
  , lfst_r_lfu3rt
  , lfst_r_lfe2emprt
  , lfst_r_lfe2ehrwa
  , ilc_di11_r)
  , fill = T
  , use.names = TRUE
)

pgr[, .N, gruppo] -> gruppi

pgr <- pgr[!gruppo %in% c("No response", "Unknown")]

pgr[, iCode2 := paste0(iCode
                       , (tolower(substring(
                         gsub("persons|with|\\,| |-", "", gruppo)
                         , 1, 10))) )]

unique(pgr$iCode2)
pgr <- pgr[, .(iCode = iCode2
               , iName = paste(indice, gruppo)
               , uCode = geo
               , uName = area
               , TIME_PERIOD, values)]

pgr <- pgr[
  substring(uName, 1, 2) != "UK"]


# setnames(indici_purse, "TIME_PERIOD", "TIME_PERIOD")
saveRDS(pgr, "../datasets/indici_all_years.drs")


# pgr %>% regions::get_country_code(typology = "NUTS2")

novalori <- pgr[is.na(values), .N, .(iCode, uName)]

# pgr[, values := nafill(values, type = "locf"), .(iCode, uCode)]




# plot gruppo riferimento ----
library(ggplot2)
names(pgr)
unique(pgr$iCode)
toplot <- pgr[iCode %in% c(
  "Popolaziototal",
  "PPSprcptPpurchasing"
) & TIME_PERIOD == 2022
]

tpp <- dcast(toplot, uCode + uName ~ iCode, value.var = "values")
names(tpp) <- c("uCode", "uName", "PPS", "pop")


ven_pps = tpp[uName == "IT Veneto"]$PPS
ven_pop = tpp[uName == "IT Veneto"]$pop

ggplot(tpp) +
  aes(x = log(PPS)) +
  geom_density()

ggplot(tpp) +
  aes(x = log(pop)) +
  geom_density()

quantile((tpp$PPS), na.rm = T, p = seq(.1, 1, by = .1))
mean((tpp$PPS), na.rm = T)
IQR((tpp$PPS), na.rm = T)

tp2 <- tpp[(pop) > (ven_pop*0.3) & (PPS) >= ven_pps*(1-0.33) & PPS <= 200]

ggplot(tp2) +
  aes(x = pop, y =(PPS), label = uName) +
  geom_text() +
  geom_smooth() +
  geom_point() +
  theme_minimal()

indici <- pgr[uName %in%  tp2$uName & TIME_PERIOD == 2023]

indici[!is.na(values), .N, .(iCode)]

indici <- indici[!iCode %in% incompleto]


# codici_paese <- countrycode::codelist
# countrycode::get_dictionary()
#
# regions::all_valid_nuts_codes -> temp

saveRDS(indici, "dati/indici_2023.drs")


indici[, .N, iCode]


