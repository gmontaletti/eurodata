#' Dataset transformation specifications
#'
#' A list-of-lists defining how each Eurostat dataset should be filtered,
#' remapped, and transformed into the common eurodata schema. Each element
#' contains: `dataset_id`, `indice`, `iCode`, `gruppo_col` or
#' `gruppo_literal`, `filters`, `filter_in`, `filter_not_in`, `filter_na`,
#' `remap`, `substring_transform`, `aggregate`, and `area_formula`.
#'
#' @export
dataset_specs <- list(
  # 1. edat_lfs_9917 -----
  list(
    dataset_id = "edat_lfs_9917",
    indice = "Pct 25-64 anni per titolo di studio",
    iCode = "Educ2564P",
    gruppo_col = "isced11",
    filters = list(
      sex = "Total",
      c_birth = "Total",
      age = "From 25 to 64 years"
    ),
    filter_in = list(
      isced11 = c(
        "Less than primary, primary and lower secondary education (levels 0-2)",
        "Tertiary education (levels 5-8)",
        "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)"
      )
    )
  ),

  # 2. edat_lfse_16 -----
  list(
    dataset_id = "edat_lfse_16",
    indice = "Pct 18-24 abbandono studi",
    iCode = "AbbaStudP",
    gruppo_col = "sex"
  ),

  # 3. edat_lfse_33 -----
  list(
    dataset_id = "edat_lfse_33",
    indice = "Occ entro 3 anni dal titolo",
    iCode = "Occ3AnniP",
    gruppo_col = "isced11",
    filters = list(
      sex = "Total",
      duration = "3 years or less",
      age = "From 18 to 34 years"
    ),
    filter_in = list(
      isced11 = c(
        "Tertiary education (levels 5-8)",
        "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)"
      )
    )
  ),

  # 4. edat_lfse_22 -----
  list(
    dataset_id = "edat_lfse_22",
    indice = "NEET",
    iCode = "NEETP",
    gruppo_col = "age",
    filters = list(sex = "Total")
  ),

  # 5. hrst_st_rcat -----
  list(
    dataset_id = "hrst_st_rcat",
    indice = "Pct in sci e tech",
    iCode = "SCItecPOPP",
    gruppo_col = "category",
    filters = list(unit = "Percentage of total population"),
    remap = list(
      col = "category",
      mapping = list(
        "Persons employed in science and technology" = "OccSciTech",
        "Persons with tertiary education (ISCED)" = "PersTerEdu",
        "Persons with tertiary education (ISCED) and employed in science and technology" = "TeduAndStech",
        "Persons with tertiary education (ISCED) and/or employed in science and technology" = "TeduAorStech",
        "Scientists and engineers" = "ScienEngin"
      )
    )
  ),

  # 6. htec_emp_reg2 -----
  list(
    dataset_id = "htec_emp_reg2",
    indice = "Pct occupazione totale",
    iCode = "OcSetP",
    gruppo_col = "nace_r2",
    filters = list(
      unit = "Percentage of total employment",
      sex = "Total"
    ),
    filter_not_in = list(
      nace_r2 = c(
        "Total - all NACE activities",
        "Unknown NACE activity",
        "Other NACE activities"
      )
    ),
    remap = list(
      col = "nace_r2",
      mapping = list(
        "Financial and insurance activities" = "FinInscur",
        "Financial and insurance activities; real estate activities" = "FinRealEstate",
        "High-technology manufacturing" = "HiTecManif",
        "High-technology sectors (high-technology manufacturing and knowledge-intensive high-technology services)" = "HiTecAll",
        "Knowledge-intensive high-technology services" = "KiHtServices",
        "Knowledge-intensive market services (except financial intermediation and high-technology services)" = "KiMarkServices"
      )
    )
  ),

  # 7. ilc_di11_r -----
  list(
    dataset_id = "ilc_di11_r",
    indice = "redditi ratio S80_S20",
    iCode = "Income",
    gruppo_literal = "Rat8020"
  ),

  # 8. isoc_sk_oja1 -----
  list(
    dataset_id = "isoc_sk_oja1",
    indice = "ICT annunci",
    iCode = "ICTVacaP",
    gruppo_col = "unit",
    aggregate = TRUE
  ),

  # 9. isoc_sk_oja3 -----
  list(
    dataset_id = "isoc_sk_oja3",
    indice = "ICT var % annunci",
    iCode = "ICTVacVP",
    gruppo_col = "unit",
    aggregate = TRUE,
    area_formula = "geo"
  ),

  # 10. lfst_r_egad -----
  list(
    dataset_id = "lfst_r_egad",
    indice = "Durata contratti",
    iCode = "DurContrP",
    gruppo_col = "duration",
    filters = list(
      unit = "Percentage",
      sex = "Total",
      age = "From 20 to 64 years"
    )
  ),

  # 11. lfst_r_lfe2eftpt -----
  list(
    dataset_id = "lfst_r_lfe2eftpt",
    indice = "Migliaia di addetti",
    iCode = "FTimPtimM",
    gruppo_col = "workTIME_PERIOD",
    filters = list(
      wstatus = "Employed persons",
      sex = "Total",
      age = "From 20 to 64 years"
    )
  ),

  # 12. lfst_r_lfe2ehour -----
  list(
    dataset_id = "lfst_r_lfe2ehour",
    indice = "Ore lavorate di solito",
    iCode = "OreLavSol",
    gruppo_col = "age",
    filters = list(sex = "Total")
  ),

  # 13. lfst_r_lfe2ehrwa -----
  list(
    dataset_id = "lfst_r_lfe2ehrwa",
    indice = "Ore attuali",
    iCode = "OreLActual",
    gruppo_col = "age",
    filters = list(sex = "Total")
  ),

  # 14. lfst_r_lfu2ltu -----
  list(
    dataset_id = "lfst_r_lfu2ltu",
    indice = "Disocc. lungo termine % forze lavoro",
    iCode = "DisLungaP",
    gruppo_col = "isced11",
    filters = list(
      unit = "Percentage of population in the labour force",
      sex = "Total",
      age = "From 20 to 64 years"
    )
  ),

  # 15. lfst_r_lfu3rt -----
  list(
    dataset_id = "lfst_r_lfu3rt",
    indice = "Tasso di disoccupazione",
    iCode = "DisTass",
    gruppo_col = "isced11",
    filters = list(
      sex = "Total",
      age = "From 20 to 64 years"
    )
  ),

  # 16. lfst_r_lfe2emprt -----
  list(
    dataset_id = "lfst_r_lfe2emprt",
    indice = "Tasso di occupazione",
    iCode = "OccTass",
    gruppo_col = "age",
    filters = list(sex = "Total")
  ),

  # 17. lfst_r_lfp2actrtn -----
  list(
    dataset_id = "lfst_r_lfp2actrtn",
    indice = "Tasso di attivita",
    iCode = "AttTass",
    gruppo_col = "isced11",
    filters = list(
      sex = "Total",
      age = "From 15 to 64 years",
      citizen = "Total"
    )
  ),

  # 18. lfst_r_sla_ga -----
  list(
    dataset_id = "lfst_r_sla_ga",
    indice = "Slack % su fl estesa",
    iCode = "SlackFLEP",
    gruppo_col = "age",
    filters = list(
      unit = "Percentage of extended labour force",
      sex = "Total"
    )
  ),

  # 19. trng_lfse_04 -----
  list(
    dataset_id = "trng_lfse_04",
    indice = "Partecipazione % formazione u4s",
    iCode = "Formz4sett",
    gruppo_col = "age",
    filters = list(sex = "Total")
  ),

  # 20. nama_10r_2gdp -----
  list(
    dataset_id = "nama_10r_2gdp",
    indice = "PPS procapite indice",
    iCode = "PPSprcptP",
    gruppo_col = "unit",
    filters = list(
      unit = "Purchasing power standard (PPS, EU27 from 2020), per inhabitant in percentage of the EU27 (from 2020) average"
    )
  ),

  # 21. nama_10r_2rlp -----
  list(
    dataset_id = "nama_10r_2rlp",
    indice = "indice produttivita reale lavoro",
    iCode = "ProdRealI",
    gruppo_col = "na_item",
    filters = list(unit = "Index, 2015=100"),
    substring_transform = list(
      col = "na_item",
      start = 30L,
      stop = 40L
    )
  ),

  # 22. yth_empl_130 -----
  list(
    dataset_id = "yth_empl_130",
    indice = "Disoccupazione giovanile lungo termine P",
    iCode = "DisGioLuP",
    gruppo_col = "sex"
  ),

  # 23. demo_r_d2jan -----
  list(
    dataset_id = "demo_r_d2jan",
    indice = "Popolazione 1 gennaio",
    iCode = "Popolazio",
    gruppo_col = "sex",
    filters = list(age = "Total")
  ),

  # 24. demo_r_pjanind2 -----
  list(
    dataset_id = "demo_r_pjanind2",
    indice = "Indici della popolazione",
    iCode = "PopIndici",
    gruppo_col = "indic_de",
    filter_in = list(
      indic_de = c(
        "Median age of population",
        "Women per 100 men",
        "Age dependency ratio, 1st variant (population 0 to 14 years and 65 years or over to population 15 to 64 years)",
        "Old-age dependency ratio 1st variant (population 65 years or over to population 15 to 64 years)",
        "Young-age dependency ratio 1st variant (population 0 to 14 years to population 15 to 64 years)"
      )
    ),
    filter_na = TRUE
  )
)

# Set names for easy access by dataset_id
names(dataset_specs) <- vapply(dataset_specs, `[[`, character(1), "dataset_id")
