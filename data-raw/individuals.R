library(magrittr)
library(nhsbsaR)
library(dplyr)
source("data-raw/individuals_utils.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the low income scheme base query
base_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM KAYGO.INT_602_LOW_INCOME_SCHEME_BASE")
)

# Filter out NA COMPOSITE_IDs
base_df <- base_df %>%
  dplyr::filter(COMPOSITE_ID != "na")

# TOTAL_INDIVIDUALS per BAND_5YEARS
individuals_by_age_band_df <- base_df %>%
  aggregate_individuals(., BAND_5YEARS, multiply_max_individuals = FALSE) %>%
  dplyr::arrange(FINANCIAL_YEAR, desc(BAND_5YEARS))

# Calculate TOTAL_INDIVIDUALS and PCT_INDIVIDUALS
individuals_by_age_band_df <- individuals_by_age_band_df %>%
  dplyr::filter(
    !(BAND_5YEARS %in% c("Co-applicant", "Unknown"))
  ) %>%
  dplyr::group_by(FINANCIAL_YEAR) %>%
  dplyr::mutate(PCT_INDIVIDUALS = TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100) %>%
  dplyr::ungroup()

# Apply SDC to total individuals and percentage of individuals
individuals_by_age_band_df <- individuals_by_age_band_df %>%
  mutate(
    SDC = ifelse(TOTAL_INDIVIDUALS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_INDIVIDUALS =
      ifelse(SDC == 1, NA_integer_, round(TOTAL_INDIVIDUALS, -1)),
    SDC_PCT_INDIVIDUALS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_INDIVIDUALS))
  ) %>%
  select(-SDC)



# TOTAL_INDIVIDUALS per CLIENTGROUP_DESC_FORMAT
individuals_by_client_group_df <- base_df %>%
  aggregate_individuals(., CLIENTGROUP_DESC_FORMAT, multiply_max_individuals = FALSE) %>%
  dplyr::mutate(CLIENTGROUP_DESC_FORMAT = gsub("_", "/", CLIENTGROUP_DESC_FORMAT)) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    CLIENTGROUP_DESC_FORMAT = dplyr::case_when(
      CLIENTGROUP_DESC_FORMAT == "Asylum Seeker (not NAS)" ~ "Asylum Seeker (not from the National Asylum Support Service)",
      TRUE ~ CLIENTGROUP_DESC_FORMAT
    )
  )

# Calculate TOTAL_INDIVIDUALS and PCT_INDIVIDUALS
individuals_by_client_group_df <- individuals_by_client_group_df %>%
  filter(
    !(CLIENTGROUP_DESC_FORMAT %in% c("Co-applicant", "Unknown"))
  ) %>%
  group_by(FINANCIAL_YEAR) %>%
  mutate(PCT_INDIVIDUALS = TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100) %>%
  ungroup()

# Apply SDC to total individuals and percentage of individuals
individuals_by_client_group_df <- individuals_by_client_group_df %>%
  mutate(
    SDC = ifelse(TOTAL_INDIVIDUALS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_INDIVIDUALS =
      ifelse(SDC == 1, NA_integer_, round(TOTAL_INDIVIDUALS, -1)),
    SDC_PCT_INDIVIDUALS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_INDIVIDUALS))
  ) %>%
  select(-SDC)


# TOTAL_INDIVIDUALS per INDEX_OF_MULT_DEPRIV_DECILE
individuals_by_imd_df <- base_df %>%
  aggregate_individuals(INDEX_OF_MULT_DEPRIV_DECILE, multiply_max_individuals = TRUE) %>%
  arrange(FINANCIAL_YEAR, INDEX_OF_MULT_DEPRIV_DECILE)

# TOTAL_INDIVIDUALS per HEALTH_DEPRIVATION_DECILE
individuals_by_health_df <- base_df %>%
  aggregate_individuals(HEALTH_DEPRIVATION_DECILE, multiply_max_individuals = TRUE) %>%
  arrange(FINANCIAL_YEAR, HEALTH_DEPRIVATION_DECILE)


# Calculate TOTAL_INDIVIDUALS and PCT_INDIVIDUALS by IMD and Health IMD
individuals_by_imd_health_df <- lowIncomeSchemeScrollytellR::individuals_by_imd_df %>%
  mutate(DEPRIVATION = "Index of Multiple Deprivation") %>%
  rename(DECILE = INDEX_OF_MULT_DEPRIV_DECILE) %>%
  rbind(
    lowIncomeSchemeScrollytellR::individuals_by_health_df %>%
      mutate(DEPRIVATION = "Health Deprivation") %>%
      rename(DECILE = HEALTH_DEPRIVATION_DECILE)
  ) %>%
  group_by(FINANCIAL_YEAR, DEPRIVATION) %>%
  mutate(PCT_INDIVIDUALS = TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100) %>%
  ungroup()

# Apply SDC to total individuals and percentage of individuals

individuals_by_imd_health_df <- individuals_by_imd_health_df %>%
  mutate(
    SDC = ifelse(TOTAL_INDIVIDUALS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_INDIVIDUALS =
      ifelse(SDC == 1, NA_integer_, round(TOTAL_INDIVIDUALS, -1)),
    SDC_PCT_INDIVIDUALS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_INDIVIDUALS))
  ) %>%
  select(-SDC)


# TOTAL_SUCCESSFUL_INDIVIDUALS per PCD_REGION_NAME
successful_individuals_by_region_df <- base_df %>%
  dplyr::filter(OUTCOME_LEVEL1 == "Successful") %>%
  aggregate_individuals(
    df = .,
    PCD_REGION_NAME,
    multiply_max_individuals = TRUE,
    total_col = "TOTAL_SUCCESSFUL_INDIVIDUALS"
  ) %>%
  dplyr::arrange(FINANCIAL_YEAR, PCD_REGION_NAME)

# TOTAL_SUCCESSFUL_INDIVIDUALS per Local Authority
successful_individuals_by_la_df <- base_df %>%
  dplyr::filter(OUTCOME_LEVEL1 == "Successful") %>%
  aggregate_individuals(
    df = .,
    PCD_LAD_NAME,
    PCD_LAD_IMD_RANK,
    multiply_max_individuals = TRUE,
    total_col = "TOTAL_SUCCESSFUL_INDIVIDUALS"
  ) %>%
  dplyr::arrange(FINANCIAL_YEAR, PCD_LAD_NAME)


usethis::use_data(individuals_by_age_band_df, overwrite = TRUE)
usethis::use_data(individuals_by_client_group_df, overwrite = TRUE)
usethis::use_data(individuals_by_imd_health_df, overwrite = TRUE)
usethis::use_data(successful_individuals_by_region_df, overwrite = TRUE)
usethis::use_data(successful_individuals_by_la_df, overwrite = TRUE)

DBI::dbDisconnect(con)
