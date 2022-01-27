library(dbplyr)
library(dbplyr)
source("data-raw/individuals_utils.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the low income scheme base query
base_df <- con %>%
  tbl(from = in_schema("KAYGO", "INT_602_LOW_INCOME_SCHEME_BASE"))

# Filter out NA COMPOSITE_IDs
base_df <- base_df %>%
  filter(COMPOSITE_ID != "na")

# TOTAL_INDIVIDUALS per BAND_5YEARS
individuals_by_age_band_df <- base_df %>%
  aggregate_individuals(., BAND_5YEARS, multiply_max_individuals = FALSE) %>%
  arrange(FINANCIAL_YEAR, desc(BAND_5YEARS))

# Calculate TOTAL_INDIVIDUALS and PCT_INDIVIDUALS
# It is overall figure so all values are greater than 5 (don't need to do recode step)
# calculate raw value and change to janitor::round_half_up
individuals_by_age_band_df <- individuals_by_age_band_df %>%
  filter(
    !(BAND_5YEARS %in% c("Co-applicant", "Unknown"))
  ) %>%
  group_by(FINANCIAL_YEAR) %>%
  mutate(
    PCT_INDIVIDUALS = janitor::round_half_up(TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100, 1),
    TOTAL_INDIVIDUALS = round(TOTAL_INDIVIDUALS, -1)
  ) %>%
  ungroup()



# TOTAL_INDIVIDUALS per CLIENTGROUP_DESC_FORMAT
individuals_by_client_group_df <- base_df %>%
  aggregate_individuals(., CLIENTGROUP_DESC_FORMAT, multiply_max_individuals = FALSE) %>%
  mutate(CLIENTGROUP_DESC_FORMAT = gsub("_", "/", CLIENTGROUP_DESC_FORMAT)) %>%
  collect() %>%
  mutate(
    CLIENTGROUP_DESC_FORMAT = case_when(
      CLIENTGROUP_DESC_FORMAT == "Asylum Seeker (not NAS)" ~ "Asylum Seeker (not from the National Asylum Support Service)",
      TRUE ~ CLIENTGROUP_DESC_FORMAT
    )
  )

# Calculate TOTAL_INDIVIDUALS and PCT_INDIVIDUALS
# Apply round numbers
individuals_by_client_group_df <- individuals_by_client_group_df %>%
  filter(
    !(CLIENTGROUP_DESC_FORMAT %in% c("Co-applicant", "Unknown"))
  ) %>%
  group_by(FINANCIAL_YEAR) %>%
  mutate(
    PCT_INDIVIDUALS = janitor::round_half_up(TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100, 1),
    TOTAL_INDIVIDUALS = round(TOTAL_INDIVIDUALS, -1)
  ) %>%
  ungroup()


# TOTAL_INDIVIDUALS per INDEX_OF_MULT_DEPRIV_DECILE
individuals_by_imd_df <- base_df %>%
  aggregate_individuals(INDEX_OF_MULT_DEPRIV_DECILE, multiply_max_individuals = TRUE) %>%
  arrange(FINANCIAL_YEAR, INDEX_OF_MULT_DEPRIV_DECILE)

# TOTAL_INDIVIDUALS per HEALTH_DEPRIVATION_DECILE
individuals_by_health_df <- base_df %>%
  aggregate_individuals(HEALTH_DEPRIVATION_DECILE, multiply_max_individuals = TRUE) %>%
  arrange(FINANCIAL_YEAR, HEALTH_DEPRIVATION_DECILE)


# Calculate TOTAL_INDIVIDUALS and PCT_INDIVIDUALS by IMD and Health IMD
individuals_by_imd_health_df <- individuals_by_imd_df %>%
  mutate(DEPRIVATION = "Index of Multiple Deprivation") %>%
  rename(DECILE = INDEX_OF_MULT_DEPRIV_DECILE) %>%
  rbind(
    individuals_by_health_df %>%
      mutate(DEPRIVATION = "Health Deprivation") %>%
      rename(DECILE = HEALTH_DEPRIVATION_DECILE)
  ) %>%
  group_by(FINANCIAL_YEAR, DEPRIVATION) %>%
  mutate(
    PCT_INDIVIDUALS = janitor::round_half_up(TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100, 1),
    TOTAL_INDIVIDUALS = round(TOTAL_INDIVIDUALS, -1)
  ) %>%
  ungroup()



# TOTAL_SUCCESSFUL_INDIVIDUALS per PCD_REGION_NAME
successful_individuals_by_region_df <- base_df %>%
  filter(OUTCOME_LEVEL1 == "Successful") %>%
  aggregate_individuals(
    df = .,
    PCD_REGION_NAME,
    multiply_max_individuals = TRUE,
    total_col = "TOTAL_SUCCESSFUL_INDIVIDUALS"
  ) %>%
  arrange(FINANCIAL_YEAR, PCD_REGION_NAME)

# TOTAL_SUCCESSFUL_INDIVIDUALS per Local Authority
successful_individuals_by_la_df <- base_df %>%
  filter(OUTCOME_LEVEL1 == "Successful") %>%
  aggregate_individuals(
    df = .,
    PCD_LAD_NAME,
    PCD_LAD_IMD_RANK,
    multiply_max_individuals = TRUE,
    total_col = "TOTAL_SUCCESSFUL_INDIVIDUALS"
  ) %>%
  arrange(FINANCIAL_YEAR, PCD_LAD_NAME)


usethis::use_data(individuals_by_age_band_df, overwrite = TRUE)
usethis::use_data(individuals_by_client_group_df, overwrite = TRUE)
usethis::use_data(individuals_by_imd_health_df, overwrite = TRUE)
usethis::use_data(successful_individuals_by_region_df, overwrite = TRUE)
usethis::use_data(successful_individuals_by_la_df, overwrite = TRUE)

DBI::dbDisconnect(con)
