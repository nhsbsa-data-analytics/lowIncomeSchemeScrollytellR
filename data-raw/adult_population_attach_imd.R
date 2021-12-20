library(magrittr)
library(nhsbsaR)
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography mapping table
geography_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_GEOGRAPHY_MAPPING"))

# Create a lazy table from the ONS population table
ons_pop <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_POPULATION"))

# Create a lazy table for IMD data
imd_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_INDEX_OF_MULTIPLE_DEPRIVATION"))


adult_population_imd_db <- ons_pop %>%
  # filter only for 16+ LSOA
  filter(GEOGRAPHY_TYPE == "LSOA" & AGE >= 16) %>%
  select(
    POPULATION_YEAR,
    GEOGRAPHY_ONS_CODE, # LSOA
    TOTAL_POPULATION
  ) %>%
  # LSOA_WARD2020
  inner_join(
    y = geography_db %>%
      filter(RELATIONSHIP == "LSOA_WARD2020") %>%
      select(CHILD_ONS_CODE, PARENT_ONS_CODE),
    by = c("GEOGRAPHY_ONS_CODE" = "CHILD_ONS_CODE")
  ) %>%
  # WARD2020_LAD2020
  select(POPULATION_YEAR, TOTAL_POPULATION, LSOA_CODE = GEOGRAPHY_ONS_CODE, PARENT_ONS_CODE) %>%
  inner_join(
    y = geography_db %>%
      filter(RELATIONSHIP == "WARD2020_LAD2020") %>%
      select(CHILD_ONS_CODE, PCD_LAD_NAME = PARENT_NAME, PCD_LAD_CODE = PARENT_ONS_CODE),
    by = c("PARENT_ONS_CODE" = "CHILD_ONS_CODE")
  ) %>%
  # LAD2020_REG2020
  inner_join(
    y = geography_db %>%
      filter(RELATIONSHIP == "LAD2020_REG2020") %>%
      select(CHILD_ONS_CODE, PCD_REGION_NAME = PARENT_NAME, PCD_REGION_CODE = PARENT_ONS_CODE),
    by = c("PCD_LAD_CODE" = "CHILD_ONS_CODE")
  ) %>%
  # Index of multiple deprivation
  left_join(
    y = imd_db %>%
      filter(IMD_YEAR == 2019) %>%
      select(LSOA_CODE, INDEX_OF_MULT_DEPRIV_DECILE)
  )


# ONS over 16 population
adult_population_df <- adult_population_imd_db %>%
  group_by(POPULATION_YEAR, PCD_LAD_CODE, PCD_LAD_NAME, PCD_REGION_CODE, PCD_REGION_NAME) %>%
  summarise(TOTAL_ADULT_POPULATION = sum(TOTAL_POPULATION)) %>%
  ungroup() %>%
  mutate(
    FINANCIAL_YEAR = dplyr::case_when(
      POPULATION_YEAR == 2015 ~ "2015/16",
      POPULATION_YEAR == 2016 ~ "2016/17",
      POPULATION_YEAR == 2017 ~ "2017/18",
      POPULATION_YEAR == 2018 ~ "2018/19",
      POPULATION_YEAR == 2019 ~ "2019/20",
      POPULATION_YEAR == 2020 ~ "2020/21"
    )
  ) %>%
  dplyr::select(
    FINANCIAL_YEAR,
    PCD_LAD_NAME,
    PCD_REGION_NAME,
    TOTAL_ADULT_POPULATION
  ) %>%
  collect() %>%
  arrange(FINANCIAL_YEAR)

# IMD decile percentage by LA
imd_decile_df <- adult_population_imd_db %>%
  # count decile by LA
  # data is single age at the moment so distinct first
  distinct(LSOA_CODE, INDEX_OF_MULT_DEPRIV_DECILE, PCD_LAD_NAME, PCD_REGION_NAME) %>%
  group_by(PCD_LAD_NAME, PCD_REGION_NAME, INDEX_OF_MULT_DEPRIV_DECILE) %>% # counting IMD decile in LAD
  summarise(IMD_DECILE_COUNT = n()) %>%
  ungroup() %>%
  group_by(PCD_LAD_NAME, PCD_REGION_NAME) %>%
  mutate(IMD_DECILE_P = IMD_DECILE_COUNT / sum(IMD_DECILE_COUNT) * 100) %>%
  ungroup() %>%
  collect()



usethis::use_data(adult_population_df, overwrite = TRUE)
usethis::use_data(imd_decile_df, overwrite = TRUE)


DBI::dbDisconnect(con)
