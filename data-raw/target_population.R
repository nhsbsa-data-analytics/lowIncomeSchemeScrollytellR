library(magrittr)


# Set the API key
statxplorer::set_api_key(Sys.getenv("STATXPLORE_API_KEY"))

# Read in the query file
target_population_query <- readr::read_file("data-raw/target_population.json")

# Fetch the result from the Stat-Xplorer ODP API
target_population_result <- statxplorer::fetch_table(target_population_query)

# Result is a list and the dataframe is the first and only element
raw_target_population_df <- target_population_result$dfs[[1]]

# Data is in long format, process it into our final data-set
target_population_df <- raw_target_population_df %>%
  # Lowercase and underscore the column names
  janitor::clean_names() %>%
  # Manually shorten some of the longer column names
  dplyr::rename(
    FINANCIAL_YEAR = financial_year,
    PCD_REGION_NAME = government_office_region_of_the_household_in_the_united_kingdom_please_calculate_three_year_averages_click_on_i_for_the_correct_method,
    LOW_INCOME = x60_per_cent_of_median_net_household_income_ahc_in_latest_prices,
    GENDER = gender_of_the_individual,
    TYPE = type_of_individual,
    AGE_BAND = age_band_of_the_individual,
    UC_OR_EQUIV = universal_credit_or_equivalent_received_by_the_family,
    PC = pension_credit_pc_received_by_the_family,
    SAVINGS = savings_and_investments_of_adults_in_the_family,
    TOTAL_POPULATION = weighted_sum_of_income_distribution_whole_population
  ) %>%
  # Recode values for attribute columns
  dplyr::mutate(
    # "In ..." -> 1 and "Not in ..." -> 0
    LOW_INCOME = ifelse(LOW_INCOME == "In low income (below threshold)", 1, 0),
    UC_OR_EQUIV = ifelse(UC_OR_EQUIV == "In receipt", 1, 0),
    PC = ifelse(PC == "In receipt", 1, 0),
    # Indicate whether savings are over £16k threshold (must aggregate afterwards
    # this as we are grouping the column)
    SAVINGS = ifelse(SAVINGS %in% c("£16,000 but less than £20,000", "£20,000 or more"), 1, 0)
  ) %>%
  dplyr::group_by(dplyr::across(-c(TOTAL_POPULATION))) %>%
  dplyr::summarise(TOTAL_POPULATION = sum(TOTAL_POPULATION)) %>%
  dplyr::ungroup() %>%
  # Convert financial year from YYYY-YY to YYYY/YY
  dplyr::mutate(FINANCIAL_YEAR = gsub("-", "/", FINANCIAL_YEAR)) %>%
  # Remove the code from region
  dplyr::mutate(
    PCD_REGION_NAME = gsub("\\s*\\([^\\)]+\\)", "", PCD_REGION_NAME)
  ) %>%
  # Recode one of the regions
  dplyr::mutate(
    PCD_REGION_NAME = ifelse(PCD_REGION_NAME == "East", "East of England", PCD_REGION_NAME)
  )


usethis::use_data(target_population_df, overwrite = TRUE)
