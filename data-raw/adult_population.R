library(magrittr)
library(nhsbsaR)


# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the ONS population table
adult_population_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM KAYGO.INT_602_LA_ONS_POP")
)

# ONS over 16 population
adult_population_df <- adult_population_df %>%
  dplyr::mutate(
    FINANCIAL_YEAR = dplyr::case_when(
      POPULATION_YEAR == 2015 ~ "2015/16",
      POPULATION_YEAR == 2016 ~ "2016/17",
      POPULATION_YEAR == 2017 ~ "2017/18",
      POPULATION_YEAR == 2018 ~ "2018/19",
      POPULATION_YEAR == 2019 ~ "2019/20"
    )
  ) %>%
  dplyr::select(
    FINANCIAL_YEAR,
    PCD_LAD_NAME = LAD_NAME,
    PCD_REGION_NAME = RGN_NAME,
    TOTAL_ADULT_POPULATION = ONS_POPULATION
  ) %>%
  dplyr::collect()


usethis::use_data(adult_population_df, overwrite = TRUE)


DBI::dbDisconnect(con)
