library(magrittr)
library(nhsbsaR)
library(dplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the low income scheme base query
base_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM KAYGO.INT_602_LOW_INCOME_SCHEME_BASE")
)

# Applications
applications_df <- base_df %>%
  group_by(
    FINANCIAL_YEAR,
    ACADEMIC_YEAR,
    APPLICATION_MONTH,
    PCD_REGION_NAME,
    CLIENTGROUP_DESC,
    OUTCOME_LEVEL1,
    OUTCOME_LEVEL2,
    HELP_WITH_SIGHT_TEST,
    HELP_WITH_BAND_1,
    HELP_WITH_BAND_2,
    HELP_WITH_BAND_3
  ) %>%
  summarise(TOTAL_APPLICATIONS = n()) %>%
  ungroup() %>%
  collect() %>%
  mutate(

    # Order outcomes from reject to approve
    OUTCOME_LEVEL2 = ordered(
      x = OUTCOME_LEVEL2,
      levels = c(
        "Over income limit",
        "Over capital limit",
        "Already receiving benefits",
        "Withdrawn/Abandoned",
        "Ongoing",
        "Partial benefit",
        "Full benefit"
      )
    )
  )


# Aggregate
applications_agg_df <- applications_df %>%
  group_by(FINANCIAL_YEAR) %>%
  summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
  ungroup()

# Apply SDC
applications_agg_df <- applications_agg_df %>%
  mutate(
    SDC = ifelse(TOTAL_APPLICATIONS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_APPLICATIONS =
      ifelse(SDC == 1, NA_integer_, round(TOTAL_APPLICATIONS, -1))
  ) %>%
  select(-SDC)

usethis::use_data(applications_df, overwrite = TRUE)
usethis::use_data(applications_agg_df, overwrite = TRUE)


DBI::dbDisconnect(con)
