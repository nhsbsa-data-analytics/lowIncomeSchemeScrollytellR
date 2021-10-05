library(magrittr)


# Set up connection to the DB
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Oracle in OraClient19Home1",
  DBQ = Sys.getenv("DB_CONNECTION"),
  UID = Sys.getenv("DB_USERNAME"),
  PWD = Sys.getenv("DB_PASSWORD")
)

# Create a lazy table from the low income scheme base query
base_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM KAYGO.INT_602_LOW_INCOME_SCHEME_BASE")
)

# Applications
applications_df <- base_df %>%
  dplyr::group_by(
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
  dplyr::summarise(TOTAL_APPLICATIONS = n()) %>%
  dplyr::ungroup() %>%
  dplyr::collect() %>%
  dplyr::mutate(

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


usethis::use_data(applications_df, overwrite = TRUE)


DBI::dbDisconnect(con)
