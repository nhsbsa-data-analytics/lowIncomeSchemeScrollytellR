library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the low income scheme base query
base_df <- con %>%
  tbl(from = in_schema("KAYGO", "INT_602_LOW_INCOME_SCHEME_BASE"))

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
applications_overall_df <- applications_df %>%
  group_by(FINANCIAL_YEAR) %>%
  summarise(TOTAL_APPLICATIONS = round(sum(TOTAL_APPLICATIONS), -1)) %>%
  ungroup()



# Outcome aggregate and apply rounding
applications_outcome_df <- applications_df %>%
  filter(OUTCOME_LEVEL2 != "Ongoing") %>%
  group_by(FINANCIAL_YEAR, OUTCOME_LEVEL2) %>%
  summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
  ungroup() %>%
  group_by(FINANCIAL_YEAR) %>%
  mutate(
    PCT_OUTCOMES = janitor::round_half_up(TOTAL_APPLICATIONS / sum(TOTAL_APPLICATIONS) * 100, 1),
    TOTAL_APPLICATIONS = round(TOTAL_APPLICATIONS, -1)
  ) %>%
  ungroup()



# Aggregate student
applications_student_df <- applications_df %>%
  mutate(TYPE = ifelse(CLIENTGROUP_DESC == "Student", "Student", "Non-Student")) %>%
  group_by(FINANCIAL_YEAR, TYPE) %>%
  summarise(TOTAL_APPLICATIONS = round(sum(TOTAL_APPLICATIONS), -1)) %>%
  ungroup()


# Outcome aggregate and apply rounding for students
applications_outcome_student_df <- applications_df %>%
  filter(OUTCOME_LEVEL2 != "Ongoing") %>%
  droplevels() %>%
  mutate(TYPE = ifelse(CLIENTGROUP_DESC == "Student", "Student", "Non-Student")) %>%
  group_by(TYPE, OUTCOME_LEVEL2, FINANCIAL_YEAR) %>%
  summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
  ungroup() %>%
  group_by(TYPE, FINANCIAL_YEAR) %>%
  mutate(
    PCT_OUTCOMES = janitor::round_half_up(TOTAL_APPLICATIONS / sum(TOTAL_APPLICATIONS) * 100, 1),
    TOTAL_APPLICATIONS = round(TOTAL_APPLICATIONS, -1)
  ) %>%
  ungroup()




usethis::use_data(applications_df, overwrite = TRUE)
usethis::use_data(applications_overall_df, overwrite = TRUE)
usethis::use_data(applications_outcome_df, overwrite = TRUE)
usethis::use_data(applications_student_df, overwrite = TRUE)
usethis::use_data(applications_outcome_student_df, overwrite = TRUE)

DBI::dbDisconnect(con)
