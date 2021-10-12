library(magrittr)
library(nhsbsaR)
source("data-raw/individuals_utils.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the low income scheme base query
base_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM KAYGO.INT_602_LOW_INCOME_SCHEME_BASE")
)

# Filter out NA COMPOSITE_IDs & to Student CLIENTGROUP_DESC
student_base_df <- base_df %>%
  dplyr::filter(COMPOSITE_ID != "na" & CLIENTGROUP_DESC == "Student")

# TOTAL_SUCCESSFUL_INDIVIDUALS per PCD_REGION_NAME
successful_student_individuals_by_region_df <- student_base_df %>%
  dplyr::filter(OUTCOME_LEVEL1 == "Successful") %>%
  aggregate_individuals(
    df = .,
    PCD_REGION_NAME,
    multiply_max_individuals = TRUE,
    time_col = "ACADEMIC_YEAR",
    total_col = "TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS"
  ) %>%
  dplyr::arrange(ACADEMIC_YEAR, PCD_REGION_NAME)


usethis::use_data(successful_student_individuals_by_region_df, overwrite = TRUE)


DBI::dbDisconnect(con)
