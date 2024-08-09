library(dplyr)
library(dbplyr)

# Define a function to add a row to a dataframe with the count of co-applicants
# (we use this whenever we aren't multiplying out by MAX_INDIVIDUALS_COVERED)
reformat_co_applicants <- function(df, time_col) {
  # Count the co-applicants
  co_applicants <- df %>%
    filter(MAX_INDIVIDUALS_COVERED == 2) %>%
    group_by(.data[[time_col]]) %>%
    summarise(TOTAL_APPLICANTS = sum(TOTAL_APPLICANTS)) %>%
    select(.data[[time_col]], TOTAL_APPLICANTS)

  # Get the dummy columns we need to add
  non_dummy_cols <- c(time_col, "MAX_INDIVIDUALS_COVERED", "TOTAL_APPLICANTS")
  dummy_cols <- colnames(df)[!(colnames(df) %in% non_dummy_cols)]

  # Add new columns
  for (c in dummy_cols) {
    co_applicants <- co_applicants %>%
      mutate({{ c }} := "Co-applicant")
  }

  # Remove the MAX_INDIVIDUALS_COVERED column and bind the rows
  df %>%
    select(-MAX_INDIVIDUALS_COVERED) %>%
    rbind(co_applicants)
}

# Define a function to aggregate individual level data
aggregate_individuals <- function(df,
                                  ...,
                                  multiply_max_individuals,
                                  time_col = "FINANCIAL_YEAR",
                                  total_col = "TOTAL_INDIVIDUALS") {
  # Get the max individuals covered and count the total applicants
  df <- df %>%
    group_by(.data[[time_col]], ..., COMPOSITE_ID) %>%
    summarise(MAX_INDIVIDUALS_COVERED = max(INDIVIDUALS_COVERED)) %>%
    ungroup() %>%
    group_by(.data[[time_col]], ..., MAX_INDIVIDUALS_COVERED) %>%
    summarise(TOTAL_APPLICANTS = n_distinct(COMPOSITE_ID)) %>%
    ungroup()

  # Deal with max individuals (whether multiplying or reformatting)
  if (multiply_max_individuals) {
    df %>%
      group_by(.data[[time_col]], ...) %>%
      summarise(
        {{ total_col }} := sum(MAX_INDIVIDUALS_COVERED * TOTAL_APPLICANTS)
      ) %>%
      ungroup() %>%
      collect()
  } else {
    df %>%
      collect() %>%
      reformat_co_applicants(., time_col) %>%
      group_by(.data[[time_col]], ...) %>%
      summarise({{ total_col }} := sum(TOTAL_APPLICANTS)) %>%
      ungroup()
  }
}
