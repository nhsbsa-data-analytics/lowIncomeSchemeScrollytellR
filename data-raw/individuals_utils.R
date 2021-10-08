# Define a function to add a row to a dataframe with the count of co-applicants
# (we use this whenever we aren't multiplying out by MAX_INDIVIDUALS_COVERED)
reformat_co_applicants <- function(df, time_col) {

  # Count the co-applicants
  co_applicants <- df %>%
    dplyr::filter(MAX_INDIVIDUALS_COVERED == 2) %>%
    dplyr::group_by(!!as.name(time_col)) %>%
    dplyr::summarise(TOTAL_APPLICANTS = sum(TOTAL_APPLICANTS)) %>%
    dplyr::select(!!as.name(time_col), TOTAL_APPLICANTS)

  # Get the dummy columns we need to add
  non_dummy_cols <- c(time_col, "MAX_INDIVIDUALS_COVERED", "TOTAL_APPLICANTS")
  dummy_cols <- colnames(df)[!(colnames(df) %in% non_dummy_cols)]

  # Add new columns
  for (c in dummy_cols) {
    co_applicants <- co_applicants %>%
      dplyr::mutate({{ c }} := "Co-applicant")
  }

  # Remove the MAX_INDIVIDUALS_COVERED column and bind the rows
  df %>%
    dplyr::select(-MAX_INDIVIDUALS_COVERED) %>%
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
    dplyr::group_by(!!as.name(time_col), ..., COMPOSITE_ID) %>%
    dplyr::summarise(MAX_INDIVIDUALS_COVERED = max(INDIVIDUALS_COVERED)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!as.name(time_col), ..., MAX_INDIVIDUALS_COVERED) %>%
    dplyr::summarise(TOTAL_APPLICANTS = dplyr::n_distinct(COMPOSITE_ID)) %>%
    dplyr::ungroup()

  # Deal with max individuals (whether multiplying or reformatting)
  if (multiply_max_individuals) {
    df %>%
      dplyr::group_by(!!as.name(time_col), ...) %>%
      dplyr::summarise({{ total_col }} := sum(MAX_INDIVIDUALS_COVERED * TOTAL_APPLICANTS)) %>%
      dplyr::ungroup() %>%
      dplyr::collect()
  } else {
    df %>%
      dplyr::collect() %>%
      reformat_co_applicants(., time_col) %>%
      dplyr::group_by(!!as.name(time_col), ...) %>%
      dplyr::summarise({{ total_col }} := sum(TOTAL_APPLICANTS)) %>%
      dplyr::ungroup()
  }
}
