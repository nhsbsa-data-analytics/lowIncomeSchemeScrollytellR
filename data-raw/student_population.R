library(magrittr)


# Download the raw data-files by using the download link from:
# https://www.hesa.ac.uk/data-and-analysis/students/table-1
tmp <- tempfile()
download.file(
  url = "https://www.hesa.ac.uk/data-and-analysis/students/table-1.csv",
  destfile = tmp,
  mode = "wb"
)

# Unzip the data-files
unzip(zipfile = tmp, exdir = tempdir())

# Extract the raw df paths
raw_student_df_paths <- list.files(
  path = file.path(tempdir(), "Table-1-051-054"),
  pattern = "*.csv",
  full.names = TRUE
)

# Read all of the dataframes and stack them together
raw_student_df <- purrr::map_df(
  .x = raw_student_df_paths,
  .f = function(x) {
    readr::read_csv(
      file = x,
      skip = 14,
      col_types = readr::cols(.default = "c")
    )
  }
)

# Remove the raw data-files
unlink(
  x = c("raw.zip", "Table-1-051", "Table-1-051-054", "README.txt"),
  recursive = TRUE
)

# Process the data to regional
student_population_df <- raw_student_df %>%
  # Lowercase and underscrore the column names
  janitor::clean_names() %>%
  # First filter Country of HE provider = England
  # Exclude Region = 'All'
  # and select remaining field 'All
  dplyr::filter(
    he_provider != "Total",
    country_of_he_provider == "England",
    region_of_he_provider != "All",
    first_year_marker == "All",
    level_of_study == "All",
    mode_of_study == "All",
    category_marker == "Total",
    category == "Total"
  ) %>%
  dplyr::mutate(number = as.numeric(number)) %>%
  dplyr::rename(ACADEMIC_YEAR = academic_year) %>%
  # Aggregate to regional level
  dplyr::group_by(ACADEMIC_YEAR, PCD_REGION_NAME = region_of_he_provider) %>%
  dplyr::summarise(TOTAL_STUDENT_POPULATION = sum(number)) %>%
  dplyr::ungroup()


usethis::use_data(student_population_df, overwrite = TRUE)

unlink(x = tempdir(), recursive = TRUE)
