library(magrittr)


# Set up connection to the DB
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Oracle in OraClient19Home1",
  DBQ = Sys.getenv("DB_CONNECTION"),
  UID = Sys.getenv("DB_USERNAME"),
  PWD = Sys.getenv("DB_PASSWORD")
)

# Create a lazy table from the DALF_REF count deprivation by LSOA
deprivation_count_df <- dplyr::tbl(
  src = con,
  from = dbplyr::sql("SELECT * FROM KAYGO.INT_602_DEPRIVATION_COUNT")
)

# Option 1: IMD decile
imd_decile_df <- deprivation_count_df %>% 
  dplyr::group_by(INDEX_OF_MULT_DEPRIV_DECILE, PCD_LAD_NAME, PCD_REGION_NAME) %>% 
  dplyr::summarise(IMD_DECILE_COUNT_LAD = sum(IMD_DECILE_COUNT)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(PCD_LAD_NAME) %>% 
  dplyr::mutate(IMD_DECILE_P = IMD_DECILE_COUNT_LAD / sum(IMD_DECILE_COUNT_LAD) * 100) %>% 
  dplyr::arrange(PCD_LAD_NAME,INDEX_OF_MULT_DEPRIV_DECILE) %>% 
  dplyr::ungroup() %>% 
  dplyr::collect()



# Option 2: Income decile (same as ONS income explorer scrollytell)

income_decile_df <- deprivation_count_df %>% 
  dplyr::group_by(INCOME_DECILE, PCD_LAD_NAME, PCD_REGION_NAME) %>% 
  dplyr::summarise(INCOME_DECILE_COUNT_LAD = sum(INCOME_DECILE_COUNT)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(PCD_LAD_NAME) %>% 
  dplyr::mutate(INCOME_DECILE_P = INCOME_DECILE_COUNT_LAD / sum(INCOME_DECILE_COUNT_LAD) * 100) %>% 
  dplyr::arrange(PCD_LAD_NAME,INCOME_DECILE) %>% 
  dplyr::ungroup() %>% 
  dplyr::collect()



usethis::use_data(imd_decile_df, overwrite = TRUE)
usethis::use_data(income_decile_df, overwrite = TRUE)



DBI::dbDisconnect(con)
