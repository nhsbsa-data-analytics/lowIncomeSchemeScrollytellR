library(magrittr)

library(openxlsx)

# Download the raw data-files by using the download link from:
# 

la_deprivation_rate <- 
  read.xlsx(
    "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpersonalandhouseholdfinances%2fincomeandwealth%2fdatasets%2fmappingincomedeprivationatalocalauthoritylevel%2f2019/localincomedeprivationdata.xlsx",
    cols = c(2,3), sheet=3)

# column name change
la_deprivation_rate <- la_deprivation_rate %>% 
  dplyr::rename(PCD_LA_NAME = "Local.Authority.District.name.(2019)",
                INCOME_DEPRIVATION_AVERAGE_SCORE = "Income.deprivation-.Average.score" )


usethis::use_data(la_deprivation_rate, overwrite = TRUE)


