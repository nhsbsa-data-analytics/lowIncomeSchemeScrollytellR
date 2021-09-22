# Pull the region map
region_map <- sf::read_sf("https://opendata.arcgis.com/api/v3/datasets/bafeb380d7e34f04a3cdf1628752d5c3_0/downloads/data?format=geojson&spatialRefId=4326") %>%  
  janitor::clean_names() %>% 
  dplyr::select(PCD_REGION_NAME = rgn18nm, PCD_REGION_GEOMETRY = geometry) %>% 
  sf::st_transform(crs = 27700) %>% 
  geojsonsf::sf_geojson()  %>% 
  jsonlite::fromJSON(simplifyVector = F)

   
  

# Pull the local authority map (don't convert to geojson as we need it as a 
# dataframe)
la_map <- sf::read_sf("https://opendata.arcgis.com/api/v3/datasets/21f7fb2d524b44c8ab9dd0f971c96bba_0/downloads/data?format=geojson&spatialRefId=4326") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(grepl("^E", lad21cd)) %>%
  dplyr::select(PCD_LAD_NAME = lad21nm, PCD_LAD_GEOMETRY = geometry) %>%
  sf::st_transform(crs = 27700)

# Pull the region / local authority lookup
region_la_lookup <- read.csv("https://opendata.arcgis.com/api/v3/datasets/6a41affae7e345a7b2b86602408ea8a2_0/downloads/data?format=csv&spatialRefId=4326") %>% 
  janitor::clean_names() %>% 
  dplyr::select(
    PCD_REGION_NAME = rgn21nm,
    PCD_LAD_NAME = lad21nm
  )


usethis::use_data(region_map, overwrite = TRUE)
usethis::use_data(la_map, overwrite = TRUE)
usethis::use_data(region_la_lookup, overwrite = TRUE)