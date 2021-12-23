# Pull the region map
region_map <- sf::read_sf("https://opendata.arcgis.com/api/v3/datasets/bafeb380d7e34f04a3cdf1628752d5c3_0/downloads/data?format=geojson&spatialRefId=4326") %>%
  janitor::clean_names() %>%
  dplyr::select(PCD_REGION_NAME = rgn18nm, PCD_REGION_GEOMETRY = geometry) %>%
  sf::st_transform(crs = 27700) %>%
  geojsonsf::sf_geojson() %>%
  jsonlite::fromJSON(simplifyVector = F)





# 2020 boundary is used.
# spatial reference is selected in API call.
la_map <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2020_UK_BUC_V2/FeatureServer/0/query?where=1%3D1&outFields=LAD20NM,LAD20CD&outSR=27700&f=json") %>%
  janitor::clean_names() %>%
  dplyr::filter(grepl("^E", lad20cd)) %>%
  dplyr::select(PCD_LAD_NAME = lad20nm, PCD_LAD_GEOMETRY = geometry) 


# Pull the region / local authority lookup 
region_la_lookup <- jsonlite::fromJSON("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_RGN20_EN_LU/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json")$features$attributes %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%  
  dplyr::select(
    PCD_REGION_NAME = rgn20nm,
    PCD_LAD_NAME = lad20nm
  )


usethis::use_data(region_map, overwrite = TRUE)
usethis::use_data(la_map, overwrite = TRUE)
usethis::use_data(region_la_lookup, overwrite = TRUE)
