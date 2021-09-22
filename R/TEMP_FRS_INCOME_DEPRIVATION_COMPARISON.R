# # Map 1: 2019/2020 using FRS dataset as denominator
# 
# # Calculate take-up rate per region
# region_df <- nhslowincomeschemescrollytell::target_population_df %>%
#   dplyr::filter(FINANCIAL_YEAR == "2019/20") %>%
#   dplyr::filter(TYPE != "Child" &
#                   !(AGE_BAND %in% c("0 to 15", "16 to 19 child")) &
#                   LOW_INCOME == 1 &
#                   UC_OR_EQUIV + PC + SAVINGS == 0) %>%
#   dplyr::group_by(FINANCIAL_YEAR, PCD_REGION_NAME) %>%
#   dplyr::summarise(TOTAL_POPULATION = sum(TOTAL_POPULATION)) %>%
#   dplyr::ungroup() %>%
#   dplyr::inner_join(nhslowincomeschemescrollytell::successful_individuals_by_region_df) %>%
#   dplyr::mutate(value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_POPULATION * 100,
#                 drilldown = tolower(PCD_REGION_NAME)) %>%
#   dplyr::select(FINANCIAL_YEAR, PCD_REGION_NAME, value, drilldown)
# 
# # Calculate rate per 1k adult population of each Local Authority
# la_df <- nhslowincomeschemescrollytell::adult_population_df %>%
#   dplyr::filter(FINANCIAL_YEAR == "2019/20") %>%
#   dplyr::inner_join(nhslowincomeschemescrollytell::region_la_lookup) %>%
#   dplyr::inner_join(nhslowincomeschemescrollytell::successful_individuals_by_la_df) %>%
#   dplyr::mutate(
#     value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000,
#     drilldown = tolower(PCD_REGION_NAME)
#   ) %>%
#   dplyr::select(FINANCIAL_YEAR,
#                 PCD_REGION_NAME,
#                 PCD_LAD_NAME,
#                 value,
#                 drilldown)
# 
# # Join the region to the local authority map
# la_map <- nhslowincomeschemescrollytell::la_map %>%
#   dplyr::inner_join(nhslowincomeschemescrollytell::region_la_lookup)
# 
# # Create plot
# highcharter::highchart(type = "map") %>%
#   highcharter::hc_add_series(
#     data = region_df,
#     mapData = nhslowincomeschemescrollytell::region_map,
#     joinBy = "PCD_REGION_NAME",
#     name = "regional take-up",
#     tooltip = list(headerFormat = "",
#                    pointFormat = "<b>Region:</b> {point.PCD_REGION_NAME}<br><b>Take-up:</b> {point.value:.1f}% (of the eligible population)<br><br>Click for Local Authority breakdown")
#   ) %>%
#   highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
#   highcharter::hc_title(text = "Estimated take-up of NHS Low Income Scheme (2015/16 to 2019/20)") %>%
#   highcharter::hc_colorAxis(max = 20) %>%
#   highcharter::hc_drilldown(series = lapply(
#     X = region_df$PCD_REGION_NAME,
#     FUN = function(x) {
#       list(
#         id = tolower(x),
#         data = la_df %>%
#           dplyr::filter(PCD_REGION_NAME == x) %>%
#           highcharter::list_parse(),
#         mapData = la_map %>%
#           dplyr::filter(PCD_REGION_NAME == x) %>%
#           geojsonsf::sf_geojson() %>%
#           jsonlite::fromJSON(simplifyVector = F),
#         value = "value",
#         joinBy = "PCD_LAD_NAME",
#         tooltip = list(headerFormat = "",
#                        pointFormat = "<b>Local Authority:</b> {point.PCD_LAD_NAME}<br><b>Take-up:</b> {point.value:.1f} (per thousand of the general population)")
#       )
#     }
#   )) %>%
#   highcharter::hc_credits(enabled = TRUE)
# 
# 
# 
# # Map 2:
# # FRS population
# # 16+ population to get what %s over
# # LA to Region lookup 
# # 
#   
