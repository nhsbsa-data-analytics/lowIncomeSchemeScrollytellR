#' 06_take_up_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_06_take_up_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Estimated take-up is low and decreasing over time"),
    p(
      "It is difficult to determine the eligible population for the NHS Low ",
      "Income Scheme as we need to identify people living in relative poverty",
      "after housing and other costs, who are not already receiving ",
      "qualifying benefits such as Universal Credit."
    ),
    p(
      "We have used data from the ",       
      a(
        "Family Resources Survey", 
        href = "https://www.gov.uk/government/collections/family-resources-survey--2",
        target="_blank"
      ), 
      ", to determine a proxy for the eligible population in England",
      " who could apply to the NHS Low Income Scheme. ",
      "This is the estimated number of individuals (excluding ",
      a(
        "children", 
        href = "https://stat-xplore.dwp.gov.uk/webapi/metadata/HBAI/Type of Individual.html",
        target="_blank"
      ), 
      "), whose net household income after housing costs is 60% below the ",
      "median AND:"
    ),
    tags$ul(
      tags$li(
        "Who are not in receipt of Universal Credit/Equivalent or Pension ",
        "Credit"
      ),
      tags$li("Who do not have savings over £16 thousand")
    ),
    br(),
    p(
      "Take-up is the number of NHS Low Income Scheme individuals covered by ",
      "the application who have received full or partial benefit as a ",
      "percentage of the eligible population."
    ),
    fluidRow(
      column(
        width = 5,
        br(),
        br(),
        p(
          "In the map we can see that estimated take-up relative to the ",
          "eligible population, continues to be ", 
          tags$b("highest in the North East of England"),
          " and the North in general. Although the North East rate has ",
          "declined from 19% in 2015/16 to 10% in 2019/20."
        ),
        br(),
        p(
          "Although London has the greatest number of total applications, ",
          "take-up relative to the eligible population, is ",
          tags$b("lowest in the London region.")
        ),
        br(),
        p(
          "It is not possible to consider take-up at Local Authority level ",
          "using the eligible population. But we can consider take-up per ",
          "thousand of the general population aged 16+ years. Local authority ",
          "rates can be viewed by selecting the region you wish to look at or ",
          "viewing the scatterplot for the rate relative to deprivation."
        )
      ),
      column(
        offset = 1,
        width = 6,
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_successful_individuals_by_region"),
          height = "700px"
        ),
        mod_input_ui(id = ns("input_1"), max = "2019/20")
      )
    )
    )
}

#' 06_take_up Server Function
#'
#' @noRd
mod_06_take_up_map_server <- function(input, output, session) {
  ns <- session$ns
  
  # Pull the slider value
  year <- reactive({callModule(mod_input_server, "input_1")})
 
  output$plot_successful_individuals_by_region = highcharter::renderHighchart({
    
    # Calculate take-up rate per region
    region_df <- lowincomeschemeucd::target_population_df %>%
      dplyr::filter(FINANCIAL_YEAR == year()) %>%
      dplyr::filter(
        TYPE != "Child" &
          !(AGE_BAND %in% c("0 to 15", "16 to 19 child")) &
          LOW_INCOME == 1 &
          UC_OR_EQUIV + PC + SAVINGS == 0
      ) %>%
      dplyr::group_by(FINANCIAL_YEAR, PCD_REGION_NAME) %>%
      dplyr::summarise(TOTAL_POPULATION = sum(TOTAL_POPULATION)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(lowincomeschemeucd::successful_individuals_by_region_df) %>%
      dplyr::mutate(
        value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_POPULATION * 100,
        drilldown = tolower(PCD_REGION_NAME)
      ) %>% 
      dplyr::select(FINANCIAL_YEAR, PCD_REGION_NAME, value, drilldown)
    
    # Calculate rate per 1k adult population of each Local Authority
    la_df <- lowincomeschemeucd::adult_population_df %>%
      dplyr::filter(FINANCIAL_YEAR == year()) %>%
      dplyr::inner_join(lowincomeschemeucd::region_la_lookup) %>%
      dplyr::inner_join(lowincomeschemeucd::successful_individuals_by_la_df) %>%
      dplyr::mutate(
        value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000,
        drilldown = tolower(PCD_REGION_NAME)
      ) %>%
      dplyr::select(FINANCIAL_YEAR, PCD_REGION_NAME, PCD_LAD_NAME, value, drilldown)
    
    # Join the region to the local authority map
    la_map <- lowincomeschemeucd::la_map %>%
      dplyr::inner_join(lowincomeschemeucd::region_la_lookup)
    
    # Create plot
    highcharter::highchart(type = "map") %>%
      highcharter::hc_add_series(
        data = region_df,
        mapData = lowincomeschemeucd::region_map,
        joinBy = "PCD_REGION_NAME",
        name = "regional take-up",
        tooltip = list(
          headerFormat = "",
          pointFormat = "<b>Region:</b> {point.PCD_REGION_NAME}<br><b>Take-up:</b> {point.value:.1f}% (of the eligible population)<br><br>Click for Local Authority breakdown")
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      highcharter::hc_title(
        text = "Estimated take-up of NHS Low Income Scheme (2015/16 to 2019/20)"
      ) %>%
      highcharter::hc_colorAxis(max = 20) %>%
      highcharter::hc_drilldown(
        series = lapply(
          X = region_df$PCD_REGION_NAME, 
          FUN = function(x) {
            list(
              id = tolower(x),
              data = la_df %>% 
                dplyr::filter(PCD_REGION_NAME == x) %>%
                highcharter::list_parse(),
              mapData = la_map %>%
                dplyr::filter(PCD_REGION_NAME == x) %>%
                geojsonsf::sf_geojson() %>% 
                jsonlite::fromJSON(simplifyVector = F),
              value = "value",
              joinBy = "PCD_LAD_NAME",
              tooltip = list(
                headerFormat = "",
                pointFormat = "<b>Local Authority:</b> {point.PCD_LAD_NAME}<br><b>Take-up:</b> {point.value:.1f} (per thousand of the general population)"
              )
            )
          }
        )
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
    
  })
  
  
  
}

## To be copied in the UI
# mod_06_take_up_map_ui("06_take_up_map_1")

## To be copied in the server
# callModule(mod_06_take_up_map_server, "06_take_up_map_1")