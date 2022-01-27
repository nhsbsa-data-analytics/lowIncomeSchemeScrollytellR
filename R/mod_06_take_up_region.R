#' 06_take_up_region UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_06_take_up_region_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Estimated take-up is low and decreasing over time"),
    p(
      "Estimated take-up is the number of people who have received full or ",
      "partial benefit per thousand of the general population aged 16 or over."
    ),
    br(),
    p(
      tags$b(
        "Estimated take-up per thousand of the general population was 8 in ",
        "2015/16, decreasing to 4 in 2020/21."
      )
    ),
    p(
      "By region, we can see that estimated take-up, relative to the ",
      "population, continues to be ",
      tags$b("highest in the North East of England"),
      " and the North in general. Although the North East rate has declined ",
      "from 13 in 2015/16 to 6 in 2020/21."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Estimated take-up of NHS Low Income Scheme in England (2015/16 to ",
        "2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_successful_individuals_by_region"),
        height = "350px"
      )
    ),
    mod_download_ui(
      id = ns("download_successful_individuals_by_region_df")
    )
  )
}

#' 06_take_up_region Server Functions
#'
#' @noRd
mod_06_take_up_region_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate take-up rate per 1k adult population of each region
    # Because of inner join from other data-raw output, calculating SDC here
    successful_individuals_by_region_df <-
      lowIncomeSchemeScrollytellR::adult_population_df %>%
      dplyr::group_by(FINANCIAL_YEAR, PCD_REGION_NAME) %>%
      dplyr::summarise(TOTAL_POPULATION = sum(TOTAL_ADULT_POPULATION)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(
        y = lowIncomeSchemeScrollytellR::successful_individuals_by_region_df
      ) %>%
      dplyr::mutate(
        TAKE_UP_PER_THOUSAND = janitor::round_half_up(
          TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_POPULATION * 1000, 1
        ),
        TOTAL_SUCCESSFUL_INDIVIDUALS = round(TOTAL_SUCCESSFUL_INDIVIDUALS, -1)
      )


    # Create chart
    output$plot_successful_individuals_by_region <-
      highcharter::renderHighchart({
        successful_individuals_by_region_df %>%
          highcharter::hchart(
            type = "line",
            highcharter::hcaes(
              x = FINANCIAL_YEAR,
              y = TAKE_UP_PER_THOUSAND,
              group = PCD_REGION_NAME
            )
          ) %>%
          theme_nhsbsa(stack = NA) %>%
          highcharter::hc_yAxis(
            title = list(text = "Per thousand of the general population")
          ) %>%
          highcharter::hc_xAxis(
            title = list(text = "Financial year")
          ) %>%
          highcharter::hc_tooltip(
            shared = TRUE,
            valueDecimals = 1
          )
      })

    # Add data download
    mod_download_server(
      id = "download_successful_individuals_by_region_df",
      filename = "successful_individuals_by_region_df.csv",
      export_data = successful_individuals_by_region_df %>%
        dplyr::rename(
          REGION_NAME = PCD_REGION_NAME,
          OVER16_POPULATION = TOTAL_POPULATION
        )
    )
  })
}

## To be copied in the UI
# mod_06_take_up_region_ui("06_take_up_region_ui_1")

## To be copied in the server
# mod_06_take_up_region_server("06_take_up_region_ui_1")
