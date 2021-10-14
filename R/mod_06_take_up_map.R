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
      "Take-up is the number of NHS Low Income Scheme individuals covered by ",
      "the application who have received full or partial benefit ",
      "per thousand of the general population aged 16 or over years."
    ),
    br(),
    p(
      tags$b(
        "Estimated take-up per thousand of the general population was 8 in 2015/16,",
        "decreasing to 6 in 2019/20."
      )
    ),
    fluidRow(
      column(
        width = 5,
        br(),
        br(),
        p(
          "By region, we can see that estimated take-up, relative to the ",
          "population, continues to be ",
          tags$b("highest in the North East of England"),
          " and the North in general. Although the North East rate has ",
          "declined from 13 in 2015/16 to 10 in 2019/20."
        ),
        br(),
        p(
          "It is ",
          tags$b("lowest in the South East region.")
        ),
        br(),
        p(
          "It is helpful to consider estimated take-up relative to deprivation.",
          "The chart and map show estimated take-up by local authority area relative ",
          "to the population and overall deprivation profile of an area."
        )
      ),
      column(
        offset = 1,
        width = 6,
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_successful_individuals_by_region"), # change this one to animation style
          height = "700px"
        )
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
  year <- reactive({
    input$input_year
  })

  output$plot_successful_individuals_by_region <- highcharter::renderHighchart({

    # Calculate take-up rate per 1k adult population of each region
    # Removed FRS population based on team discussion
    # Now changed to student map style (with animation due to tooltip chart)
    plot_df <- lowIncomeSchemeScrollytellR::adult_population_df %>%
      # dplyr::filter(FINANCIAL_YEAR == year()) %>%
      dplyr::group_by(FINANCIAL_YEAR, PCD_REGION_NAME) %>%
      dplyr::summarise(TOTAL_POPULATION = sum(TOTAL_ADULT_POPULATION)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_region_df) %>%
      dplyr::mutate(
        value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_POPULATION * 1000
      )

    # Format for highchater animation
    # using tidyr::complete
    plot_sequence_series <- plot_df %>%
      tidyr::complete(FINANCIAL_YEAR, PCD_REGION_NAME,
        fill = list(value = 0)
      ) %>%
      dplyr::group_by(PCD_REGION_NAME) %>%
      dplyr::do(sequence = .$value) %>%
      highcharter::list_parse()


    # Create plot
    highcharter::highchart(type = "map") %>%
      highcharter::hc_chart(marginBottom = 100) %>%
      highcharter::hc_add_series(
        data = plot_sequence_series,
        mapData = lowIncomeSchemeScrollytellR::region_map,
        joinBy = "PCD_REGION_NAME",
        tooltip = list(
          headerFormat = "",
          pointFormat = "<b>Region:</b> {point.PCD_REGION_NAME}<br><b>Take-up:</b> {point.value:.1f} (per thousand of the general population)"
        )
      ) %>%
      highcharter::hc_motion(
        labels = unique(plot_df$FINANCIAL_YEAR),
        startIndex = 4
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      highcharter::hc_title(
        text = "Estimated take-up of NHS Low Income Scheme (2015/16 to 2019/20)"
      ) %>%
      highcharter::hc_colorAxis(min = 0, max = 20)
  })
}

## To be copied in the UI
# mod_06_take_up_map_ui("06_take_up_map_1")

## To be copied in the server
# callModule(mod_06_take_up_map_server, "06_take_up_map_1")
