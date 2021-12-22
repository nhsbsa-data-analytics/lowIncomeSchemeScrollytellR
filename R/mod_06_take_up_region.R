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
      "Estimated take-up is the number of NHS Low Income Scheme individuals covered by ",
      "the application who have received full or partial benefit ",
      "per thousand of the general population aged 16 or over."
    ),
    br(),
    p(
      tags$b(
        "Estimated take-up per thousand of the general population was 8 in 2015/16,",
        "decreasing to 4 in 2020/21."
      )
    ),
    # Keep old structure until we confirm
    # fluidRow(
    #   column(
    #     width = 5,
    #     br(),
    #     br(),
    #     p(
    #       "By region, we can see that estimated take-up, relative to the ",
    #       "population, continues to be ",
    #       tags$b("highest in the North East of England"),
    #       " and the North in general. Although the North East rate has ",
    #       "declined from 13 in 2015/16 to 6 in 2020/21."
    #     ),
    #     br(),
    #     p(
    #       "It is ",
    #       tags$b("lowest in the South East region.")
    #     )
    #   ),
    #   column(
    #     # offset = 1,
    #     width = 7,
    #     align = "center",
    #     style = "background-color: #FFFFFF;",
    #     highcharter::highchartOutput(
    #       outputId = ns("plot_successful_individuals_by_region"), # change this one to animation style
    #       height = "700px"
    #     )
    #   )
    # )
    ######
    fluidRow(
      col_12(
        br(),
        p(
          "By region, we can see that estimated take-up, relative to the ",
          "population, continues to be ",
          tags$b("highest in the North East of England"),
          " and the North in general. Although the North East rate has ",
          "declined from 13 in 2015/16 to 6 in 2020/21."
        ),
        br(),
        p(
          "It is ",
          tags$b("lowest in the South East region.")
        )
      ),
      br(),
      br(),
      col_12(
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_successful_individuals_by_region"),
          height = "600px"
        ),
        mod_download_ui(
          id = ns("download_region_take_up")
        )
      )
    )
    ####
  )
}

#' 06_take_up_region Server Functions
#'
#' @noRd
mod_06_take_up_region_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$plot_successful_individuals_by_region <- highcharter::renderHighchart({

      # Calculate take-up rate per 1k adult population of each region
      # Because of inner join from other data-raw output, calculating SDC here

      region_take_up <- lowIncomeSchemeScrollytellR::adult_population_df %>%
        # dplyr::filter(FINANCIAL_YEAR == year()) %>%
        dplyr::group_by(FINANCIAL_YEAR, PCD_REGION_NAME) %>%
        dplyr::summarise(TOTAL_POPULATION = sum(TOTAL_ADULT_POPULATION)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_region_df) %>%
        dplyr::mutate(
          PCT_INDIVIDUALS_REGION = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_POPULATION * 1000
        )


      region_take_up <- region_take_up %>%
        dplyr::mutate(
          SDC = ifelse(TOTAL_SUCCESSFUL_INDIVIDUALS %in% c(1, 2, 3, 4), 1, 0),
          SDC_PCT_INDIVIDUALS_REGION =
            ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_INDIVIDUALS_REGION))
        ) %>%
        dplyr::select(-SDC)

      # Create plot
      region_take_up %>%
        highcharter::hchart(
          type = "line",
          highcharter::hcaes(x = FINANCIAL_YEAR, y = SDC_PCT_INDIVIDUALS_REGION, group = PCD_REGION_NAME)
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_title(
          text = "Estimated take-up of NHS Low Income Scheme (2015/16 to 2020/21)"
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "per thousand of the general population")
        ) %>%
        highcharter::hc_xAxis(
          title = list(text = "Financial year")
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE
        )
    })

    region_take_up_download <- region_take_up %>%
      dplyr::mutate(
        SDC_PCT_INDIVIDUALS_REGION = ifelse(
          test = is.na(SDC_PCT_INDIVIDUALS_REGION),
          yes = "c",
          no = as.character(SDC_PCT_INDIVIDUALS_REGION)
        )
      ) %>%
      dplyr::rename(REGION_TAKE_UP_PER_THOUSAND_POP = SDC_PCT_INDIVIDUALS_REGION) %>%
      dplyr::select(-TOTAL_POPULATION, -TOTAL_SUCCESSFUL_INDIVIDUALS, -PCT_INDIVIDUALS_REGION)

    # add data download
    mod_download_server(
      id = "download_region_take_up",
      filename = "region_take_up.csv",
      export_data = region_take_up_download
    )
  })
}

## To be copied in the UI
# mod_06_take_up_region_ui("06_take_up_region_ui_1")

## To be copied in the server
# mod_06_take_up_region_server("06_take_up_region_ui_1")
