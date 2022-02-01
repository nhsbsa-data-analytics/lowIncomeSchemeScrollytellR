#' 05_what_help_does_lis_provide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_what_help_does_lis_provide_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("What level of help do applicants receive in England?"),
    p(
      "The level of help applicants receive depends on a ",
      tags$b("complex set of criteria"), " encompassing their weekly income, ",
      "necessary outgoings, plus any savings or investments."
    ),
    p(
      "In 2019/20:",
      tags$ul(
        tags$li(
          "Just over half of applications (53%) resulted in a HC2 award (full ",
          "benefit), which typically provides full help with health costs."
        ),
        tags$li(
          "Just under one-quarter (23%) resulted in a HC3 award (partial ",
          "benefit) . Partial benefit offers differing levels of help towards ",
          "health costs such as dental treatment and eye tests according to ",
          "assessed income, and no assistance with prescription costs."
        ),
        tags$li(
          "Around one in ten applications are withdrawn or abandoned due to ",
          "insufficient information provided by the applicant."
        )
      ),
      p(
        "The remaining applications are deemed unnecessary, either because ",
        "applicants:"
      ),
      tags$ul(
        tags$li("Are already receiving other benefits (8%)"),
        tags$li("Have excess income too high to qualify (4%)"),
        tags$li(
          "Have more than £16 thousand in savings, investments or property ",
          "(0.4%)"
        )
      ),
      br(),
      fluidRow(
        align = "center",
        style = "background-color: #FFFFFF;",
        h6(
          "Outcome of NHS Low Income Scheme applications in England (2015/16 ",
          "to 2020/21)"
        ),
        highcharter::highchartOutput(
          outputId = ns("plot_applications_by_outcome"),
          height = "300px"
        )
      ),
      mod_download_ui(
        id = ns("download_applications_outcome")
      ),
      br(),
      p(
        "There is a decline in applications receiving full and partial help, ",
        "between 2015/16 and 2019/20 from 87% to 76%. Whereas there is an ",
        "increase in the proportion of applications:"
      ),
      tags$ul(
        tags$li("Which have been withdrawn/abandoned."),
        tags$li(
          "Where applicants are already in receipt of qualifying benefits."
        ),
        tags$li("Where applicants have excess income too high to qualify.")
      )
    )
  )
}

#' 05_what_help_does_lis_provide Server Functions
#'
#' @noRd
mod_05_what_help_does_lis_provide_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Stacked column plot for outcome
    output$plot_applications_by_outcome <- highcharter::renderHighchart({

      # Create  plot
      lowIncomeSchemeScrollytellR::applications_outcome_df %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = FINANCIAL_YEAR,
            y = PCT_OUTCOMES,
            group = OUTCOME_LEVEL2
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_caption(
          text = paste(
            "Excludes ongoing applications.",
            "<br>", "Percentages are rounded to one decimal."
          ),
          align = "right"
        ) %>%
        highcharter::hc_yAxis(
          max = 100,
          title = list(text = "Percentage of applications")
        ) %>%
        highcharter::hc_xAxis(
          title = list(text = "Financial year")
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          headerFormat = "<b> {point.name} </b>",
          valueSuffix = "%",
          valueDecimals = 1
        )
    })

    # Add data to download button
    mod_download_server(
      id = "download_applications_outcome",
      filename = "applications_outcomes.csv",
      export_data = lowIncomeSchemeScrollytellR::applications_outcome_df %>%
        dplyr::rename(
          OUTCOME = OUTCOME_LEVEL2,
          APPLICATIONS = TOTAL_APPLICATIONS,
          OUTCOME_PERCENTAGE = PCT_OUTCOMES
        )
    )
  })
}

## To be copied in the UI
# mod_05_what_help_does_lis_provide_ui("05_what_help_does_lis_provide_ui_1")

## To be copied in the server
# mod_05_what_help_does_lis_provide_server("05_what_help_does_lis_provide_ui_1")
