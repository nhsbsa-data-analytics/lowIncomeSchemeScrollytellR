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
      "In 2019/20, ", tags$b("just over half of applications"), " resulted ",
      "in a HC2 award ", tags$b("(full benefit),"), "which typically ",
      "provides full help with health costs."
    ),
    p(
      "And ", tags$b("just under one-quarter"), " resulted in a HC3 award ",
      tags$b("(partial benefit)."), "This offers differing levels of help ",
      "towards health costs according to assessed income, and no assistance ",
      "with prescription costs."
    ),
    p(
      "Around ", tags$b("one in ten"), " applications are ",
      tags$b("withdrawn or abandoned"), " due to insufficient information ",
      "from the applicant."
    ),
    p(
      "
      And the remaining applications are deemed unnecessary, either because ",
      "applicants:"
    ),
    tags$ul(
      tags$li("Are already receiving other benefits (8%)"),
      tags$li("Have excess income too high to qualify (4%)"),
      tags$li(
        "Have more than £16 thousand in savings, investments or property (0.4%)"
      )
    ),
    fluidRow(
      style = "background-color: #FFFFFF;",
      highcharter::highchartOutput(ns("plot_applications_by_outcome")),
      mod_download_ui(
        id = ns("download_applications_outcome")
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
          highcharter::hcaes(x = FINANCIAL_YEAR, y = PCT_OUTCOMES, group = OUTCOME_LEVEL2)
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_title(
          text = "Outcome of NHS Low Income Scheme applications in England (2015/16 to 2020/21)"
        ) %>%
        highcharter::hc_subtitle(
          text = paste(
            "Note: Excludes ongoing applications.", "<br>",
            "Numbers are rounded to the nearest 10. Percentages are rounded to the nearest whole number."
          ),
          verticalAlign = "bottom",
          align = "right"
        ) %>%
        highcharter::hc_yAxis(
          max = 100,
          labels = list(
            formatter = highcharter::JS("function(){ return this.value ;}")
          ),
          title = list(text = "Percentage of applications")
        ) %>%
        highcharter::hc_xAxis(
          title = list(text = "Financial year")
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          headerFormat = "<b> {point.name} </b>", valueSuffix = "%"
        ) %>%
        highcharter::hc_credits(
          enabled = TRUE
        )
    })

    # Swap NAs for "c" for data download
    applications_outcome_download_df <- lowIncomeSchemeScrollytellR::applications_outcome_df %>%
      dplyr::rename(
        OUTCOME = OUTCOME_LEVEL2,
        APPLICATIONS = TOTAL_APPLICATIONS,
        OUTCOME_PERCENTAGE = PCT_OUTCOMES
      )

    # Add data to download button
    mod_download_server(
      id = "download_applications_outcome",
      filename = "applications_outcomes.csv",
      export_data = applications_outcome_download_df
    )
  })
}

## To be copied in the UI
# mod_05_what_help_does_lis_provide_ui("05_what_help_does_lis_provide_ui_1")

## To be copied in the server
# mod_05_what_help_does_lis_provide_server("05_what_help_does_lis_provide_ui_1")
