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
      "in a HC2 award ", tags$b("(full benefit)"), ", which typically ",
      "provides full help with health costs."
    ),
    p(
      "And ", tags$b("just under one-quarter"), " resulted in a HC3 award ",
      tags$b("(partial benefit)"), ". This offers differing levels of help ",
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
      align = "center",
      style = "background-color: #FFFFFF;",
      highcharter::highchartOutput(ns("plot_applications_by_outcome"))
    )
  )
}

#' 05_what_help_does_lis_provide Server Function
#'
#' @noRd
mod_05_what_help_does_lis_provide_server <- function(input, output, session) {
  ns <- session$ns

  # Stacked column plot for outcome
  output$plot_applications_by_outcome <- highcharter::renderHighchart({

    # Filter out Ongoing, aggregate, then calculate %s
    plot_df <- nhslowincomeschemescrollytell::applications_df %>%
      dplyr::filter(OUTCOME_LEVEL2 != "Ongoing") %>%
      dplyr::group_by(FINANCIAL_YEAR, OUTCOME_LEVEL2) %>%
      dplyr::summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(FINANCIAL_YEAR) %>%
      dplyr::mutate(p = TOTAL_APPLICATIONS / sum(TOTAL_APPLICATIONS) * 100) %>%
      dplyr::ungroup()

    # Create  plot
    plot_df %>%
      highcharter::hchart(
        type = "column",
        highcharter::hcaes(x = FINANCIAL_YEAR, y = p, group = OUTCOME_LEVEL2)
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      highcharter::hc_title(
        text = "Outcome of NHS Low Income Scheme applications in England (2015/16 to 2020/21)"
      ) %>%
      highcharter::hc_subtitle(text = "Note: Excludes ongoing applications.") %>%
      highcharter::hc_yAxis(
        max = 100,
        labels = list(
          formatter = highcharter::JS("function(){ return this.value + '%' ;}")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Outcome: </b>' + this.series.name + '<br>' + '<b>Financial Year: </b>' + this.point.FINANCIAL_YEAR + '<br/>' + '<b>Percentage: </b>' + Math.round(this.point.y * 10) / 10 + '%';}")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
  })
}

## To be copied in the UI
# mod_05_what_help_does_lis_provide_ui("05_what_help_does_lis_provide_1")

## To be copied in the server
# callModule(mod_05_what_help_does_lis_provide_server, "05_what_help_does_lis_provide_1")
