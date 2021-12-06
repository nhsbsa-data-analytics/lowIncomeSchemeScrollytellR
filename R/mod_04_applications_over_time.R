#' 04_applications_over_time UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_applications_over_time_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("There is a decrease in applications in England over time"),
    p(
      "The total number of NHS Low Income Scheme applications in England has ",
      "declined by 15% from 350 thousand in 2015/16 to 298 thousand in ",
      "2019/20, with further declines during the pandemic."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      highcharter::highchartOutput(ns("plot_applications"))
    ),
    br(),
    p(
      "The rate of decline is highest amongst:"
    ),
    tags$ul(
      tags$li("Those categorised as students at around 29%."),
      tags$li(
        "15 to 24 year olds (27%), where student applications are most ",
        "prevalent."
      ),
      tags$li(
        "45 to 54 year olds (24%), where earner applications are most common."
      ),
      tags$li("In areas of lower deprivation.")
    )
  )
}

#' 04_applications_over_time Server Function
#'
#' @noRd
mod_04_applications_over_time_server <- function(input, output, session) {
  ns <- session$ns

  # Time series plot
  output$plot_applications <- highcharter::renderHighchart({

    # Aggregate
    plot_df <- lowIncomeSchemeScrollytellR::applications_df %>%
      dplyr::group_by(FINANCIAL_YEAR) %>%
      dplyr::summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
      dplyr::ungroup()

    # Create  plot
    plot_df %>%
      highcharter::hchart(
        type = "line",
        highcharter::hcaes(x = FINANCIAL_YEAR, y = TOTAL_APPLICATIONS)
      ) %>%
      theme_nhsbsa() %>%
      highcharter::hc_title(
        text = "Number of NHS Low Income Scheme applications in England (2015/16 to 2020/21)"
      ) %>%
      highcharter::hc_yAxis(
        min = 0,
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value) / 1000; }")
        ),
        title = list(text = "Total number of applications (thousands)")
      ) %>%
      highcharter::hc_xAxis(
        title = list(text = "Financial year")
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Financial Year: </b>' + this.point.FINANCIAL_YEAR + '<br/>' + '<b>Total Applications: </b>' + (Math.round(this.point.y / 500) * 500 / 1000).toFixed(1) + 'k';}")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
  })
}

## To be copied in the UI
# mod_04_applications_over_time_ui("04_applications_over_time_1")

## To be copied in the server
# callModule(mod_04_applications_over_time_server, "04_applications_over_time_1")
