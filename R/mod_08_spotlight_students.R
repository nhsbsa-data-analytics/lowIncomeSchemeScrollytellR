#' 08_spotlight_students UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_08_spotlight_students_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Spotlight on student applicants in England"),
    p("Student applications were considered in more detail, given the rate of decline in applications."),
    p("Of student applicants in England:"),
    tags$ul(
      tags$li("The majority (98%) apply as single applicants."),
      tags$li(
        "Over half are aged 20 to 24 years and 37% are aged 15 to 19 years."
      )
    ),
    br(),
    h6("A stronger downward trend in student applications in England"),
    p(
      "The total number of", tags$b(" student applications decreased by 29% "),
      "between 2015/16 and 2019/20, and have decreased further since the ",
      "pandemic. This compares to 15% overall."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      highcharter::highchartOutput(outputId = ns("plot_student_applications")),
    ),
    br(),
    tags$em(
      "It's a great scheme and efficiently run. I am extremely grateful for the",
      " help with prescription costs. However, I feel it is not advertised very well",
      " especially to students and people who do not know other people on benefits.(Student applicant)"
    ),
    br(),
    h6("Estimated student take-up is also low and decreasing over time."),
    p(
      "In order to determine estimated take-up for students, we have used a ",
      "proxy for the eligible population as the number of enrolled students ",
      "in higher education in England."
    ),
    p(
      "Estimated take-up is the number of NHS Low Income Scheme individuals ",
      "covered by student applications who have received full or partial ",
      "benefit as a percentage of the eligible population."
    ),
    p(
      tags$b(
        "Estimated take-up was around 4% in the 2015/16 Academic Year ",
        "reducing to 2% in 2019/20."
      )
    ),
    fluidRow(
      column(
        width = 5,
        br(),
        br(),
        br(),
        p(
          "Estimated student take-up, continues to be somewhat",
          tags$b(" lower in the South East and London."), "This is despite ",
          "London having in the largest proportion of student applicants."
        ),
        br(),
        p("And student take-up is", tags$b("highest in the North West.")),
        br(),
        p(
          "The take-up rate has",
          tags$b("decreased most in the East of England.")
        )
      ),
      column(
        width = 6,
        offset = 1,
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_successful_student_individuals_by_region"),
          height = "700px"
        )
      )
    ),
  )
}

#' 08_spotlight_students Server Function
#'
#' @noRd
mod_08_spotlight_students_server <- function(input, output, session) {
  ns <- session$ns

  # Stacked column of student applications over time
  output$plot_student_applications <- highcharter::renderHighchart({

    # Group data and aggregate
    plot_df <- lowIncomeSchemeScrollytellR::applications_df %>%
      dplyr::mutate(TYPE = ifelse(CLIENTGROUP_DESC == "Student", "Student", "Non-Student")) %>%
      dplyr::group_by(FINANCIAL_YEAR, TYPE) %>%
      dplyr::summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
      dplyr::ungroup()

    # Create plot
    plot_df %>%
      highcharter::hchart(
        type = "column",
        highcharter::hcaes(x = FINANCIAL_YEAR, y = TOTAL_APPLICATIONS, group = TYPE)
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa(col_type = "highlight")) %>%
      highcharter::hc_title(
        text = "Number of NHS Low Income Scheme Student applications in England (2015/16 to 2020/21)"
      ) %>%
      highcharter::hc_legend(reversed = TRUE) %>%
      highcharter::hc_yAxis(
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value) / 1000 + 'k'; }")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Client Group: </b>' + this.series.name + '<br>' + '<b>Total Applications: </b>' + Math.round(this.point.y / 500) * 500 / 1000 + 'k';}")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
  })




  output$plot_successful_student_individuals_by_region <- highcharter::renderHighchart({

    # Calculate rate per region
    plot_df <- lowIncomeSchemeScrollytellR::student_population_df %>%
      dplyr::filter(ACADEMIC_YEAR != "2014/15") %>%
      dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_student_individuals_by_region_df) %>%
      dplyr::mutate(
        value = TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS / TOTAL_STUDENT_POPULATION * 100
      )

    # Format for highcharter animation
    plot_sequence_series <- plot_df %>%
      tidyr::expand(ACADEMIC_YEAR, PCD_REGION_NAME) %>%
      dplyr::left_join(plot_df) %>%
      dplyr::mutate(value = tidyr::replace_na(value)) %>%
      dplyr::group_by(PCD_REGION_NAME) %>%
      dplyr::do(sequence = .$value) %>%
      highcharter::list_parse()

    # Create plot
    highcharter::highchart(type = "map") %>%
      highcharter::hc_chart(marginBottom = 100) %>%
      highcharter::hc_add_series(
        data = plot_sequence_series,
        mapData = region_map,
        joinBy = "PCD_REGION_NAME",
        tooltip = list(
          headerFormat = "",
          pointFormat = "<b>Region:</b> {point.PCD_REGION_NAME}<br><b>Take-up:</b> {point.value:.1f}% (of the student population)<br>"
        )
      ) %>%
      highcharter::hc_motion(
        labels = unique(plot_df$ACADEMIC_YEAR),
        startIndex = 4
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      highcharter::hc_title(
        text = "Estimated take-up of NHS Low Income Scheme Student individuals by England region (2015/16 to 2019/20)"
      ) %>%
      highcharter::hc_colorAxis(min = 0, max = 6)
  })


  output$plot_student_applications_per_month <- highcharter::renderHighchart({

    # Aggregate overall for line plot
    plot_overall_df <- lowIncomeSchemeScrollytellR::applications_df %>%
      dplyr::filter(CLIENTGROUP_DESC == "Student") %>%
      dplyr::group_by(APPLICATION_MONTH) %>%
      dplyr::summarise(TOTAL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(MONTH = lubridate::ym(APPLICATION_MONTH))

    # Aggregate by benefit type (partial or full)
    plot_benefit_df <- lowIncomeSchemeScrollytellR::applications_df %>%
      dplyr::filter(
        CLIENTGROUP_DESC == "Student",
        OUTCOME_LEVEL2 %in% c("Full benefit", "Partial benefit")
      ) %>%
      dplyr::group_by(APPLICATION_MONTH, OUTCOME_LEVEL2) %>%
      dplyr::summarise(SUCCESSFUL_APPLICATIONS = sum(TOTAL_APPLICATIONS)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(MONTH = lubridate::ym(APPLICATION_MONTH)) %>%
      dplyr::inner_join(plot_overall_df) %>%
      dplyr::mutate(p = SUCCESSFUL_APPLICATIONS / TOTAL_APPLICATIONS * 100)

    # Create plot
    highcharter::highchart() %>%
      highcharter::hc_add_series(
        data = plot_overall_df,
        type = "line",
        highcharter::hcaes(x = MONTH, y = TOTAL_APPLICATIONS),
        name = "Total applications",
        yAxis = 0
      ) %>%
      highcharter::hc_add_series(
        data = plot_benefit_df,
        type = "column",
        highcharter::hcaes(x = MONTH, y = p, group = OUTCOME_LEVEL2),
        yAxis = 1
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      highcharter::hc_xAxis(
        type = "datetime"
      ) %>%
      highcharter::hc_yAxis_multiples(
        list(
          top = "0%",
          height = "40%",
          min = 0,
          title = list(enabled = TRUE, text = "Total Applications"),
          labels = list(formatter = highcharter::JS("function(){ return this.value / 1000 + 'k'; }"))
        ),
        list(
          top = "40%",
          height = "60%",
          min = 0,
          opposite = TRUE,
          title = list(enabled = TRUE, text = "Successful Applications"),
          labels = list(format = "{value}%")
        )
      ) %>%
      highcharter::hc_title(
        text = "Total and successful outcome (%) of NHS Low Income Scheme Student applications by month in England (2015/16 to 2020/21)"
      ) %>%
      highcharter::hc_tooltip(
        shared = TRUE,
        formatter = highcharter::JS("function () { return '<b>Month: </b>' + Highcharts.dateFormat('%b %Y', new Date(this.x)) + '<br>' + '<b>Total applications: </b>' + Math.round(this.points[0].total / 500) * 500 / 1000 + 'k' + '<br>' + '<b>Partial benefit: </b>' + this.points[1].y.toFixed(1) + '%<br>' + '<b>Full benefit: </b>' + this.points[2].y.toFixed(1) + '%' ; }")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
  })
}

## To be copied in the UI
# mod_08_spotlight_students_ui("08_spotlight_students_1")

## To be copied in the server
# callModule(mod_08_spotlight_students_server, "08_spotlight_students_1")
