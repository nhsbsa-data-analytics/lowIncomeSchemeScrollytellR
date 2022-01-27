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
    p(
      "Student applications were considered in more detail, given the rate of ",
      "decline in applications."
    ),
    p("Of student applicants in England in 2019/20:"),
    tags$ul(
      tags$li("The majority (98%) apply as single applicants."),
      tags$li(
        "Over half (52%) are aged 20 to 24 years and 37% are aged 15 to 19 ",
        "years."
      )
    ),
    br(),
    h6("A stronger downward trend in student applications in England"),
    p(
      "The total number of", tags$b(" student applications decreased by 29% "),
      "between 2015/16 and 2019/20, compared to a 10% decrease in non student ",
      "applications. It has decreased further since the beginning of the ",
      "COVID-19 pandemic."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Number of NHS Low Income Scheme Student applications in England ",
        "(2015/16 to 2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_student_applications"),
        height = "300px"
      )
    ),
    mod_download_ui(id = ns("download_student_applications")),
    br(),
    tags$em(
      "It's a great scheme and efficiently run. I am extremely grateful for ",
      "the help with prescription costs. However, I feel it is not advertised ",
      "very well especially to students and people who do not know other ",
      "people on benefits.(Student applicant)"
    ),
    br(),
    p(
      "Student applications are more likely to receive partial benefit than ",
      "non student applications and are less likely to be unsuccessful. There ",
      "is a less of a decline in the percentage of student applications ",
      "resulting in full or partial benefit than non student applications ",
      "between 2015/16 and 2019/20."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Outcome of NHS Low Income Scheme Student applications in England ",
        "(2015/16 to 2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_student_applications_by_outcome"),
        height = "300px"
      )
    ),
    mod_download_ui(id = ns("download_student_applications_by_outcome")),
    br(),
    h6("Estimated student take-up is also low and decreasing over time."),
    p(
      "To determine estimated take-up for students, we have used a proxy for ",
      "the eligible population as the number of enrolled students in higher ",
      "education in England by academic year."
    ),
    p(
      "Estimated take-up is the number of people receiving full or partial ",
      "benefit from the NHS Low Income Scheme from student applications, as a ",
      "percentage of the eligible population."
    ),
    p(
      "Estimated take-up was around 4% in the 2015/16 Academic Year reducing ",
      "to 2% in 2019/20."
    ),
    fluidRow(
      col_5(
        br(),
        br(),
        br(),
        p(
          "Estimated student take-up, continues to be somewhat",
          tags$b(" lower in the South East and London."), "This appears to ",
          "be primarily due to lower numbers of applications overall relative ",
          "to the population."
        ),
        br(),
        p("And student take-up is", tags$b("highest in the North West.")),
        br(),
        p(
          "The take-up rate has",
          tags$b("decreased most in the East of England"), "between 2015/16 ",
          "and 2019/20."
        )
      ),
      col_6(
        offset = 1,
        style = "background-color: #FFFFFF;",
        h6(
          "Estimated take-up of NHS Low Income Scheme Student individuals by ",
          "England region 2015/16 to 2019/20"
        ),
        highcharter::highchartOutput(
          outputId = ns("plot_successful_student_individuals_by_region"),
          height = "600px"
        ),
        mod_download_ui(id = ns("download_student_individuals_by_region"))
      )
    )
  )
}

#' 08_spotlight_students Server Functions
#'
#' @noRd
mod_08_spotlight_students_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Stacked column of student applications over time
    output$plot_student_applications <- highcharter::renderHighchart({

      # Create plot
      lowIncomeSchemeScrollytellR::applications_student_df %>%
        highcharter::hchart(
          type = "line",
          highcharter::hcaes(
            x = FINANCIAL_YEAR, 
            y = TOTAL_APPLICATIONS, 
            group = TYPE
          )
        ) %>%
        theme_nhsbsa(palette = "highlight") %>%
        highcharter::hc_legend(reversed = TRUE) %>%
        highcharter::hc_xAxis(
          title = list(text = "Financial year")
        ) %>%
        highcharter::hc_yAxis(
          labels = list(
            formatter = highcharter::JS(
              "
              function(){ 
              
                return (Math.abs(this.value) / 1000) + 'k'
              
              }
              "
            )
          ),
          title = list(text = "Total Applications")
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            "
            function () { 
              
              outHTML = 
                '<b>Client Group: </b>' + this.series.name + '<br>' + 
                '<b>Total Applications: </b>' + (Math.round(this.point.y / 500) * 500 / 1000).toFixed(0) + 'k'
              
              return outHTML
            
            }
            "
          )
        )
    })


    mod_download_server(
      id = "download_student_applications",
      filename = "student_applications.csv",
      export_data = lowIncomeSchemeScrollytellR::applications_student_df
    )


    output$plot_student_applications_by_outcome <- highcharter::renderHighchart({

      # Format for highcharter animation
      plot_sequence_series_list <- 
        lowIncomeSchemeScrollytellR::applications_outcome_student_df %>%
        tidyr::expand(FINANCIAL_YEAR, TYPE, OUTCOME_LEVEL2) %>%
        dplyr::left_join(
          y = lowIncomeSchemeScrollytellR::applications_outcome_student_df
        ) %>%
        dplyr::mutate(p = tidyr::replace_na(PCT_OUTCOMES, 0)) %>%
        dplyr::group_by(TYPE, OUTCOME_LEVEL2) %>%
        dplyr::do(data = list(sequence = .$PCT_OUTCOMES)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(OUTCOME_LEVEL2) %>%
        dplyr::do(data = .$data) %>%
        dplyr::mutate(name = OUTCOME_LEVEL2) %>%
        highcharter::list_parse()

      # Create plot
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "bar", marginBottom = 100) %>%
        highcharter::hc_add_series_list(plot_sequence_series_list) %>%
        highcharter::hc_motion(
          labels = unique(lowIncomeSchemeScrollytellR::applications_outcome_student_df$FINANCIAL_YEAR),
          series = c(0, 1, 2, 3, 4, 5, 6),
          startIndex = 4
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_caption(
          text = "Excludes ongoing applications.",
          align = "right"
        ) %>%
        highcharter::hc_legend(reversed = TRUE) %>%
        highcharter::hc_xAxis(
          categories = unique(lowIncomeSchemeScrollytellR::applications_outcome_student_df$TYPE)
        ) %>%
        highcharter::hc_yAxis(
          max = 100,
          title = list(text = "Percentage of applications")
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          headerFormat = "<b> {point.name} </b>", 
          valueSuffix = "%", 
          valueDecimals = 1
        )
      
    })


    mod_download_server(
      id = "download_student_applications_by_outcome",
      filename = "student_non_student_applications_by_outcome.csv",
      export_data = 
        lowIncomeSchemeScrollytellR::applications_outcome_student_df %>%
        dplyr::rename(CATEGORY = TYPE, OUTCOME = OUTCOME_LEVEL2)
    )

    # Calculate rate per region
    student_individuals_by_region <- 
      lowIncomeSchemeScrollytellR::student_population_df %>%
      dplyr::filter(ACADEMIC_YEAR != "2014/15") %>%
      dplyr::inner_join(
        y = lowIncomeSchemeScrollytellR::successful_student_individuals_by_region_df
      ) %>%
      dplyr::mutate(
        value = janitor::round_half_up(
          x = TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS / 
            TOTAL_STUDENT_POPULATION * 100, 
          digits = 1
        ),
        TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS = 
          round(TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS, -1)
      )

    output$plot_successful_student_individuals_by_region <- 
      highcharter::renderHighchart({

      # Format for highcharter animation
      plot_sequence_series <- student_individuals_by_region %>%
        tidyr::complete(
          ACADEMIC_YEAR, PCD_REGION_NAME,
          fill = list(value = 0)
        ) %>%
        dplyr::group_by(PCD_REGION_NAME) %>%
        dplyr::do(sequence = .$value) %>%
        highcharter::list_parse()

      # Create plot
      highcharter::highchart(type = "map") %>%
        highcharter::hc_chart(marginBottom = 70) %>%
        highcharter::hc_add_series(
          data = plot_sequence_series,
          mapData = region_map,
          joinBy = "PCD_REGION_NAME",
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Region:</b> {point.PCD_REGION_NAME}<br><b>Take-up:</b> {point.value}% (of the student population)<br>"
          )
        ) %>%
        highcharter::hc_motion(
          labels = unique(student_individuals_by_region$ACADEMIC_YEAR),
          startIndex = 4
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_subtitle(
          text = "Students were allocated to a region based on the address on the application."
        ) %>%
        highcharter::hc_colorAxis(min = 0, max = 6) %>%
        highcharter::hc_legend(enabled = FALSE)
    })

    mod_download_server(
      id = "download_student_individuals_by_region",
      file = "student_take_up.csv",
      export_data = student_individuals_by_region %>%
        dplyr::rename(STUDENT_TAKE_UP = value)
    )
  })
}

## To be copied in the UI
# mod_08_spotlight_students_ui("08_spotlight_students_ui_1")

## To be copied in the server
# mod_08_spotlight_students_server("08_spotlight_students_ui_1")
