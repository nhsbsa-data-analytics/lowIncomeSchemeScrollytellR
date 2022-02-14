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
    h2("Spotlight on student applicants in England"),
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
    p(tags$b("A stronger downward trend in student applications in England")),
    p(
      "The total number of", tags$b(" student applications decreased by 29% "),
      "between 2015/16 and 2019/20, compared to a 10% decrease in non-student ",
      "applications. It has decreased further since the beginning of the ",
      "COVID-19 pandemic."
    ),
    nhs_card(
      heading = "Number of NHS Low Income Scheme Student applications in England (2015/16 to 2020/21)",
      highcharter::highchartOutput(
        outputId = ns("plot_student_applications"),
        height = "300px"
      ),
      mod_nhs_download_ui(id = ns("download_student_applications"))
    ),
    br(),
    p(
      tags$i(
        style = "padding: 0 10%; display: block;",
        "“It's a great scheme and efficiently run. I am extremely grateful ",
        "for the help with prescription costs. However, I feel it is not ",
        "advertised very well especially to students and people who do not ",
        "know other people on benefits.” - (Student applicant)"
      )
    ),
    p(
      "Student applications are more likely to receive partial benefit than ",
      "non-student applications and are less likely to be unsuccessful. There ",
      "is a less of a decline in the percentage of student applications ",
      "resulting in full or partial benefit than non-student applications ",
      "between 2015/16 and 2019/20."
    ),
    nhs_card(
      heading = "Outcome of NHS Low Income Scheme Student applications in England (2015/16 to 2020/21)",
      highcharter::highchartOutput(
        outputId = ns("plot_student_applications_by_outcome"),
        height = "300px"
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "This excludes ongoing applications."
      ),
      nhs_grid_2_col(
        nhs_animated_sliderInput(
          inputId = ns("slider_student_applications_by_outcome"),
          choices = c(
            "2015/16",
            "2016/17",
            "2017/18",
            "2018/19",
            "2019/20",
            "2020/21"
          ),
          selected = "2019/20"
        ),
        mod_nhs_download_ui(
          id = ns("download_student_applications_by_outcome")
        )
      )
    ),
    br(),
    p(
      tags$b("Estimated student take-up is also low and decreasing over time.")
    ),
    p(
      "To determine estimated take-up for students, we have used a proxy for ",
      "the eligible population as the number of enrolled students in higher ",
      "education in England by academic year."
    ),
    p(
      "Estimated take-up is the number of people who receive full or partial ",
      "benefit from the NHS Low Income Scheme from student applications ",
      "submitted in the time period, as a percentage of the eligible population."
    ),
    p(
      "Estimated take-up was around 4% in the 2015/16 Academic Year reducing ",
      "to 2% in 2019/20."
    ),
    p(
      "Estimated student take-up, continues to be somewhat",
      tags$b(" lower in the South East and London."), "This appears to be ",
      "primarily due to lower numbers of applications overall relative to the ",
      "population."
    ),
    p("And student take-up is", tags$b("highest in the North West.")),
    p(
      "The take-up rate has",
      tags$b("decreased most in the East of England"), "between 2015/16 and ",
      "2019/20."
    ),
    nhs_card(
      heading = "Estimated take-up of NHS Low Income Scheme Student individuals by England region 2015/16 to 2019/20",
      nhs_grid_2_col(
        tags$div(
          highcharter::highchartOutput(
            outputId = ns("plot_successful_student_individuals_by_region"),
            height = "500px"
          ),
          tags$label(
            `for` = "play-range",
            class = "visuallyhidden",
            "year-range"
          )
        ),
        DT::DTOutput(
          outputId = ns("table_successful_student_individuals_by_region")
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Students were allocated to a region based on the address on the application."
      ),
      nhs_grid_2_col(
        nhs_animated_sliderInput(
          inputId = ns("slider_student_individuals_by_region"),
          choices = c(
            "2015/16",
            "2016/17",
            "2017/18",
            "2018/19",
            "2019/20"
          ),
          selected = "2019/20",
          # Slow as takes a while to load
          animate_interval = 1500
        ),
        mod_nhs_download_ui(
          id = ns("download_student_individuals_by_region")
        )
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

    # Number of applications over time

    # Create the chart
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

    # Download the data
    mod_nhs_download_server(
      id = "download_student_applications",
      filename = "student_applications.csv",
      export_data = lowIncomeSchemeScrollytellR::applications_student_df
    )

    # Student outcomes

    # Ensure we have all combinations of data
    applications_outcome_student_df <-
      lowIncomeSchemeScrollytellR::applications_outcome_student_df %>%
      tidyr::complete(
        FINANCIAL_YEAR, TYPE, OUTCOME_LEVEL2,
        fill = list(PCT_OUTCOMES = 0)
      )

    # Create chart
    output$plot_student_applications_by_outcome <- highcharter::renderHighchart({
      req(input$slider_student_applications_by_outcome)

      applications_outcome_student_df %>%
        dplyr::filter(
          FINANCIAL_YEAR == input$slider_student_applications_by_outcome
        ) %>%
        highcharter::hchart(
          type = "bar",
          highcharter::hcaes(
            x = TYPE,
            y = PCT_OUTCOMES,
            group = OUTCOME_LEVEL2
          ),
          animation = FALSE
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_legend(reversed = TRUE) %>%
        highcharter::hc_xAxis(
          categories = unique(applications_outcome_student_df$TYPE)
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

    # Download the data
    mod_nhs_download_server(
      id = "download_student_applications_by_outcome",
      filename = "student_non_student_applications_by_outcome.csv",
      export_data = applications_outcome_student_df %>%
        dplyr::rename(CATEGORY = TYPE, OUTCOME = OUTCOME_LEVEL2)
    )

    # Individuals by region

    # Calculate rate per region
    student_individuals_by_region_df <-
      lowIncomeSchemeScrollytellR::student_population_df %>%
      dplyr::filter(ACADEMIC_YEAR != "2014/15") %>%
      dplyr::inner_join(
        y = lowIncomeSchemeScrollytellR::successful_student_individuals_by_region_df
      ) %>%
      dplyr::mutate(
        PCT_SUCCESSFUL_STUDENT_INDIVIDUALS = janitor::round_half_up(
          x = TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS /
            TOTAL_STUDENT_POPULATION * 100,
          digits = 1
        ),
        TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS =
          round(TOTAL_SUCCESSFUL_STUDENT_INDIVIDUALS, -1)
      ) %>%
      # Get all combinations
      tidyr::complete(
        ACADEMIC_YEAR, PCD_REGION_NAME,
        fill = list(PCT_SUCCESSFUL_STUDENT_INDIVIDUALS = 0)
      )

    # Filter the data to AY (so we can use it for the map and table)
    student_individuals_by_region_filtered_df <- reactive({
      req(input$slider_student_individuals_by_region)

      student_individuals_by_region_df %>%
        dplyr::filter(
          ACADEMIC_YEAR == input$slider_student_individuals_by_region
        )
    })

    # Create a table to go alongside the map
    output$table_successful_student_individuals_by_region <- DT::renderDT(
      expr = {
        req(input$slider_student_individuals_by_region)

        # Format the table
        student_individuals_by_region_filtered_df() %>%
          dplyr::arrange(desc(PCT_SUCCESSFUL_STUDENT_INDIVIDUALS)) %>%
          dplyr::mutate(RANK = dplyr::row_number()) %>%
          dplyr::select(
            "<span class='nhsuk-body-s'>Rank</span>" := RANK,
            "<span class='nhsuk-body-s'>Region</span>" := PCD_REGION_NAME,
            "<span class='nhsuk-body-s'>Take up (%)</span>" :=
              PCT_SUCCESSFUL_STUDENT_INDIVIDUALS
          ) %>%
          DT::datatable(
            escape = FALSE,
            rownames = FALSE,
            options = list(dom = "t"),
            height = "400px",
            filter = "none"
          ) %>%
          DT::formatStyle(columns = 0:3, `font-size` = "14px") %>%
          DT::formatRound(
            columns = 3,
            digits = 1
          )
      }
    )


    # Create chart
    output$plot_successful_student_individuals_by_region <-
      highcharter::renderHighchart({
        req(input$slider_student_individuals_by_region)

        highcharter::highchart() %>%
          highcharter::hc_add_series_map(
            map = lowIncomeSchemeScrollytellR::region_map,
            df = student_individuals_by_region_filtered_df(),
            joinBy = "PCD_REGION_NAME",
            value = "PCT_SUCCESSFUL_STUDENT_INDIVIDUALS",
            tooltip = list(
              headerFormat = "",
              pointFormat = "<b>Region:</b> {point.PCD_REGION_NAME}<br><b>Take-up:</b> {point.value}% (of the student population)<br>"
            ),
            animation = FALSE
          ) %>%
          theme_nhsbsa() %>%
          highcharter::hc_colorAxis(min = 0, max = 6)
      })

    mod_nhs_download_server(
      id = "download_student_individuals_by_region",
      file = "student_take_up.csv",
      export_data = student_individuals_by_region_df
    )
  })
}

## To be copied in the UI
# mod_08_spotlight_students_ui("08_spotlight_students_ui_1")

## To be copied in the server
# mod_08_spotlight_students_server("08_spotlight_students_ui_1")
