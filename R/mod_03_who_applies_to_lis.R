#' 03_who_applies_to_lis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_who_applies_to_lis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Who applies to the NHS Low Income Scheme in England?"),
    p(
      "Around 7 in 10 applicants are single applicants and the remainder are ",
      "applying as part of a couple/dual application."
    ),
    p(
      "We only hold age for the lead applicant (person making the ",
      "application). One in four lead applicants are aged 15 to 24 years in ",
      "2019/20. This age group have the highest number of applications in ",
      "each year observed."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Age band of NHS Low Income Scheme lead applicants in England ",
        "(2015/16 to 2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_individuals_by_age_band"),
        height = "300px"
      ),
      HTML("<label for='play-range' style = 'visibility: hidden;'>year-range</label>"),
    ),
    mod_nhs_download_ui(
      id = ns("download_individuals_by_age_band")
    ),
    br(),
    p(
      "Applicants are allocated a client group based on the main source ",
      "of household income. They are most likely to be categorised as ",
      tippy(
        text = "benefits/other",
        tooltip = tooltip_text$benefits_others
      ),
      " or earners."
    ),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Client group of NHS Low Income Scheme applications in England ",
        "(2015/16 to 2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_individuals_by_client_group"),
        height = "300px"
      )
    ),
    mod_nhs_download_ui(
      id = ns("download_individuals_by_client_group")
    ),
    br(),
    p(
      "Individuals covered by the application are more likely to live in ",
      "areas of higher deprivation, as we can see in the below ",
      tippy(
        text = "English indices of deprivation",
        tooltip = tooltip_text$english_imd
      ),
      " decile chart."
    ),
    p("This trend is consistent in each year observed."),
    fluidRow(
      align = "center",
      style = "background-color: #FFFFFF;",
      h6(
        "Deprivation decile of NHS Low Income Scheme individuals in England ",
        "(2015/16 to 2020/21)"
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_individuals_by_deprivation"),
        height = "300px"
      )
    ),
    mod_nhs_download_ui(
      id = ns("download_individuals_by_deprivation")
    )
  )
}

#' 03_who_applies_to_lis Server Functions
#'
#' @noRd
mod_03_who_applies_to_lis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pyramid plot for age band
    output$plot_individuals_by_age_band <- highcharter::renderHighchart({

      # Format for highcharter animation
      plot_series_list <- lowIncomeSchemeScrollytellR::individuals_by_age_band_df %>%
        tidyr::complete(
          FINANCIAL_YEAR, BAND_5YEARS,
          fill = list(value = 0)
        ) %>%
        dplyr::group_by(BAND_5YEARS) %>%
        dplyr::do(data = list(sequence = .$PCT_INDIVIDUALS)) %>%
        dplyr::ungroup() %>%
        # dplyr::group_by(FINANCIAL_YEAR) %>%
        dplyr::do(data = .$data) %>%
        dplyr::mutate(name = "Age Band (5 Year)") %>%
        highcharter::list_parse()

      # Create plot
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "column", marginBottom = 100) %>%
        highcharter::hc_add_series_list(x = plot_series_list) %>%
        highcharter::hc_motion(
          labels = unique(lowIncomeSchemeScrollytellR::individuals_by_age_band_df$FINANCIAL_YEAR),
          series = c(0, 1),
          startIndex = 4
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_caption(
          text = paste(
            "This excludes lead applicants without an age band.",
            "<br>", "Percentages are rounded to one decimal."
          ),
          align = "right"
        ) %>%
        highcharter::hc_xAxis(
          title = list(text = "Age band (5 years)"),
          categories = sort(unique(lowIncomeSchemeScrollytellR::individuals_by_age_band_df$BAND_5YEARS)),
          reversed = FALSE
        ) %>%
        highcharter::hc_yAxis(
          max = max(lowIncomeSchemeScrollytellR::individuals_by_age_band_df$PCT_INDIVIDUALS),
          title = list(text = "Percentage of applicants")
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            "
            function () {

              outHTML =
                '<b>Age band (5 years): </b>' + this.point.category + '<br/>' +
                '<b>Percentage: </b>' + this.point.y + '%'

              return outHTML

            }
            "
          )
        )
    })

    # Add data to download button
    mod_nhs_download_server(
      id = "download_individuals_by_age_band",
      filename = "applicants_age_band.csv",
      export_data = lowIncomeSchemeScrollytellR::individuals_by_age_band_df %>%
        dplyr::rename(
          COUNT_APPLICANTS_BY_AGE_GROUP = TOTAL_INDIVIDUALS,
          PERCENTAGE_APPLICANTS_BY_AGE_GROUP = PCT_INDIVIDUALS
        )
    )

    # Stacked column plot by client group
    output$plot_individuals_by_client_group <- highcharter::renderHighchart({

      # Create plot
      lowIncomeSchemeScrollytellR::individuals_by_client_group_df %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = FINANCIAL_YEAR,
            y = PCT_INDIVIDUALS,
            group = CLIENTGROUP_DESC_FORMAT
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_caption(
          text = paste(
            "This excludes lead applicants with an unknown client group.",
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
    mod_nhs_download_server(
      id = "download_individuals_by_client_group",
      filename = "individual_client_group.csv",
      export_data =
        lowIncomeSchemeScrollytellR::individuals_by_client_group_df %>%
          dplyr::rename(
            COUNT_APPLICANTS_BY_OUTCOME = TOTAL_INDIVIDUALS,
            PERCENTAGE_APPLICANTS_BY_OUTCOME = PCT_INDIVIDUALS
          )
    )


    # Column plot by deprivation
    output$plot_individuals_by_deprivation <- highcharter::renderHighchart({

      # Pull the max p
      max_p <- max(abs(lowIncomeSchemeScrollytellR::individuals_by_imd_health_df$PCT_INDIVIDUALS))

      # Format for highcharter animation
      plot_series_list <- lowIncomeSchemeScrollytellR::individuals_by_imd_health_df %>%
        tidyr::complete(FINANCIAL_YEAR, DECILE, DEPRIVATION,
          fill = list(value = 0)
        ) %>%
        dplyr::group_by(DECILE, DEPRIVATION) %>%
        dplyr::do(data = list(sequence = .$PCT_INDIVIDUALS)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(DEPRIVATION) %>%
        dplyr::do(data = .$data) %>%
        dplyr::mutate(name = DEPRIVATION) %>%
        highcharter::list_parse()

      # Create plot
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "column", marginBottom = 120) %>%
        highcharter::hc_add_series_list(x = plot_series_list) %>%
        highcharter::hc_motion(
          labels = unique(lowIncomeSchemeScrollytellR::individuals_by_imd_health_df$FINANCIAL_YEAR),
          series = c(0, 1),
          startIndex = 4
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_caption(
          text = "Percentages are rounded to one decimal.",
          align = "right"
        ) %>%
        highcharter::hc_xAxis(
          categories = c("1<br>Most<br>deprived", 2:9, "10<br>Least<br>deprived"),
          title = list(text = "Deprivation decile")
        ) %>%
        highcharter::hc_yAxis(
          max = max(lowIncomeSchemeScrollytellR::individuals_by_imd_health_df$PCT_INDIVIDUALS),
          title = list(
            text = "Percentage of individuals covered by the application"
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            "
            function () {

              outHTML =
                '<b>Deprivation: </b>' + this.series.name + '<br>' +
                '<b>Decile: </b>' + parseInt(this.point.category) + '<br>' +
                '<b>Percentage: </b>' + this.point.y + '%'

              return outHTML

            }
            "
          )
        )
    })

    # Add data to download button
    mod_nhs_download_server(
      id = "download_individuals_by_deprivation",
      filename = "individual_deprivation.csv",
      export_data =
        lowIncomeSchemeScrollytellR::individuals_by_imd_health_df %>%
          dplyr::rename(
            COUNT_APPLICANTS_BY_OUTCOME = TOTAL_INDIVIDUALS,
            PERCENTAGE_APPLICANTS_BY_OUTCOME = PCT_INDIVIDUALS
          )
    )
  })
}

## To be copied in the UI
# mod_03_who_applies_to_lis_ui("03_who_applies_to_lis_ui_1")

## To be copied in the server
# mod_03_who_applies_to_lis_server("03_who_applies_to_lis_ui_1")
