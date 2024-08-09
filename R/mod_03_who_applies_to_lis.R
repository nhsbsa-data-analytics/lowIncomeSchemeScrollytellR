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
    h2("Who applies to the NHS Low Income Scheme in England?"),
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
    nhs_card(
      heading = "Age band of NHS Low Income Scheme lead applicants in England (2015/16 to 2020/21)",
      nhs_selectInput(
        inputId = ns("slider_individuals_by_age_band"),
        label = "Financial year",
        choices = c(
          "2015/16",
          "2016/17",
          "2017/18",
          "2018/19",
          "2019/20",
          "2020/21"
        ),
        selected = "2019/20",
        full_width = TRUE
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_individuals_by_age_band"),
        height = "250px"
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "This excludes lead applicants without an age band."
      ),
      mod_nhs_download_ui(
        id = ns("download_individuals_by_age_band")
      )
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
    nhs_card(
      heading = "Client group of NHS Low Income Scheme applications in England (2015/16 to 2020/21)",
      highcharter::highchartOutput(
        outputId = ns("plot_individuals_by_client_group"),
        height = "350px"
      ),
      nhs_grid_2_col(
        tags$text(
          class = "highcharts-caption",
          style = "font-size: 9pt",
          "This excludes lead applicants with an unknown client group."
        ),
        mod_nhs_download_ui(
          id = ns("download_individuals_by_client_group")
        )
      )
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
    nhs_card(
      heading = "Deprivation decile of NHS Low Income Scheme individuals in England (2015/16 to 2020/21)",
      nhs_selectInput(
        inputId = ns("slider_individuals_by_deprivation"),
        label = "Financial year",
        choices = c(
          "2015/16",
          "2016/17",
          "2017/18",
          "2018/19",
          "2019/20",
          "2020/21"
        ),
        selected = "2019/20",
        full_width = TRUE
      ),
      highcharter::highchartOutput(
        outputId = ns("plot_individuals_by_deprivation"),
        height = "300px"
      ),
      mod_nhs_download_ui(
        id = ns("download_individuals_by_deprivation")
      )
    )
  )
}

#' 03_who_applies_to_lis Server Functions
#'
#' @noRd
mod_03_who_applies_to_lis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Individuals by age band

    # Add data to download button for all data
    mod_nhs_download_server(
      id = "download_individuals_by_age_band",
      filename = "individuals_by_age_band.csv",
      export_data = lowIncomeSchemeScrollytellR::individuals_by_age_band_df
    )

    # Bar chart by age band with an animation for financial year
    output$plot_individuals_by_age_band <- highcharter::renderHighchart({
      req(input$slider_individuals_by_age_band)

      # Create chart
      lowIncomeSchemeScrollytellR::individuals_by_age_band_df %>%
        # Filter the data based on the slider value
        dplyr::filter(FINANCIAL_YEAR == input$slider_individuals_by_age_band) %>%
        # Chart
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(x = BAND_5YEARS, y = PCT_INDIVIDUALS),
          animation = FALSE
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_legend(enabled = FALSE) %>%
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
                '<b>Percentage: </b>' + Highcharts.numberFormat(this.point.y, 1) + '%'

              return outHTML

            }
            "
          )
        )
    })

    # Client group

    # Add data to download button
    mod_nhs_download_server(
      id = "download_individuals_by_client_group",
      filename = "individuals_by_client_group.csv",
      export_data = lowIncomeSchemeScrollytellR::individuals_by_client_group_df
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
        highcharter::hc_yAxis(
          max = 100,
          title = list(text = "Percentage of applications")
        ) %>%
        highcharter::hc_xAxis(title = list(text = "Financial year")) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          headerFormat = "<b> {point.name} </b>",
          valueSuffix = "%",
          valueDecimals = 1
        )
    })

    # Deprivation

    # Add data to download button for all data
    mod_nhs_download_server(
      id = "download_individuals_by_deprivation",
      filename = "individuals_by_deprivation.csv",
      export_data = lowIncomeSchemeScrollytellR::individuals_by_imd_health_df
    )

    # Column plot by deprivation with animation over time
    output$plot_individuals_by_deprivation <- highcharter::renderHighchart({
      req(input$slider_individuals_by_deprivation)

      # Create chart
      lowIncomeSchemeScrollytellR::individuals_by_imd_health_df %>%
        # Filter the data based on the slider value
        dplyr::filter(FINANCIAL_YEAR == input$slider_individuals_by_deprivation) %>%
        # Chart
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = DECILE,
            y = PCT_INDIVIDUALS,
            group = DEPRIVATION
          ),
          animation = FALSE
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_xAxis(
          categories = c(0, "1<br>Most<br>deprived", 2:9, "10<br>Least<br>deprived"),
          title = list(text = "Deprivation decile"),
          reversed = FALSE
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
                '<b>Percentage: </b>' + Highcharts.numberFormat(this.point.y, 1) + '%'

              return outHTML

            }
            "
          )
        )
    })
  })
}

## To be copied in the UI
# mod_03_who_applies_to_lis_ui("03_who_applies_to_lis_ui_1")

## To be copied in the server
# mod_03_who_applies_to_lis_server("03_who_applies_to_lis_ui_1")
