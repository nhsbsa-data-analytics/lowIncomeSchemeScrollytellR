#' 07_take_up_la UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_take_up_la_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "The chart and map show estimated take-up by local authority ",
      "area relative to the population and deprivation profile of a ",
      "local authority, based on ",
      enurl(
        url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833947/IoD2019_Research_Report.pdf",
        text = "MHCLG methodology."
      )
    ),
    nhs_card(
      heading = "Estimated take-up of NHS Low Income Scheme by Index of Multiple Deprivation for English Local Authorities (2015/16 to 2020/21)",
      nhs_selectInput(
        inputId = ns("input_region"),
        label = "Region:",
        choices = c(
          "East Midlands",
          "East of England",
          "London",
          "North East",
          "North West",
          "South East",
          "South West",
          "West Midlands",
          "Yorkshire and The Humber"
        ),
        selected = "North West",
        full_width = FALSE
      ),
      shiny::htmlOutput(ns("text")),
      highcharter::highchartOutput(
        outputId = ns("scatter_successful_individuals_by_la_imd"),
        height = "300px"
      ),
      nhs_grid_2_col(
        highcharter::highchartOutput(
          outputId = ns("plot_selected_region_la"),
          height = "300px"
        ),
        highcharter::highchartOutput(
          outputId = ns("plot_imd_decile_by_selected_la"),
          height = "300px"
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Deprivation data uses 2019 estimates."
      ),
      nhs_grid_2_col(
        nhs_animated_sliderInput(
          inputId = ns("financial_year"),
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
        mod_nhs_download_ui(id = ns("la_take_up_download"))
      )
    )
  )
}

#' 07_take_up_la Server Functions
#'
#' @noRd
mod_07_take_up_la_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preprocess data
    merged_df <- lowIncomeSchemeScrollytellR::adult_population_df %>%
      dplyr::inner_join(
        y = lowIncomeSchemeScrollytellR::successful_individuals_by_la_df
      ) %>%
      dplyr::mutate(
        LA_TAKE_UP_PER_THOUSAND = janitor::round_half_up(
          x = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000,
          digits = 1
        )
      ) %>%
      tidyr::complete(
        FINANCIAL_YEAR,
        tidyr::nesting(PCD_REGION_NAME, PCD_LAD_NAME, PCD_LAD_IMD_RANK),
        fill = list(
          LA_TAKE_UP_PER_THOUSAND = 0,
          TOTAL_SUCCESSFUL_INDIVIDUALS = 0
        )
      )

    # Filter to the financial year
    financial_year_df <- reactive({
      req(input$input_region)

      merged_df %>%
        dplyr::filter(FINANCIAL_YEAR == input$financial_year)
    })

    # Filter to region
    region_df <- reactive({
      req(input$financial_year)
      req(input$input_region)

      financial_year_df() %>%
        dplyr::filter(PCD_REGION_NAME == input$input_region)
    })

    # Create scatter chart
    output$scatter_successful_individuals_by_la_imd <- highcharter::renderHighchart({
      req(input$financial_year)
      req(input$input_region)

      # Create chart
      financial_year_df() %>%
        # Indicate selected region
        dplyr::mutate(
          GROUP = ifelse(
            test = PCD_REGION_NAME == input$input_region,
            yes = "Selected region",
            no = "Other regions"
          )
        ) %>%
        highcharter::hchart(
          type = "scatter",
          highcharter::hcaes(
            x = PCD_LAD_IMD_RANK,
            y = LA_TAKE_UP_PER_THOUSAND,
            group = GROUP
          ),
          animation = FALSE
        ) %>%
        theme_nhsbsa(stack = NA, palette = "highlight") %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_legend(reversed = TRUE) %>%
        highcharter::hc_xAxis(
          min = 1,
          max = 330, # Pad to ensure we can see the 314 label
          categories = c(NA, "1<br>Most deprived", rep(NA, 312), "314<br>Least deprived"),
          labels = list(step = 313),
          title = list(text = "Deprivation rank")
        ) %>%
        highcharter::hc_yAxis(
          max = max(merged_df$LA_TAKE_UP_PER_THOUSAND),
          title = list(text = "Take-up per thousand of the population aged 16+ years")
        ) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>Local authority:</b> {point.PCD_LAD_NAME} <br><b>2019 IMD rank:</b> {point.x} <br><b>Take-up:</b> {point.y:.1f} (per thousand of the general population)"
        )
    })

    # click local authority and show decile distribution
    output$plot_selected_region_la <- highcharter::renderHighchart({
      req(input$financial_year)
      req(input$input_region)

      highcharter::highchart() %>%
        highcharter::hc_add_series_map(
          map = lowIncomeSchemeScrollytellR::la_map %>%
            dplyr::inner_join(region_df() %>% dplyr::select(PCD_LAD_NAME)) %>%
            sf::st_transform(crs = 27700) %>%
            geojsonsf::sf_geojson() %>%
            jsonlite::fromJSON(simplifyVector = FALSE),
          df = region_df(),
          joinBy = "PCD_LAD_NAME",
          value = "LA_TAKE_UP_PER_THOUSAND",
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Local Authority: </b> {point.PCD_LAD_NAME}<br><b>Take-up: </b> {point.value:.1f} (per thousand of the general population)"
          ),
          animation = FALSE
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_colorAxis(min = 0, max = 20) %>%
        highcharter::hc_plotOptions(
          map = list(
            events = list(
              click = htmlwidgets::JS(
                "
                function(event) {
                  Shiny.setInputValue('07_take_up_la_ui_1-mapclick', event.point.PCD_LAD_NAME);
                }
                "
              )
            )
          )
        )
    })


    # IMD chart
    observeEvent(input$mapclick, {
      output$plot_imd_decile_by_selected_la <- highcharter::renderHighchart({
        req(input$mapclick)

        la_imd_count <- lowIncomeSchemeScrollytellR::imd_decile_df %>%
          # complete and fill to keep all IMD DECILE values from 1- 10
          tidyr::complete(
            INDEX_OF_MULT_DEPRIV_DECILE,
            tidyr::nesting(PCD_LAD_NAME, PCD_REGION_NAME),
            fill = list(IMD_DECILE_COUNT_LAD = 0, IMD_DECILE_P = 0)
          ) %>%
          # filter here first to hold of selected region value
          dplyr::filter(PCD_LAD_NAME == input$mapclick) %>%
          dplyr::select(INDEX_OF_MULT_DEPRIV_DECILE, PCD_LAD_NAME, IMD_DECILE_P)

        la_imd_count %>%
          highcharter::hchart(
            type = "column",
            highcharter::hcaes(
              x = INDEX_OF_MULT_DEPRIV_DECILE,
              y = IMD_DECILE_P
            )
          ) %>%
          theme_nhsbsa() %>%
          highcharter::hc_title(text = glue::glue({
            input$mapclick
          })) %>%
          highcharter::hc_yAxis(
            title = list(
              text = "Percentage of LSOA in deprivation",
              align = "middle"
            ),
            labels = list(format = "{value:.0f}%")
          ) %>%
          highcharter::hc_xAxis(
            min = 1,
            max = 11, # Pad to ensure we can see the 10 label
            categories = c(NA, "1<br>Most<br>deprived", rep(NA, 8), "10<br>Least<br>deprived"),
            labels = list(step = 9),
            title = list(text = "Deprivation decile")
          ) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            formatter = highcharter::JS(
              "
              function () {

                outHTML =
                  '<b>2019 IMD decile: </b>' + this.point.x + '<br>' +
                  '<b>Percentage: </b>' + Highcharts.numberFormat(this.point.y, 1) + '%'

                return outHTML

              }
              "
            )
          )
      })
    })

    # Download data
    la_take_up_download_df <- reactive({
      lowIncomeSchemeScrollytellR::adult_population_df %>%
        dplyr::filter(PCD_REGION_NAME == input$input_region) %>%
        dplyr::inner_join(
          y = lowIncomeSchemeScrollytellR::successful_individuals_by_la_df
        ) %>%
        dplyr::mutate(
          LA_TAKE_UP_PER_THOUSAND = janitor::round_half_up(
            x = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000,
            digits = 1
          )
        ) %>%
        dplyr::left_join(
          y = lowIncomeSchemeScrollytellR::imd_decile_df %>%
            tidyr::complete(
              INDEX_OF_MULT_DEPRIV_DECILE,
              tidyr::nesting(PCD_LAD_NAME, PCD_REGION_NAME),
              fill = list(IMD_DECILE_COUNT_LAD = 0, IMD_DECILE_P = 0)
            ),
          by = c("PCD_LAD_NAME", "PCD_REGION_NAME")
        ) %>%
        dplyr::select(
          FINANCIAL_YEAR,
          PCD_LAD_NAME,
          PCD_REGION_NAME,
          PCD_LAD_IMD_RANK,
          LA_TAKE_UP_PER_THOUSAND,
          INDEX_OF_MULT_DEPRIV_DECILE,
          IMD_DECILE_P
        )
    })

    mod_nhs_download_server(
      id = "la_take_up_download",
      filename = "la_take_up.csv",
      export_data = la_take_up_download_df
    )


    # Take-up of selected region
    # this part will change depends on the region selection from the reactive value.
    output$text <- renderUI({
      req(input$financial_year)
      req(input$input_region)

      # dynamic text
      plot_df <- reactive({
        lowIncomeSchemeScrollytellR::adult_population_df %>%
          dplyr::inner_join(
            y = lowIncomeSchemeScrollytellR::successful_individuals_by_la_df
          ) %>%
          dplyr::filter(FINANCIAL_YEAR == input$financial_year) %>%
          dplyr::mutate(
            p = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
          ) %>%
          dplyr::filter(PCD_REGION_NAME == input$input_region)
      })

      la_count <- reactive({
        plot_df() %>%
          dplyr::distinct(PCD_LAD_NAME) %>%
          dplyr::count()
      })

      # which is the highest take-up local authority in the selected region.
      highest_take_up_la <- reactive({
        plot_df() %>%
          dplyr::filter(p == max(p)) %>%
          dplyr::select(PCD_LAD_NAME, PCD_LAD_IMD_RANK, p)
      })

      # which is the lowest take-up local authority in the selected region.
      lowest_take_up_la <- reactive({
        plot_df() %>%
          dplyr::filter(p == min(p)) %>%
          dplyr::select(PCD_LAD_NAME, PCD_LAD_IMD_RANK, p)
      })


      main_text <- paste(
        "In ",
        tags$b(paste0(input$financial_year, ","), highest_take_up_la()[1]), " ",
        "has the highest take-up per thousand of the general population in ",
        "the ", tags$b(input$input_region), ". Of all local authorities in ",
        "England the IMD rank in ", tags$b(highest_take_up_la()[1]), " is ",
        tags$b(highest_take_up_la()[2]), ". ",
        tags$b(lowest_take_up_la()[1]), " has the lowest take-up in the ",
        tags$b(input$input_region), ", with an IMD rank of ",
        tags$b(lowest_take_up_la()[2]),
        " out of all local authorities in England. ",
        sep = ""
      )

      additional_text <- paste(
        "Take-up is somewhat", tags$b(" lower, relative to deprivation "),
        "in five local authorities in the", tags$b(" North West (Knowsley,
          Hyndburn, Halton, St Helens and Barrow in Furness)."),
        sep = ""
      )

      tags$text(
        class = "highcharts-caption",
        switch(input$input_region,
          "North West" = HTML(paste(main_text, additional_text)),
          HTML(paste(main_text))
        )
      )
    })
  })
}

## To be copied in the UI
# mod_07_take_up_la_ui("07_take_up_la_ui_1")

## To be copied in the server
# mod_07_take_up_la_server("07_take_up_la_ui_1")
