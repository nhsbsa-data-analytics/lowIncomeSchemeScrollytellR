#' 07_take_up_scatter_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_take_up_scatter_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        # align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_successful_individuals_by_la_imd"),
          height = "500px"
        )
      ),
      column(
        width = 12,
        style = "background-color: #FFFFFF;",
        # change to highcharter LA map but zoomed in
        highcharter::highchartOutput(
          outputId = ns("plot_selected_region_la"),
          height = "470px"
        ),
        shiny::selectInput(
          inputId = ns("input_year"),
          label = "Financial Year:",
          choices = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20"),
          selected = "2019/20",
          width = "60%"
        )
      )
    )
  )
}

#' 06_take_up Server Function
#'
#' @noRd
mod_07_take_up_scatter_graph_server <- function(input, output, session, region_name) {
  ns <- session$ns


  # Pull the drop down value
  region_sel <- reactive({
    region_name$input
  })

  observe({
    print(region_sel())
  })

  year <- reactive({
    input$input_year
  })

  output$plot_successful_individuals_by_la_imd <- highcharter::renderHighchart({

    # Calculate %s
    plot_df <- lowIncomeSchemeScrollytellR::adult_population_df %>%
      dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_la_df) %>%
      dplyr::mutate(
        p = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
      )

    # Format for highcharter animation
    plot_sequence_df <- plot_df %>%
      tidyr::expand(FINANCIAL_YEAR, tidyr::nesting(PCD_REGION_NAME, PCD_LAD_NAME, PCD_LAD_IMD_RANK)) %>%
      dplyr::left_join(plot_df) %>%
      dplyr::mutate(p = tidyr::replace_na(p)) %>%
      dplyr::group_by(PCD_REGION_NAME, PCD_LAD_NAME, PCD_LAD_IMD_RANK) %>%
      dplyr::do(sequence = .$p) %>%
      # Mutate to create color variable to reflect selected region with lightgrey and darkblue colour
      dplyr::mutate(color = ifelse(PCD_REGION_NAME == region_sel(), "#003087", "#DDE1E4"))

    # Create plot
    plot_sequence_df %>%
      highcharter::hchart(
        type = "scatter",
        highcharter::hcaes(x = PCD_LAD_IMD_RANK, y = sequence, color = color)
      ) %>%
      # Add two dummy series for the legend
      highcharter::hc_add_series(
        data = NULL,
        name = "Selected Region",
        showInLegend = TRUE,
        color = "#003087"
      ) %>%
      highcharter::hc_add_series(
        data = NULL,
        name = "Other Regions",
        showInLegend = TRUE,
        color = "#DDE1E4"
      ) %>%
      highcharter::hc_motion(
        labels = unique(plot_df$FINANCIAL_YEAR),
        startIndex = 4
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa(stacking = NA)) %>%
      highcharter::hc_title(
        text = "Estimated take-up of NHS Low Income Scheme by IMD Rank for English Local Authorities (2015/16 to 2019/20)"
      ) %>%
      highcharter::hc_xAxis(
        min = 1,
        max = 320, # Pad to ensure we can see the 314 label
        categories = c(NA, "1<br>Most<br>deprived", rep(NA, 312), "314<br>Least<br>deprived"),
        labels = list(step = 313),
        title = list(text = "")
      ) %>%
      highcharter::hc_yAxis(
        max = ceiling(max(plot_df$p) / 5) * 5,
        title = list(text = "")
      ) %>%
      highcharter::hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>Local authority:</b> {point.PCD_LAD_NAME} <br><b>IMD rank:</b> {point.x} <br><b>Take-up:</b> {point.y:.1f} (per thousand of the general population)"
      ) %>%
      highcharter::hc_chart(marginBottom = 125) %>%
      highcharter::hc_plotOptions(
        series = list(
          marker = list(radius = 3),
          stickyTracking = FALSE,
          states = list(
            hover = list(
              halo = list(
                size = 20,
                attributes = list(
                  fill = "#ED8B00"
                )
              )
            )
          )
        )
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
  })

  output$plot_selected_region_la <- highcharter::renderHighchart({

    # filter local authority by input variable.

    la_imd_count <- lowIncomeSchemeScrollytellR::imd_decile_df %>%
      # filter here first to hold of selected region value
      dplyr::filter(PCD_REGION_NAME == region_sel()) %>%
      # complete and fill to keep all IMD DECILE values from 1- 10
      tidyr::complete(INDEX_OF_MULT_DEPRIV_DECILE,
        tidyr::nesting(PCD_LAD_NAME, PCD_REGION_NAME),
        fill = list(IMD_DECILE_COUNT_LAD = 0, IMD_DECILE_P = 0)
      ) %>%
      dplyr::select(INDEX_OF_MULT_DEPRIV_DECILE, PCD_LAD_NAME, IMD_DECILE_P) %>%
      # convert to ttdata (nesting and purrr::map)
      tidyr::nest(-PCD_LAD_NAME) %>%
      dplyr::mutate(
        data = purrr::map(data, highcharter::mutate_mapping,
          highcharter::hcaes(
            x = INDEX_OF_MULT_DEPRIV_DECILE,
            y = IMD_DECILE_P
          ),
          drop = TRUE
        ),
        data = purrr::map(data, highcharter::list_parse)
      ) %>%
      dplyr::rename(ttdata = data)

    # la data frame, change to sequence data but only for selected region

    plot_df <- lowIncomeSchemeScrollytellR::adult_population_df %>%
      dplyr::filter(PCD_REGION_NAME == region_sel()) %>%
      dplyr::filter(FINANCIAL_YEAR == year()) %>%
      dplyr::group_by(FINANCIAL_YEAR, PCD_LAD_NAME) %>%
      dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_la_df) %>%
      dplyr::mutate(
        value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
      ) %>%
      dplyr::select(FINANCIAL_YEAR, PCD_LAD_NAME, value) %>%
      dplyr::inner_join(la_imd_count)

    # plot_sequence_series <- plot_df %>%
    #   tidyr::complete(FINANCIAL_YEAR, PCD_LAD_NAME,
    #                   fill = list(value = 0)) %>%
    #   dplyr::group_by(PCD_LAD_NAME) %>%
    #   dplyr::do(sequence = .$value) %>%
    #   highcharter::list_parse()

    # filter la_map as well

    la_map <- lowIncomeSchemeScrollytellR::la_map %>%
      dplyr::inner_join(lowIncomeSchemeScrollytellR::region_la_lookup) %>%
      dplyr::filter(PCD_REGION_NAME == region_sel()) %>%
      sf::st_transform(crs = 27700) %>%
      geojsonsf::sf_geojson() %>%
      jsonlite::fromJSON(simplifyVector = F)

    # create plot (first without tooltip)

    highcharter::highchart(type = "map") %>%
      # highcharter::hc_chart(marginBottom = 100) %>%
      highcharter::hc_add_series(
        data = plot_df,
        mapData = la_map,
        joinBy = "PCD_LAD_NAME",
        tooltip = list(
          headerFormat = "",
          pointFormat = "<b>Region:</b> {point.PCD_LAD_NAME}<br><b>Take-up:</b> {point.value:.1f} (per thousand of the general population)"
        )
      ) %>%
      # highcharter::hc_motion(
      #   labels = unique(plot_df$FINANCIAL_YEAR),
      #   startIndex = 4
      # ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      # highcharter::hc_title(
      #   text = "Estimated take-up of NHS Low Income Scheme (2015/16 to 2019/20)"
      # ) %>%
      highcharter::hc_colorAxis(min = 0, max = 20) %>%
      highcharter::hc_tooltip(
        useHTML = TRUE,
        headerFormat = "<b>{point.key}</b>",
        backgroundColor = "rgba(255,255,255,1)",
        style = list(opacity = 0),
        pointFormatter = highcharter::tooltip_chart(
          accesor = "ttdata",
          hc_opts = list(
            title = list(
              text = "point.PCD_LAD_NAME",
              size = 7
            ),
            chart = list(type = "column"),
            xAxis = list(
              title = list(text = "Deprivation"), # doesn't show and i dont know why!
              min = 1,
              max = 10,
              type = "category",
              labels = list(step = 1)
            ),
            yAxis = list(
              title = list(
                text = "% in deprivation",
                align = "high"
              ),
              type = "category",
              labels = list(format = "{value:.0f}%")
            ),
            series = list(list(color = "#425563"))
          ),
          height = 200,
          width = 250
        )
      )
  })
}

## To be copied in the UI
# mod_07_take_up_scatter_graph_ui("07_take_up_scatter_graph_1")

## To be copied in the server
# callModule(mod_07_take_up_scatter_graph_server, "07_take_up_scatter_graph_1")
