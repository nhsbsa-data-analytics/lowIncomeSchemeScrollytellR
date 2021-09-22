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
        # offset = 1,
        width = 5,
        # align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_successful_individuals_by_la_imd"),
          height = "600px"
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
 
  
  output$plot_successful_individuals_by_la_imd = highcharter::renderHighchart({
    
    # Calculate %s
    plot_df <- lowincomeschemeucd::adult_population_df %>%
      dplyr::inner_join(lowincomeschemeucd::successful_individuals_by_la_df) %>%
      dplyr::mutate(
        p = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
      ) 

    # Format for highcharter animation
    plot_sequence_df <- plot_df %>%
      tidyr::expand(FINANCIAL_YEAR, tidyr::nesting(PCD_REGION_NAME,PCD_LAD_NAME, PCD_LAD_IMD_RANK)) %>%
      dplyr::left_join(plot_df) %>%
      dplyr::mutate(p = tidyr::replace_na(p)) %>%
      dplyr::group_by(PCD_REGION_NAME, PCD_LAD_NAME, PCD_LAD_IMD_RANK) %>%
      dplyr::do(sequence = .$p) %>% 
    # Mutate one variable to reflect selected region
      dplyr::mutate(selected_region = ifelse(PCD_REGION_NAME == region_sel(), "Selected", "Other Regions")
      )
    
    # Create plot
    plot_sequence_df %>%
      highcharter::hchart(
        type = "scatter", 
        highcharter::hcaes(x = PCD_LAD_IMD_RANK, y = sequence , color = selected_region )
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
        labels = list(step = 313)
      ) %>%
      highcharter::hc_yAxis(
        max = ceiling(max(plot_df$p) / 5) * 5
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
  
}

## To be copied in the UI
# mod_07_take_up_scatter_graph_ui("07_take_up_scatter_graph_1")

## To be copied in the server
# callModule(mod_07_take_up_scatter_graph_server, "07_take_up_scatter_graph_1")