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
    fluidPage(
      fluidRow(
        column(
          width = 12,
          style = "background-color: #FFFFFF;",
        ),
        fluidRow(
          column(
            width = 4,
            p(
              "It is helpful to consider estimated take-up relative to deprivation.",
              "The chart and map show estimated take-up by local authority area relative ",
              "to the population and overall deprivation profile of an area."
            ),
            shiny::htmlOutput(
              ns("text")
            )
          ),
          column(
            width = 8,
            fluidRow(
              h6("Estimated take-up of NHS Low Income Scheme by Index of Multiple Deprivation for English Local Authorities (2015/16 to 2020/21)"),
              column(
                width = 6,
                shiny::selectInput(
                  inputId = ns("input_year"),
                  label = "Financial Year:",
                  choices = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21"),
                  selected = "2019/20"
                )
              ),
              column(
                width = 6,
                shiny::selectInput(
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
                  selected = "North West"
                )
              )
            ),
            highcharter::highchartOutput(
              outputId = ns("plot_selected_region_la"),
              height = "450px"
            ),
            highcharter::highchartOutput(
              outputId = ns("plot_successful_individuals_by_la_imd"),
              height = "450px"
            )
          )
        )
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



    output$plot_successful_individuals_by_la_imd <- highcharter::renderHighchart({

      # Calculate %s
      plot_df <- lowIncomeSchemeScrollytellR::adult_population_df %>%
        # dplyr::filter(FINANCIAL_YEAR == input$input_year) %>%
        dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_la_df) %>%
        dplyr::mutate(
          p = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
        ) # %>%
      # dplyr::mutate(color = ifelse(PCD_REGION_NAME == input$input_region, "#003087", "#DDE1E4"))

      # Format for highcharter animation
      # Removed as it confused with drop down menu (need to check though)
      plot_sequence_df <- plot_df %>%
        tidyr::expand(FINANCIAL_YEAR, tidyr::nesting(PCD_REGION_NAME, PCD_LAD_NAME, PCD_LAD_IMD_RANK)) %>%
        dplyr::left_join(plot_df) %>%
        dplyr::mutate(p = tidyr::replace_na(p)) %>%
        dplyr::group_by(PCD_REGION_NAME, PCD_LAD_NAME, PCD_LAD_IMD_RANK) %>%
        dplyr::do(sequence = .$p) %>%
        # Mutate to create color variable to reflect selected region with lightgrey and darkblue colour
        dplyr::mutate(color = ifelse(PCD_REGION_NAME == input$input_region, "#003087", "#DDE1E4"))

      # Create plot
      plot_sequence_df %>%
        highcharter::hchart(
          type = "scatter",
          highcharter::hcaes(x = PCD_LAD_IMD_RANK, y = sequence, color = color),
          marginBottom = 50
        ) %>%
        # Add two dummy series for the legend
        highcharter::hc_add_series(
          data = NULL,
          name = "Selected local authority",
          showInLegend = TRUE,
          color = "#003087"
        ) %>%
        highcharter::hc_add_series(
          data = NULL,
          name = "Other local authority",
          showInLegend = TRUE,
          color = "#DDE1E4"
        ) %>%
        highcharter::hc_motion(
          labels = unique(plot_df$FINANCIAL_YEAR),
          startIndex = 4
        ) %>%
        # highcharter::hc_title(
        theme_nhsbsa(stack = NA) %>%
        #   text = "Estimated take-up of NHS Low Income Scheme by IMD Rank for English Local Authorities (2015/16 to 2020/21)"
        # ) %>%
        # highcharter::hc_subtitle(
        #   text = "Note:  IMD rank is based on English indicies of deprivation 2019."
        # ) %>%
        highcharter::hc_xAxis(
          min = 1,
          max = 330, # Pad to ensure we can see the 314 label
          categories = c(NA, "1<br>Most deprived", rep(NA, 312), "314<br>Least deprived"),
          labels = list(step = 313),
          title = list(text = "Local authority deprivation rank (2019)")
        ) %>%
        highcharter::hc_yAxis(
          max = ceiling(max(plot_df$p) / 5) * 5,
          title = list(text = "Take-up per thousand of the population aged 16+ years")
        ) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>Local authority:</b> {point.PCD_LAD_NAME} <br><b>2019 IMD rank:</b> {point.x} <br><b>Take-up:</b> {point.y:.1f} (per thousand of the general population)"
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


      # la data frame, change to sequence data but only for selected region and year
      plot_df <- lowIncomeSchemeScrollytellR::adult_population_df %>%
        dplyr::filter(PCD_REGION_NAME == input$input_region) %>%
        dplyr::filter(FINANCIAL_YEAR == input$input_year) %>%
        dplyr::group_by(FINANCIAL_YEAR, PCD_LAD_NAME) %>%
        dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_la_df) %>%
        dplyr::mutate(
          value = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
        ) %>%
        dplyr::select(FINANCIAL_YEAR, PCD_LAD_NAME, value) %>%
        dplyr::inner_join(la_imd_count)

      # filter la_map as well

      la_map <- lowIncomeSchemeScrollytellR::la_map %>%
        dplyr::inner_join(lowIncomeSchemeScrollytellR::region_la_lookup) %>%
        dplyr::filter(PCD_REGION_NAME == input$input_region) %>%
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
        theme_nhsbsa() %>%
        # highcharter::hc_title(
        #   text = "Estimated take-up of NHS Low Income Scheme (2015/16 to 2019/20)"
        # ) %>%
        highcharter::hc_subtitle(
          text = "Note:  IMD rank is based on English indicies of deprivation 2019.",
          verticalAlign = "bottom"
        ) %>%
        highcharter::hc_colorAxis(min = 0, max = 20)
    })


    # Just keep the region name and the rest of element is done prior to the text rendering.


    # Pull number of local authorities in the selected region.
    # get the highest take-up local authority, mention the deprivation rank
    # get the lowest take-up local authority, mention the deprivation rank


    # Calculate %s
    plot_df <- reactive({
      lowIncomeSchemeScrollytellR::adult_population_df %>%
        dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_la_df) %>%
        dplyr::filter(FINANCIAL_YEAR == input$input_year) %>%
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

    # observe({
    #   print(paste(highest_take_up_la()[1], "&", highest_take_up_la()[2]))
    # })


    # which is the lowest take-up local authority in the selected region.
    lowest_take_up_la <- reactive({
      plot_df() %>%
        dplyr::filter(p == min(p)) %>%
        dplyr::select(PCD_LAD_NAME, PCD_LAD_IMD_RANK, p)
    })

    # observe({
    #   print(paste(lowest_take_up_la()[1], "&", lowest_take_up_la()[2]))
    # })




    # Take-up of selected region
    # this part will change depends on the region selection from the reactive value.
    output$text <- shiny::renderUI({
      if (input$input_region == "North West") {
        shiny::HTML(paste(
          "<br>",
          "<br>",
          "<p>", "In ", "<b>", input$input_year, "</b>, <b>", highest_take_up_la()[1], "</b>",
          " has the highest take-up per thousand of the general population in the ",
          "<b>", input$input_region, "</b>", ". Of all local authorities in England, the IMD rank in ", "<b>",
          highest_take_up_la()[1], " </b> is ", "<b>", highest_take_up_la()[2], ".</b>",
          "</p>", "<p>", "<b>", lowest_take_up_la()[1], " </b> has the lowest take-up in the ", " <b>", input$input_region, "</b>",
          ", with an IMD rank of ", " <b>", lowest_take_up_la()[2], " </b> out of all local authorities in England.",
          "<p>",
          "Take-up is somewhat", "<b> lower, relative to deprivation </b>", "in five local authorities in the", " <b> North West </b>",
          "<b> (Knowsley, Hyndburn, Halton, St Helens and Barrow in Furness.)", "</p>",
          sep = ""
        ))
      } else {
        shiny::HTML(paste(
          "<br>",
          "<br>",
          "<p>", "In ", "<b>", input$input_year, ", ", highest_take_up_la()[1], "</b>",
          " has the highest take-up per thousand of the general population in the ",
          "<b>", input$input_region, "</b>", ". Of all local authorities in England, the IMD rank in ", "<b>",
          highest_take_up_la()[1], " </b> is ", "<b>", highest_take_up_la()[2], ".</b>",
          "</p>", "<p>", "<b>", lowest_take_up_la()[1], " </b> has the lowest take-up in the ", " <b>", input$input_region, "</b>",
          ", with an IMD rank of ", " <b>", lowest_take_up_la()[2], " </b> out of all local authorities in England.",
          sep = ""
        ))
      }
    })
  })
}

## To be copied in the UI
# mod_07_take_up_la_ui("07_take_up_la_ui_1")

## To be copied in the server
# mod_07_take_up_la_server("07_take_up_la_ui_1")
