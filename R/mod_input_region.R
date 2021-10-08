#' slider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_region_ui <- function(id,
                                label = "Region:",
                                min = "East Midlands",
                                max = "Yorkshire and The Humber",
                                selected = "North East",
                                width = "40%") {
  ns <- NS(id)

  regions <- c(
    "East Midlands",
    "East of England",
    "London",
    "North East",
    "North West",
    "South East",
    "South West",
    "West Midlands",
    "Yorkshire and The Humber"
  )

  stopifnot(min %in% regions)
  stopifnot(max %in% regions)
  stopifnot(selected %in% regions)

  tagList(
    shiny::selectInput(
      inputId = ns("input"),
      label = label,
      choices = regions, # [which(regions == min):which(regions == max)],
      selected = selected,
      width = width
    )
  )
}

#' slider Server Function
#'
#' @noRd
mod_input_region_server <- function(input, output, session) {
  ns <- session$ns

  vals <- reactiveValues()

  observe({
    vals$input <- input$input
  }) # taking reactive values and store them to use sticky chart part.

  return(vals)
}

## To be copied in the UI
# mod_slider_ui("slider_1")

## To be copied in the server
# callModule(mod_slider_server, "slider_1")
