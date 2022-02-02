#' navlistPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_navlistPanel_ui <- function(
  ..., 
  id = NULL, 
  selected = NULL, 
  header = NULL,
  footer = NULL,
  fluid = TRUE, 
  widths = c(4, 8)
) {
  ns <- NS(id)
  
  # Create navlist panel
  custom_navlistPanel <- shiny::navlistPanel(
    ...,
    id = id,
    selected = selected,
    header = header,
    footer = footer,
    fluid = fluid,
    widths = widths
  )
  
  # Hack the CSS to look like an NHS list
  custom_navlistPanel$children[[1]]$children[[1]]$attribs$class <- "nhsuk-list app-side-nav__list"
  
  tagList(
    custom_navlistPanel
  )
}

#' navlistPanel Server Functions
#'
#' @noRd
mod_navlistPanel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_navlistPanel_ui("navlistPanel_ui_1")

## To be copied in the server
# mod_navlistPanel_server("navlistPanel_ui_1")
