#' 01_intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/app/www/mod_01_intro.md")
  )
}

#' 01_intro Server Functions
#'
#' @noRd
mod_01_intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_01_intro_ui("01_intro_ui_1")

## To be copied in the server
# mod_01_intro_server("01_intro_ui_1")
