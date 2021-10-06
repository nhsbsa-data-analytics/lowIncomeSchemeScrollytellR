#' 09_final_thoughts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_09_final_thoughts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/app/www/mod_09_final_thoughts.md")
  )
}

#' 09_final_thoughts Server Function
#'
#' @noRd
mod_09_final_thoughts_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_09_final_thoughts_ui("09_final_thoughts_1")

## To be copied in the server
# callModule(mod_09_final_thoughts_server, "09_final_thoughts_1")
