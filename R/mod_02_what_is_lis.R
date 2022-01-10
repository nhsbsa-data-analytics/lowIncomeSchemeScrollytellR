#' 02_what_is_lis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_what_is_lis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("What is the NHS Low Income Scheme?"),
    p(
      "The ",
      a(
        "NHS Low Income Scheme",
        href = "https://www.nhsbsa.nhs.uk/nhs-low-income-scheme",
        target = "_blank"
      ),
      " (LIS) provides income related help to people who are not ",
      tippy(
        text = "already entitled to help with health costs(HwHC)",
        tooltip = tooltip_text$who_can_apply
      ),
      " if they have a low income. It is broadly the same as a means tested ",
      "benefit but also takes into account council tax and housing costs."
    ),
    p("The scheme helps towards health costs related to:"),
    tags$ul(
      tags$li("NHS prescription charges"),
      tags$li("NHS dental treatment charges"),
      tags$li("The cost of sight tests, glasses and contact lenses"),
      tags$li("The cost of travelling to receive NHS treatment"),
      tags$li("NHS wigs and fabric supports")
    ),
    p(
      "Certificates are generally issued from periods of between 6 months ",
      "and 5 years."
    )
  )
}

#' 02_what_is_lis Server Functions
#'
#' @noRd
mod_02_what_is_lis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_02_what_is_lis_ui("02_what_is_lis_ui_1")

## To be copied in the server
# mod_02_what_is_lis_server("02_what_is_lis_ui_1")
