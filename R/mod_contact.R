#' contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Contact us"),
    p(
      "If you have any feedback, questions or comments regarding this project please contact",
      enurl(
        text = "nhsbsa.dall@nhs.net",
        url = "mailto:nhsbsa.dall@nhs.net"
      )
    ),
    br(),
    p(
      enurl(
        text = "Check if you're able to get help to pay NHS charges",
        url = "https://www.nhsbsa.nhs.uk/check-if-youre-eligible-help"
      )
    ),
    p(
      enurl(
        text = "Apply online for the NHS Low Income Scheme",
        url = "https://services.nhsbsa.nhs.uk/apply-for-help-with-nhs-costs/apply-online"
      )
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
}

#' contact Server Functions
#'
#' @noRd
mod_contact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_contact_ui("contact_ui_1")

## To be copied in the server
# mod_contact_server("contact_ui_1")
