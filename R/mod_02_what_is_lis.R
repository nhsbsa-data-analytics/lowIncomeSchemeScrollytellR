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
        target="_blank"
      ), 
      " (LIS) provides income related help to people who are not ", 
      actionLink(
        inputId = ns("modal"), 
        label = "already entitled to help with health costs"
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
      "Certificates are generally issued from periods of between six months ",
      "and five years."
    )
  )
}

#' 02_what_is_lis Server Function
#'
#' @noRd
mod_02_what_is_lis_server <- function(input, output, session) {
  ns <- session$ns
  
  # Create reactive eligibility hyperlink
  eligibility_click <- reactive({ input$modal })
  
  # Eligibility hyperlink modal
  observeEvent(
    eventExpr = eligibility_click(), 
    handlerExpr = {
      showModal(
        modalDialog(
          title = "Eligibility",
          includeMarkdown("inst/app/www/eligibility.md"),
          easyClose = TRUE
        )
      )
    }
  )
  
}

## To be copied in the UI
# mod_02_what_is_lis_ui("02_what_is_lis_1")

## To be copied in the server
# callModule(mod_02_what_is_lis_server, "02_what_is_lis_1")