#' 10_footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_10_footer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$footer(
      role = "contentinfo",
      tags$div(
        class = "nhsuk-footer",
        id = "nhsuk-footer",
        tags$div(
          class = "nhsuk-width-container app-width-container",
          tags$ul(
            class = "nhsuk-footer__list",
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "#",
                target = "_blank",
                "Accessibility statement"
              )
            ),
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "mailto:nhsbsa.dall@nhs.net",
                target = "_blank",
                "Contact us"
              )
            ),
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "https://github.com/nhsbsa-data-analytics/lowIncomeSchemeScrollytellR",
                target = "_blank",
                "GitHub"
              )
            )
          ),
          p(
            class = "nhsuk-footer__copyright",
            "© APLv2"
          )
        )
      )
    )
  )
}

#' 10_footer Server Functions
#'
#' @noRd
mod_10_footer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_10_footer_ui("10_footer_ui_1")

## To be copied in the server
# mod_10_footer_server("10_footer_ui_1")
