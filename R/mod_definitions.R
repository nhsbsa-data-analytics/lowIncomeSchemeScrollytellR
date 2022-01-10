#' definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_definitions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # includeMarkdown("inst/app/www/mod_definitions.md") # didn't render nicely..
    h4("Definitions"),
    br(),
    h6("The NHS Low Income Scheme"),
    p(
      tags$b("The NHS Low Income Scheme"),
      "provides income related help to people who are not already entitled ",
      "to help with health cost if they have a low income.",
      "It is broadly the same as a means tested ",
      "benefit but also takes into account council tax and housing costs."
    ),
    br(),
    h6("Who can apply?"),
    p(
      "Anyone can apply as long as they do not have savings ",
      "or investments over a certain limit.",
      "You cannot get help if you or your partner (or both) have more than:",
      tags$li(
        "£16,000 in savings, investments or property ",
        "(not including the place where you live)"
      ),
      tags$li(
        "£23,250 in savings, investments or property if you live permanently in ",
        "a care home (£24,000 if you live in Wales)"
      )
    ),
    br(),
    h6("Who does not need to apply?"),
    p(
      "You do not need to apply if you're already entitled to full help with health costs.",
      "You already get full help with health costs if you or your partner get:",
      tags$li("Income Support"),
      tags$li("Income-based Jobseeker's Allowance"),
      tags$li("Income-related Employment and Support Allowance"),
      tags$li("Pension Credit Guarantee Credit"),
      tags$li(
        "Universal Credit - if your earnings during your last complete ",
        "assessment period were £435 or less, ",
        "or £935 or less if you had a child element or had limited capability for work"
      )
    ),
    p(
      "You're also entitled to full help if you are named on, ",
      "or entitled to, an NHS tax credit exemption certificate.",
      "Any dependent children under 20 included on your benefit or ",
      "tax credit claim are also entitled to the same help."
    ),
    br(),
    h6("The English Indices of Deprivation"),
    p(
      tags$b("The English Indices of Deprivation"),
      "are official measures of relative deprivation for areas in England, ",
      "ranking 32,844 areas in England according to their deprivation score ",
      "and dividing them into 10 equal sized groups, or deciles."
    ),
    p(
      "Decile 1 represents the most deprived 10% of areas nationally and decile 10, ",
      "the least deprived 10% of areas nationally."
    ),
    p(
      "The Index of Multiple Deprivation (IMD) is the most widely used of these ",
      "measures and combines information from seven domains to produce an overall ",
      "relative measure of deprivation. ",
      "One of the seven domains is Health Deprivation which is useful ",
      "when looking at deprivation in a healthcare setting.",
      "Further information can be found",
      a(
        "here.",
        href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
        target = "_blank"
      ),
    )
  )
}

#' definitions Server Functions
#'
#' @noRd
mod_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_definitions_ui("definitions_ui_1")

## To be copied in the server
# mod_definitions_server("definitions_ui_1")
