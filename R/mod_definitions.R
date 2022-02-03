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
    h4("Definitions"),
    h6("The NHS Low Income Scheme"),
    p(
      tags$b("The NHS Low Income Scheme"),
      "provides income related help to people who are not already entitled ",
      "to help with health cost if they have a low income.",
      "It is broadly the same as a means tested ",
      "benefit but also takes into account council tax and housing costs."
    ),
    h6("Who can apply?"),
    p(
      "Anyone can apply as long as they do not have savings ",
      "or investments over a certain limit.",
      "You cannot get help if you or your partner (or both) have more than:",
      tags$ul(
        tags$li(
          "£16,000 in savings, investments or property ",
          "(not including the place where you live)"
        ),
        tags$li(
          "£23,250 in savings, investments or property if you live permanently ",
          "in a care home (£24,000 if you live in Wales)"
        )
      )
    ),
    h6("Who does not need to apply?"),
    p(
      "You do not need to apply if you're already entitled to full help with ",
      "health costs. You already get full help with health costs if you or ",
      "your partner get:",
      tags$ul(
        tags$li("Income Support"),
        tags$li("Income-based Jobseeker's Allowance"),
        tags$li("Income-related Employment and Support Allowance"),
        tags$li("Pension Credit Guarantee Credit"),
        tags$li(
          "Universal Credit - if your earnings during your last complete ",
          "assessment period were £435 or less, or £935 or less if you had a ",
          "child element or had limited capability for work"
        )
      )
    ),
    p(
      "You're also entitled to full help if you are named on, ",
      "or entitled to, an NHS tax credit exemption certificate.",
      "Any dependent children under 20 included on your benefit or ",
      "tax credit claim are also entitled to the same help."
    ),
    h6("Applications"),
    p(
      "Application data is taken from a snapshot copy of the NHSBSA’s ",
      "internal database for processing applications to the ",
      "NHS Low Income Scheme, this snapshot was taken on ",
      "1st December 2021 and includes applications processed from ",
      "April 2015 onwards."
    ),
    p(
      "Application data includes records where applicants have made ",
      "an application via an HC1 form, either by paper or through ",
      "the digital application process, although digital applications ",
      "were only introduced from November 2018 onwards and are still ",
      "being rolled out. "
    ),
    p(
      "Each application record includes the applicant’s name, address, ",
      "date of birth and details of the application outcome.  ",
      "Where a certificate has been issued or an application withdrawn, ",
      "this is captured within the data and used to identify the outcome.  ",
      "Where a certificate has not been issued and the application was ",
      "registered prior to 1st January 2021, it is assumed that the ",
      "application has been abandoned.  Applications registered from ",
      "2021 onwards, where no certificate has been issued or withdrawn, ",
      "are recorded as ongoing."
    ),
    p(
      "Applications have been assigned against a reporting period based on the ",
      "financial year in which the application was registered.",
      "Application data has been limited to where a valid UK postcode can be ",
      "associated with the application."
    ),
    p(
      "Each application is assigned a unique reference number at the point of ",
      "creation, and this reference number is used throughout the ",
      "lifecycle of the application, up to and including the production of ",
      "any certificate. In cases where an applicant may abandon an application ",
      "and submit a new application, this would result in a new unique ",
      "reference number being created. Application counts throughout ",
      "this report are based on a count of the unique reference numbers."
    ),
    h6("Applicants"),
    p(
      "NHS Low Income Scheme records only retain the latest applicant ",
      "information against any single case reference ID. "
    ),
    p(
      "Where an applicant showed potential duplicate applications, ",
      "for example two or more cases created on the same date, priority is ",
      "given based on the outcome of the application ",
      "(Full benefit > Partial benefit > No assistance > ",
      "Abandoned/Withdrawn > Ongoing)."
    ),
    p(
      "Applicant age has been defined ",
      "as the age at the point of registration based on the applicant’s ",
      "date of birth., If this resulted in an age calculated as either under ",
      "16 or over 100, the age is classified as unknown."
    ),
    p(
      "As no consistent unique applicant reference number exists within the ",
      "NHS Low Income Scheme system, individual applicants have been ",
      "identified based on a derived composite identifier based on a ",
      "combination of their personal information ",
      "(forename, surname, date of birth and postcode). ",
      "Where an application does not include any of the required personal ",
      "information, no composite identifier can be created and ",
      "therefore the applicant will not be included in any applicant counts.",
      "There are 0.15% of applicants without composite ID."
    ),
    h6("Individuals covered by the application"),
    p(
      "Applicants can apply either for themselves only or include dependants ",
      "within the same household. The database only includes details of a ",
      "partner as dependent (not children) and does not include any demographic ",
      "information about the partner. Therefore each certificate has only ",
      "been identified as covering one or two people. "
    ),
    h6("Client group"),
    p(
      "Applicants are allocated a client group by assessors based on the main ",
      "source of household income. This includes:",
      tags$ul(
        tags$li("Asylum Seeker (not from the National Asylum Support Service)"),
        tags$li(
          "Benefits & Others includes claimants of benefit such as Universal ",
          "Credit and Employment and Support Allowance, people in nil income ",
          "(e.g. supported by family and friends), people between courses or ",
          "study, religious order occupants and care home residents."
        ),
        tags$li("Earner"),
        tags$li("Pensioner"),
        tags$li("Student")
      )
    ),
    h6("The English Indices of Deprivation"),
    p(
      tags$b("The English Indices of Deprivation"),
      "are official measures of relative deprivation for areas in England, ",
      "ranking 32,844 areas in England according to their deprivation score ",
      "and dividing them into 10 equal sized groups, or deciles."
    ),
    p(
      "Decile 1 represents the most deprived 10% of areas nationally and ",
      "decile 10, the least deprived 10% of areas nationally."
    ),
    p(
      "The Index of Multiple Deprivation (IMD) is the most widely used of ",
      "these measures and combines information from seven domains to produce ",
      "an overall relative measure of deprivation. One of the seven domains ",
      "is Health Deprivation which is useful when looking at deprivation in a ",
      "healthcare setting. Further information can be found ",
      enurl(
        text = "here.",
        url = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019"
      )
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
