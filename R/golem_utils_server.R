#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


#' Define tooltip text
#'
#' @noRd#'
tooltip_text <- list(
  # Already entitled to help with health cost
  who_can_apply = "<strong>Who can apply </strong> Anyone can apply as long as they do not have savings or investments over a certain limit. You cannot get help if you or your partner (or both) have more than: <br>
  * £16,000 in savings, investments or property (not including the place where you live) <br>
  * £23,250 in savings, investments or property if you live permanently in a care home (£24,000 if you live in Wales) <br><br>
  <strong>Who does not need to apply </strong> <br>
  You do not need to apply if you're already entitled to full help with health costs. <br>
  You already get full help with health costs if you or your partner get:<br><br>
  * Income Support <br>
  * income-based Jobseeker's Allowance <br>
  * income-related Employment and Support Allowance <br>
  * Pension Credit Guarantee Credit <br>
  * Universal Credit - if your earnings during your last complete assessment period were £435 or less, or £935 or less if you had a child element or had limited capability for work <br>
  You’re also entitled to full help if you are named on, or entitled to, an <a href = 'https://www.nhsbsa.nhs.uk/exemption-certificates/nhs-tax-credit-exemption-certificates' target = '_blank'> <span style = 'font-size: 15px'> NHS tax credit exemption certificate.</span></a><br>
  Any dependent children under 20 included on your benefit or tax credit claim are also entitled to the same help.",
  # Benefits / Others
  benefits_others = "<strong>Benefits & Others </strong> includes claimants of benefit such as Universal Credit and Employment and Support Allowance, people on nil income (e.g. supported by family and friends), people between courses or study, religious order occupants and care home residents.",
  # English IMD
  english_imd = "<strong>The Enlish indices of deprivation </strong> are official measures of relative deprivation for areas in England, ranking 32,844 areas in England according to their deprivation score and dividing them into 10 equal sized groups, or deciles. <br>
  Decile 1 represents the most deprived 10% of areas nationally and decile 10, the least deprived 10% of areas nationally.<br><br> The Index of Multiple Deprivation (IMD) is the most widely used of these measures and combines information from seven domains to produce an overall relative measure of deprivation. <br><br>
  One of the seven domains is Health Deprivation which is useful when looking at deprivation in a healthcare setting. <br><br> Further information can be found <a href = 'https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019' target = '_blank'> <span style = 'font-size: 15px'> here.</span> </a>"
)
