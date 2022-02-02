#' selectInput Function
#'
#' @importFrom shiny tagList
nhs_selectInput <- function(
  inputId,
  label,
  choices,
  selected = NULL,
  multiple = FALSE,
  width = NULL,
  size = NULL
) {

  # Create select input
  nhs_selectInput <- shiny::selectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    selectize = FALSE,
    width = width,
    size = size
  )
  
  # Hack the CSS to look like an NHS select input
  nhs_selectInput$attribs$class <- "nhsuk-form-group"
  nhs_selectInput$children[[1]]$attribs$class <- "nhsuk-label"
  nhs_selectInput$children[[2]]$children[[1]]$attribs$class <- "nhsuk-select"
    
  tagList(
    nhs_selectInput
  )
}
