#' slider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_ui <- function(
  id, 
  label="Financial Year:", 
  min="2015/16", 
  max="2019/20", 
  selected="2019/20",
  width="80%"
) {
  ns <- NS(id)
  
  years <- c(
    "2015/16", 
    "2016/17", 
    "2017/18", 
    "2018/19", 
    "2019/20"
  )
  
  stopifnot(min %in% years)
  stopifnot(max %in% years)
  stopifnot(selected %in% years)
  
  tagList(
    shiny::selectInput(
      inputId = ns("input"),
      label = label,
      choices = years[which(years == min):which(years == max)],
      selected = selected,
      width = width
    )
  )

}

#' slider Server Function
#'
#' @noRd
mod_input_server <- function(input, output, session) {

  ns <- session$ns
  
  return(input$input)
    
}

## To be copied in the UI
# mod_input_ui("input_1")

## To be copied in the server
# callModule(mod_input_server, "input_1")