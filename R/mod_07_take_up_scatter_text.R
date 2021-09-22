#' 07_take_up_scatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_take_up_scatter_text_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::htmlOutput(
      ns ("text")
    )
  )
}

#' 06_take_up Server Function
#'
#' @noRd
mod_07_take_up_scatter_text_server <- function(input, output, session, region_name) {
  ns <- session$ns
  
  # Pull the drop down value to get region name
  region_sel <- reactive({
    region_name$input
  })

  # Pull number of local authorities in the selected region.
  # get the highest take-up local authority, mention the deprivation rank
  # get the lowest take-up local authority, mention the deprivation rank 
  
  
  # Calculate %s
  plot_df <- reactive({nhslowincomeschemescrollytell::adult_population_df %>%
    dplyr::inner_join(nhslowincomeschemescrollytell::successful_individuals_by_la_df) %>%
    dplyr::mutate(
      p = TOTAL_SUCCESSFUL_INDIVIDUALS / TOTAL_ADULT_POPULATION * 1000
    ) %>%
    dplyr::filter(PCD_REGION_NAME == region_sel())
    })
  
  la_count <- reactive({
    plot_df() %>%
    dplyr::distinct(PCD_LAD_NAME) %>%   
    dplyr::count()
  })
  
  # which is the highest take-up local authority in the selected region.
  highest_take_up_la <- reactive({
    plot_df() %>% 
      dplyr::filter(p == max(p)) %>% 
      dplyr::select(PCD_LAD_NAME, PCD_LAD_IMD_RANK,p)
  })
  
  # observe({
  #   print(paste(highest_take_up_la()[1], "&", highest_take_up_la()[2]))
  # })
  

  # which is the lowest take-up local authority in the selected region.
  lowest_take_up_la <- reactive({
    plot_df() %>% 
      dplyr::filter(p == min(p)) %>% 
      dplyr::select(PCD_LAD_NAME, PCD_LAD_IMD_RANK, p)
  })
  
  # observe({
  #   print(paste(lowest_take_up_la()[1], "&", lowest_take_up_la()[2]))
  # })
  
  
  # Take-up of selected region 
  output$text = shiny::renderUI({
    shiny::HTML(paste("<p>", "There are", "<b>", la_count(), " </b> local authorities in",
                      "<b>", region_sel(), ".</b>",
                      "Of the",  "<b>", la_count(), " </b> local authorities,", 
                      "<b>", highest_take_up_la()[1], "</b> is the highest take-up per ",
                      "thousand of the general population.", 
                      "Of the 314 local authorities in England,", "IMD rank in", highest_take_up_la()[1], "is",
                      "<b>", highest_take_up_la()[2], "</b>.",
                      "<b>", lowest_take_up_la()[1], " </b> is the lowest take-up.",
                      "Of the 314 local authorities in England,", "IMD rank in", lowest_take_up_la()[1], "is",
                      "<b>", lowest_take_up_la()[2], "</b>."))
    
  })
  
}

## To be copied in the UI
# mod_07_take_up_scatter_text_ui("07_take_up_scatter_text_1")

## To be copied in the server
# callModule(mod_07_take_up_scatter_text_server, "07_take_up_scatter_text_1")