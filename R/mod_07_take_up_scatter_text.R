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
  # This one is the one to keep regional information for dynamic text
  
  region_sel <- reactive({
    region_name$input
  })


  
  # Just keep the region name and the rest of element is done prior to the text rendering. 
  
  
  # Pull number of local authorities in the selected region.
  # get the highest take-up local authority, mention the deprivation rank
  # get the lowest take-up local authority, mention the deprivation rank 
  
  
  # Calculate %s
  plot_df <- reactive({lowIncomeSchemeScrollytellR::adult_population_df %>%
    dplyr::inner_join(lowIncomeSchemeScrollytellR::successful_individuals_by_la_df) %>%
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
  # this part will change depends on the region selection from the reactive value. 
  output$text = shiny::renderUI({
    if (region_sel() == 'North West'){
      shiny::HTML(paste("<p>", "There are", "<b>", la_count(), " </b> local authorities in the",
                        "<b>", region_sel(), "</b>.",
                        "<b>", highest_take_up_la()[1], "</b> has the highest take-up per ",
                        "thousand of the general population.", 
                        "Of the 314 local authorities in England,", "the IMD rank in", highest_take_up_la()[1], "is",
                        "<b>", highest_take_up_la()[2], "</b>.",
                        "<b>", lowest_take_up_la()[1], " </b> has the lowest take-up in the", region_sel(), 
                        ", with an IMD rank of", "<b>", lowest_take_up_la()[2], " </b> out of the 314 local authorities in England.",
                        "Take-up is somewhat", "<b> lower, relative to deprivation </b>", "in five local authorities in the", "<b> North East </b>",
                        "<b> (Knowsley, Hyndburn, Halton, St Helens and Barrow in Furness and North East Lincolnshire Council."))
                        
    }else if(region_sel() == 'North East'){
      shiny::HTML(paste("<p>", "There are", "<b>", la_count(), " </b> local authorities in the",
                        "<b>", region_sel(), "</b>.",
                        "Of the",  "<b>", la_count(), " </b> local authorities,", 
                        "<b>", highest_take_up_la()[1], "</b> has the highest take-up per ",
                        "thousand of the general population.", 
                        "Of the 314 local authorities in England,", "the IMD rank in", highest_take_up_la()[1], "is",
                        "<b>", highest_take_up_la()[2], "</b>.",
                        "<b>", lowest_take_up_la()[1], " </b> has the lowest take-up in the", region_sel(), 
                        ", with an IMD rank of", "<b>", lowest_take_up_la()[2], " </b> out of the 314 local authorities in England."))
    }else{
      shiny::HTML(paste("<p>", "There are", "<b>", la_count(), " </b> local authorities in the",
                        "<b>", region_sel(), "</b>.",
                        "Of the",  "<b>", la_count(), " </b> local authorities,", 
                        "<b>", highest_take_up_la()[1], "</b> has the highest take-up per ",
                        "thousand of the general population.", 
                        "Of the 314 local authorities in England,", "the IMD rank in", highest_take_up_la()[1], "is",
                        "<b>", highest_take_up_la()[2], "</b>.",
                        "<b>", lowest_take_up_la()[1], " </b> has the lowest take-up in the", region_sel(), 
                        ", with an IMD rank of", "<b>", lowest_take_up_la()[2], " </b> out of the 314 local authorities in England."))
    }
    
   
    
  })
  
}

## To be copied in the UI
# mod_07_take_up_scatter_text_ui("07_take_up_scatter_text_1")

## To be copied in the server
# callModule(mod_07_take_up_scatter_text_server, "07_take_up_scatter_text_1")