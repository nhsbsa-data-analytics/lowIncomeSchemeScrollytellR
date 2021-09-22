#' 03_who_applies_to_lis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_who_applies_to_lis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Who applies to the NHS Low Income Scheme in England?"),
    fluidRow(
      p(
        "Applications either relate to a single applicant or couple. If we ",
        "consider individuals covered by applications, around 7 in 10 are ",
        "single applicants and the remainder are part of a couple. We only ",
        "hold age for the main applicant."
      ),
      tags$ul(
        tags$li("One in four main applicants are aged 15 to 24 years")
      ),
      fluidRow(
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_individuals_by_age_band"),
          height = "500px"
        )
      ),
      br(),
      p(
        "Applicants are allocated a client group based on the main source ",
        "of household income. They are most likely to be categorised as ",
        actionLink(
          inputId = ns("benefits_or_other_modal"), 
          label = "benefits/other"
        ), 
        " or earners."
      ),
      fluidRow(
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(
          outputId = ns("plot_individuals_by_client_group")
        )
      ),
      br(),
      p(
        "Individuals covered by the application are more likely to live in ",
        "areas of higher deprivation, as we can see in the below ",
        actionLink(
          inputId = ns("imd_modal"), 
          label = "English indices of deprivation"
        ),
        " decile chart."
      ),
      fluidRow(
        align = "center",
        style = "background-color: #FFFFFF;",
        highcharter::highchartOutput(outputId = ns("plot_individuals_by_deprivation"))
      )
    )
  )
}

#' 03_who_applies_to_lis Server Function
#'
#' @noRd
mod_03_who_applies_to_lis_server <- function(input, output, session) {
  ns <- session$ns
  
  # Create reactive benefits / other hyperlink
  benefits_or_other_click <- reactive({ input$benefits_or_other_modal })
  
  # Benefits / other hyperlink modal
  observeEvent(
    eventExpr = benefits_or_other_click(), 
    handlerExpr = {
      showModal(
        modalDialog(
          title = "Benefits/Other",
          p(
            "Benefits & Other includes claimants of benefit such as Universal ",
            "Credit and Employment and Support Allowance, people on nil ",
            "income (e.g. supported by family and friends), people between ",
            "courses of study, religious order occupants and care home ",
            "residents."
          ),
          easyClose = TRUE
        )
      )
    }
  )
  
  # Create IMD hyperlink
  imd_click <- reactive({ input$imd_modal })
  
  # IMD hyperlink modal
  observeEvent(
    eventExpr = imd_click(), 
    handlerExpr = {
      showModal(
        modalDialog(
          title = "English indices of deprivation",
          p(
            "The Enlish indices of deprivation are official measures of ",
            "relative deprivation for areas in England, ranking 32,844 areas ",
            "in England according to their deprivation score and dividing ",
            "them into 10 equal sized groups, or deciles. Decile 1 represents ",
            "the most deprived 10% of areas nationally and decile 10, the ",
            "least deprived 10% of areas nationally."
          ),
          p(
            "The Index of Multiple Deprivation (IMD) is the most widely used ",
            "of these measures and combines information from seven domains to ",
            "produce an overall relative measure of deprivation."
          ),
          p(
            "One of the seven domains is Health Deprivation which is useful ",
            "when looking at deprivation in a healthcare setting."
          ),
          p(
            "Further information can be found ",
            a(
              "here.", 
              href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
              target="_blank"
            )
          ),
          easyClose = TRUE
        )
      )
    }
  )
  
  # Pyramid plot for age band
  output$plot_individuals_by_age_band <- highcharter::renderHighchart({
    
    # Filter out Co-applicants and Unknowns, calculate %s
    plot_df <- lowincomeschemeucd::individuals_by_age_band_df %>%
      dplyr::filter(
        !(BAND_5YEARS %in% c("Co-applicant", "Unknown")) 
      ) %>%
      dplyr::group_by(FINANCIAL_YEAR) %>% 
      dplyr::mutate(p = TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100) %>%
      dplyr::ungroup() 
    
    # Pull the max p
    max_p <- max(abs(plot_df$p))
    
    # Format for highcharter animation
    plot_series_list <- plot_df %>%
      tidyr::expand(FINANCIAL_YEAR, BAND_5YEARS) %>%
      dplyr::left_join(plot_df) %>%
      dplyr::mutate(p = tidyr::replace_na(p)) %>%
      dplyr::group_by(BAND_5YEARS) %>%
      dplyr::do(data = list(sequence = .$p)) %>%
      dplyr::ungroup() %>%
      # dplyr::group_by(FINANCIAL_YEAR) %>%
      dplyr::do(data = .$data) %>%
      dplyr::mutate(name = "Age Band (5 Year)") %>%
      highcharter::list_parse()
    
    # Create plot
    highcharter::highchart() %>%
      highcharter::hc_chart(type = "column", marginBottom = 120) %>%
      highcharter::hc_add_series_list(x = plot_series_list) %>%
      highcharter::hc_motion(
        labels = unique(plot_df$FINANCIAL_YEAR),
        series = c(0, 1),
        startIndex = 4
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa(stacking = NA)) %>%
      highcharter::hc_title(
        text = "Age band of NHS Low Income Scheme lead applicants in England (2015/16 to 2020/21)"
      ) %>%
        highcharter::hc_subtitle(
          text = "Note: This excludes main applicants without an age band."
      ) %>%
      highcharter::hc_xAxis(
        categories = sort(unique(plot_df$BAND_5YEARS)),
        reversed = FALSE
      ) %>% 
      highcharter::hc_yAxis(
        max = ceiling(max_p / 5) * 5,
        labels = list(
          formatter = highcharter::JS("function(){ return Math.abs(this.value) + '%' ;}") 
        )
      ) %>% 
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Age band (5 years): </b>' + this.point.category + '<br/>' + '<b>Percentage: </b>' + Math.abs(Math.round(this.point.y * 10) / 10) + '%';}")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
    
  })
  
  # Stacked column plot by client group
  output$plot_individuals_by_client_group <- highcharter::renderHighchart({

    # Filter out Co-applicants and Unknowns then calculate %s
    plot_df <- lowincomeschemeucd::individuals_by_client_group_df %>%
      dplyr::filter(
        !(CLIENTGROUP_DESC_FORMAT %in% c("Co-applicant", "Unknown"))
      ) %>%
      dplyr::group_by(FINANCIAL_YEAR) %>% 
      dplyr::mutate(p = TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100) %>%
      dplyr::ungroup()
    
    # Create plot
    plot_df %>%
      highcharter::hchart(
        type = "column",
        highcharter::hcaes(x = FINANCIAL_YEAR, y = p, group = CLIENTGROUP_DESC_FORMAT)
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa()) %>%
      highcharter::hc_title(
        text = "Client group of NHS Low Income Scheme applications in England (2015/16 to 2020/21)"
      ) %>%
      highcharter::hc_subtitle(
        text = "Note: This excludes applications with an unknown client group."
      ) %>%
      highcharter::hc_yAxis(
        max = 100,
        labels = list(
          formatter = highcharter::JS("function(){ return this.value + '%' ;}") 
        )      
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Client Group: </b>' + this.series.name + '<br>' + '<b>Percentage: </b>' + Math.round(this.point.y * 10) / 10  + '%';}")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )

  })
  
  # Column plot by deprivation
  output$plot_individuals_by_deprivation <- highcharter::renderHighchart({
    
    # Calculate %s
    plot_df <- lowincomeschemeucd::individuals_by_imd_df %>%
      dplyr::mutate(DEPRIVATION = "Index of Multiple Deprivation") %>%
      dplyr::rename(DECILE = INDEX_OF_MULT_DEPRIV_DECILE) %>%
      rbind(
        lowincomeschemeucd::individuals_by_health_df %>%
          dplyr::mutate(DEPRIVATION = "Health Deprivation") %>%
          dplyr::rename(DECILE = HEALTH_DEPRIVATION_DECILE)
      ) %>%
      dplyr::group_by(FINANCIAL_YEAR, DEPRIVATION) %>%
      dplyr::mutate(p = TOTAL_INDIVIDUALS / sum(TOTAL_INDIVIDUALS) * 100) %>%
      dplyr::ungroup()
    
    # Pull the max p
    max_p <- max(abs(plot_df$p))
    
    # Format for highcharter animation
    plot_series_list <- plot_df %>%
      tidyr::expand(FINANCIAL_YEAR, DECILE, DEPRIVATION) %>%
      dplyr::left_join(plot_df) %>%
      dplyr::mutate(p = tidyr::replace_na(p)) %>%
      dplyr::group_by(DECILE, DEPRIVATION) %>%
      dplyr::do(data = list(sequence = .$p)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(DEPRIVATION) %>%
      dplyr::do(data = .$data) %>%
      dplyr::mutate(name = DEPRIVATION) %>%
      highcharter::list_parse()
    
    # Create plot    
    highcharter::highchart() %>%
      highcharter::hc_chart(type = "column", marginBottom = 120) %>%
      highcharter::hc_add_series_list(x = plot_series_list) %>% 
      highcharter::hc_motion(
        labels = unique(plot_df$FINANCIAL_YEAR),
        series = c(0, 1),
        startIndex = 4
      ) %>%
      highcharter::hc_add_theme(hc_thm = theme_nhsbsa(stacking = NA)) %>%
      highcharter::hc_title(
        text = "Deprivation decile of NHS Low Income Scheme individuals in England (2015/16 to 2020/21)"
      ) %>%
      highcharter::hc_xAxis(
        categories = c("1<br>Most<br>deprived", 2:9, "10<br>Least<br>deprived")
      ) %>%
      highcharter::hc_yAxis(
        max = ceiling(max(plot_df$p) / 5) * 5,
        labels = list(
          formatter = highcharter::JS("function(){ return this.value + '%' ;}")
        )
      ) %>%
      highcharter::hc_tooltip(
        shared = FALSE,
        formatter = highcharter::JS("function () { return '<b>Deprivation: </b>' + this.series.name + '<br>' + '<b>Decile: </b>' + parseInt(this.point.category) + '<br>' + '<b>Percentage: </b>' + Math.round(this.point.y * 10) / 10  + '%';}")
      ) %>%
      highcharter::hc_credits(
        enabled = TRUE
      )
    
  })
  
}

## To be copied in the UI
# mod_03_who_applies_to_lis_ui("03_who_applies_to_lis_1")

## To be copied in the server
# callModule(mod_03_who_applies_to_lis_server, "03_who_applies_to_lis_1")
