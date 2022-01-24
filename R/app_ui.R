#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$html(class = "no-js", lang = "en"),
    # HTML('<label for="play-range" style = "visibility: hidden;">year-range</label>'),
    # First level UI elements
    mod_00_header_ui("00_header_ui_1"),
    br(),
    navlistPanel(
      well = FALSE,
      widths = c(2, 12),
      tabPanel(
        tags$style(
          "li a {
          font-size: 20px;
          font-weight: bold;
          }"
        ),
        title = "Article",
        fluidPage(
          mod_01_intro_ui("01_intro_ui_1"),
          scrollytell::scrolly_container(
            outputId = "scrolly",
            scrollytell::scrolly_graph(),
            scrollytell::scrolly_sections(
              scrollytell::scrolly_section(
                id = "dummy"
              ),
              br(),
              scrollytell::scrolly_section(
                id = "02_what_is_lis",
                mod_02_what_is_lis_ui("02_what_is_lis_ui_1")
              ),
              br(),
              scrollytell::scrolly_section(
                id = "03_who_applies_to_lis",
                mod_03_who_applies_to_lis_ui("03_who_applies_to_lis_ui_1")
              ),
              br(),
              scrollytell::scrolly_section(
                id = "04_applications_over_time",
                mod_04_applications_over_time_ui("04_applications_over_time_ui_1")
              ),
              br(),
              scrollytell::scrolly_section(
                id = "05_what_help_does_lis_provide",
                mod_05_what_help_does_lis_provide_ui("05_what_help_does_lis_provide_ui_1")
              ),
              br(),
              scrollytell::scrolly_section(
                id = "06_take_up_region",
                mod_06_take_up_region_ui("06_take_up_region_ui_1")
              ),
              scrollytell::scrolly_section(
                id = "07_take_up_la",
                mod_07_take_up_la_ui("07_take_up_la_ui_1")
              ),
              scrollytell::scrolly_section(
                id = "08_spotlight_students",
                mod_08_spotlight_students_ui("08_spotlight_students_ui_1")
              ),
              scrollytell::scrolly_section(
                id = "09_final_thoughts",
                mod_09_final_thoughts_ui("09_final_thoughts_ui_1")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Definitions",
        fluidPage(
          br(),
          mod_definitions_ui("definitions_ui_1")
        )
      ),
      tabPanel(
        title = "Contact us",
        fluidPage(
          br(),
          mod_contact_ui("contact_ui_1")
        )
      ),
      tabPanel(
        title = "Accessibility statement",
        fluidPage(
          br(),
          mod_accessibility_ui("accessibility_ui_1")
        )
      )
    ),
    br(),
    mod_10_footer_ui("10_footer_ui_1")
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "int-602-low-income-scheme-ucd"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
