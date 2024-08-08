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
    # Need this for accessibility
    shiny::tags$html(lang = "en"),
    # Need this for shiny bootstrap dependencies TODO: check if needed
    shiny::bootstrapLib(),
    shiny::tags$a(id = "skiplink", "Skip to Main Content", href = "#maincontent"),
    # Need this for the play button
    fontawesome::fa_html_dependency(),
    # First level UI elements
    nhs_header(),
    br(),
    tags$div(
      class = "nhsuk-width-container",
      tags$div(
        class = "nhsuk-main-wrapper",
        id = "maincontent",
        role = "main",
        nhs_navlistPanel(
          well = FALSE,
          widths = c(2, 10),
          tabPanel(
            title = "Article",
            mod_01_intro_ui("01_intro_ui_1"),
            tags$div(style = "margin-top: 25vh"), # Some buffer space after the chart
            mod_02_what_is_lis_ui("02_what_is_lis_ui_1"),
            tags$div(style = "margin-top: 25vh"), # Some buffer space after the chart
            mod_03_who_applies_to_lis_ui("03_who_applies_to_lis_ui_1"),
            tags$div(style = "margin-top: 25vh"), 
            mod_04_applications_over_time_ui("04_applications_over_time_ui_1"),
            tags$div(style = "margin-top: 25vh"), 
            mod_05_what_help_does_lis_provide_ui("05_what_help_does_lis_provide_ui_1"),
            tags$div(style = "margin-top: 25vh"), 
            mod_06_take_up_region_ui("06_take_up_region_ui_1"),
            tags$div(style = "margin-top: 25vh"), 
            mod_07_take_up_la_ui("07_take_up_la_ui_1"),
            tags$div(style = "margin-top: 25vh"), 
            mod_08_spotlight_students_ui("08_spotlight_students_ui_1"),
            tags$div(style = "margin-top: 25vh"), 
            mod_09_final_thoughts_ui("09_final_thoughts_ui_1")
          ),
          tabPanel(
            title = "Definitions",
            tags$div(
              mod_definitions_ui("definitions_ui_1")
            )
          )
        )
      )
    ),
    br(),
    nhs_footer(),
    tags$script("
      $(document).ready(function () {
        $('#maincontent a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0);
        });
      });
    ")
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
      app_title = "Take-up of the NHS Low Income Scheme in England"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
