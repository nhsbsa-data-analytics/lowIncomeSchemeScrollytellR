#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Application server logic
  mod_00_header_server("00_header_ui_1")
  mod_01_intro_server("01_intro_ui_1")
  mod_02_what_is_lis_server("02_what_is_lis_ui_1")
  mod_03_who_applies_to_lis_server("03_who_applies_to_lis_ui_1")
  mod_04_applications_over_time_server("04_applications_over_time_ui_1")
  mod_05_what_help_does_lis_provide_server("05_what_help_does_lis_provide_ui_1")
  mod_06_take_up_region_server("06_take_up_region_ui_1")
  mod_07_take_up_la_server("07_take_up_la_ui_1")
  mod_08_spotlight_students_server("08_spotlight_students_ui_1")
  mod_09_final_thoughts_server("09_final_thoughts_ui_1")
  mod_10_footer_server("10_footer_ui_1")

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
