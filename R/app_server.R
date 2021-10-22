#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Application server logic
  moduleServer("00_header_1", mod_00_header_server)
  moduleServer("01_intro_1", mod_01_intro_server)
  moduleServer("02_what_is_lis_1", mod_02_what_is_lis_server)
  moduleServer("03_who_applies_to_lis_1", mod_03_who_applies_to_lis_server)
  moduleServer("04_applications_over_time_1", mod_04_applications_over_time_server)
  moduleServer("05_what_help_does_lis_provide_1", mod_05_what_help_does_lis_provide_server)
  moduleServer("06_take_up_region_1", mod_06_take_up_region_server)
  moduleServer("07_take_up_la_1", mod_07_take_up_la_server)
  moduleServer("08_spotlight_students_1", mod_08_spotlight_students_server)
  moduleServer("09_final_thoughts_1", mod_09_final_thoughts_server)
  moduleServer("10_footer_1", mod_10_footer_server)

  output$scrolly <- scrollytell::renderScrollytell({
    scrollytell::scrollytell()
  })
}
