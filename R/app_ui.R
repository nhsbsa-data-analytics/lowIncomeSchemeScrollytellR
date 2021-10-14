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

    # First level UI elements
    mod_00_header_ui("00_header_1"),
    br(),
    fluidPage(
      mod_01_intro_ui("01_intro_1"),
      scrollytell::scrolly_container(
        outputId = "scrolly_2",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "02_what_is_lis",
            mod_02_what_is_lis_ui("02_what_is_lis_1")
          )
        )
      ),

      # who applies to LIS
      scrollytell::scrolly_container(
        outputId = "scrolly_3",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "03_who_applies_to_lis",
            mod_03_who_applies_to_lis_ui("03_who_applies_to_lis_1")
          )
        )
      ),

      # Application over time
      scrollytell::scrolly_container(
        outputId = "scrolly_4",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "04_applications_over_time",
            mod_04_applications_over_time_ui("04_applications_over_time_1")
          )
        )
      ),

      # What help does LIS provide
      scrollytell::scrolly_container(
        outputId = "scrolly_5",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "05_what_help_does_lis_provide",
            mod_05_what_help_does_lis_provide_ui("05_what_help_does_lis_provide_1")
          )
        )
      ),

      # Regional level take-up map
      scrollytell::scrolly_container(
        outputId = "scrolly_6",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "06_take_up_map",
            mod_06_take_up_map_ui("06_take_up_map_1")
          )
        )
      ),

      # Take-up LA - scatterplot text on the top
      # Scatterplot chart and zoomed LA map on the bottom

      scrollytell::scrolly_container(
        outputId = "scrolly_7",
        scrollytell::scrolly_graph(
          mod_input_region_ui("input_region_1", max = "North East"),
          fluidRow(
            column(
              width = 12,
              id = "07_take_up_scatter_graph",
              mod_07_take_up_scatter_graph_ui("07_take_up_scatter_graph_1")
            )
          )
        ),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy",
            br(),
            br()
          ),
          scrollytell::scrolly_section(
            # Text changes with more flexible contents
            id = "07_take_up_scatter_text",
            mod_07_take_up_scatter_text_ui("07_take_up_scatter_text_1")
          )
        )
      ),

      # student spotlight
      scrollytell::scrolly_container(
        outputId = "scrolly_8",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "08_spotlight_students",
            mod_08_spotlight_students_ui("08_spotlight_students_1")
          )
        )
      ),

      # final thoughts
      scrollytell::scrolly_container(
        outputId = "scrolly_9",
        scrollytell::scrolly_graph(),
        scrollytell::scrolly_sections(
          scrollytell::scrolly_section(
            id = "dummy"
          ),
          scrollytell::scrolly_section(
            id = "09_final_thoughts",
            mod_09_final_thoughts_ui("09_final_thoughts_1")
          )
        )
      )
    ),
    mod_10_footer_ui("10_footer_1")
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
