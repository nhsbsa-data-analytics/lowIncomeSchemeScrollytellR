#' nhs_animated_sliderInput Function
#'
#' @importFrom shiny tagList
nhs_animated_sliderInput <- function(inputId,
                                     label = "",
                                     choices,
                                     selected = NULL) {
  
  # Pull the play and pause icons
  play <- icon("play", style = "color: #768692; font-size:20px")
  pause <- icon("pause", style = "color: #768692; font-size:20px")
  
  # Remove the aria labels (for accessibility)
  play$attribs$`aria-label` <- NULL
  pause$attribs$`aria-label` <- NULL
  
  # Create animated sliderInput
  nhs_animated_sliderInput <- shinyWidgets::sliderTextInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    animate = animationOptions(
      playButton = play,
      pauseButton = pause,
      interval = 1500
    )
  )

  # Hack the CSS to look like an NHS sliderInput
  #nhs_animated_sliderInput$children[[1]]$attribs$class <- "nhsuk-label"

  # Put the play button and slider on the same line
  play_button <- nhs_animated_sliderInput$children[[3]]
  other <- nhs_animated_sliderInput$children[[2]]
  play_button$attribs$style <- "width:5%; margin-top:20px; float:left;"
  nhs_animated_sliderInput$children[[2]] <- play_button
  nhs_animated_sliderInput$children[[3]] <- other

  tagList(
    tags$style(
      # Make the line a constant NHS pale grey
      ".irs--round .irs-line { background-color: #E8EDEE; }",
      ".irs--round .irs-bar { background-color: #E8EDEE; }",
      # Make the slider part NHS dark grey
      ".irs--round .irs-handle { border: 4px solid #768692; box-shadow: 0px 0px 0px}",
      ".irs--round .irs-single { background-color: #768692; }",
      ".irs--round .irs-single:before { border-top-color: #768692; }",
      # Colour the min and max background NHS pale grey
      ".irs--round .irs-min, .irs--round .irs-max { background-color: #E8EDEE; }",
      # Put play and slider on the same line (for every slider)
      ".js-irs-0 { width:85%; float:right; }",
      ".js-irs-1 { width:85%; float:right; }",
      ".js-irs-2 { width:85%; float:right; }",
      ".js-irs-3 { width:85%; float:right; }",
      ".js-irs-4 { width:85%; float:right; }",
      ".js-irs-5 { width:85%; float:right; }"
    ),
    tags$div(
      shinyWidgets::chooseSliderSkin(skin = "Round"),
      nhs_animated_sliderInput
    )
  )
}
