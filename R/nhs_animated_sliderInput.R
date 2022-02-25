#' nhs_animated_sliderInput Function
#'
#' @importFrom shiny tagList
nhs_animated_sliderInput <- function(inputId,
                                     label = "label",
                                     choices,
                                     selected = NULL,
                                     animate_interval = 1000) {

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
      interval = animate_interval
    ),
    hide_min_max = TRUE,
    width = "200px"
  )

  # Put the play button and slider on the same line
  play_button <- nhs_animated_sliderInput$children[[3]]
  other <- nhs_animated_sliderInput$children[[2]]
  play_button$attribs$style <- "width:5%; margin-top:27px; float:left;"
  play_button$attribs$roll <- "button"
  play_button$children[[1]]$attribs$`aria-label` <- "play-button" # accessibility
  other$attribs$`aria-label` <- "slider" # accessibility
  nhs_animated_sliderInput$children[[2]] <- play_button
  nhs_animated_sliderInput$children[[3]] <- other

  tagList(
    tags$style(
      # Make the line a constant NHS pale grey
      ".irs--round .irs-line { background-color: #E8EDEE; }",
      ".irs--round .irs-bar { background-color: #E8EDEE; }",
      # Make the slider part NHS dark grey
      ".irs--round .irs-handle { border: 4px solid #768692; box-shadow: 0px 0px 0px; }",
      # Make the text black
      ".irs--round .irs-single { background-color: transparent; color: #231f20; line-height: 1.5; }",
      # Put play and slider on the same line
      ".irs.irs--round { width: 85%; float: right; }",
      # Fix Empty form label, visibility:hidden removed Region label so had to change
      # font-size to zero.
      "label{ color: black; font-size: 0px; }",
      # Turn off opacity of play button
      ".slider-animate-button { opacity: 1; }"
    ),
    shinyWidgets::chooseSliderSkin(skin = "Round"),
    nhs_animated_sliderInput
  )
}
