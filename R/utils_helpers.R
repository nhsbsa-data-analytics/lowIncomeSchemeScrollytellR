#' NHSBSA highcharter theme
#'
#' Note: Adding the credits through the theme doesn't work so for now these
#' must be manually added.
#'
#' @param col_type
#'
#' @return
#' @export
theme_nhsbsa <- function(col_type = "normal", stacking = "normal") {

  # Define the colour palettes
  col_palettes <- list(
    "highlight" = c("#E8EDEE", "#003087"),
    "normal" = c(
      "#003087",
      "#ED8B00",
      "#41B6E6",
      "#00A499",
      "#FAE100",
      "#AE2573"
    )
  )

  # Check we have a valid colour type and allocate it
  stopifnot(col_type %in% names(col_palettes))
  col_palette <- col_palettes[[col_type]]

  # Return the theme
  highcharter::hc_theme(
    colors = col_palette,
    colAxis = list(
      min = 0,
      minColor = "#FFFFFF",
      maxColor = "#003087"
    ),
    plotOptions = list(
      series = list(stacking = stacking, borderWidth = 0),
      bar = list(groupPadding = 0.1)
    ),
    chart = list(
      backgroundColor = "#FFFFFF",
      style = list(
        fontFamily = "Frutiger W01",
        color = "#231f20"
      )
    ),
    title = list(
      style = list(
        fontFamily = "Frutiger W01 B"
      )
    ),
    subtitle = list(
      align = "left"
    ),
    legend = list(
      verticalAlign = "top",
      itemHoverStyle = list(
        color = "#768692"
      )
    ),
    labels = list(
      enabled = FALSE,
      style = list(
        fontFamily = "Frutiger W01 B"
      )
    ),
    xAxis = list(
      lineColor = "#768692",
      tickColor = "#768692",
      title = list(
        enabled = TRUE,
        style = list(
          fontFamily = "Frutiger W01 B"
        )
      )
    ),
    yAxis = list(
      lineColor = "#768692",
      tickColor = "#768692",
      gridLineWidth = 0,
      lineWidth = 1,
      title = list(
        enabled = TRUE,
        style = list(
          fontFamily = "Frutiger W01 B"
        )
      )
    ) # ,
    # credits = list(
    #  enabled = TRUE
    # )
  )
}


