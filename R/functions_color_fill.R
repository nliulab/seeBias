#' Branded colour palette
#' @param primary Primary colour in palette.
#' @param other Secondary colour.
#' @param direction Direction of colour increase.
#' @references
#' \itemize{
#'  \item{https://www.garrickadenbuie.com/blog/custom-discrete-color-scales-for-ggplot2/}
#' }
branded_pal <- function(primary = "blue", other = "red", direction = 1) {
  branded_colors <- list(
    "blue" = "#00798c",
    "red" = "#d1495b",
    "yellow" = "#edae49",
    "green" = "#66a182",
    "navy" = "#2e4057",
    "grey" = "#8d96a3"
  )
  stopifnot(primary %in% names(branded_colors))
  function(n) {
    if (n > 6) warning("Branded Color Palette only has 6 colors.")

    if (n == 2) {
      other <- if (!other %in% names(branded_colors)) {
        other
      } else {
        branded_colors[other]
      }
      color_list <- c(other, branded_colors[primary])
    } else {
      color_list <- branded_colors[1:n]
    }
    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}
#' Define scale function for colour
#' @inheritParams branded_pal
#' @param ... Other parameters passed to \code{ggplot2::discrete_scale}.
#' @importFrom ggplot2 discrete_scale
scale_colour_branded <- function(primary = "blue", other = "red", direction = 1,
                                 ...) {
  ggplot2::discrete_scale(aesthetics = "colour", scale_name = "branded",
                          palette = branded_pal(primary, other, direction), ...)
}
#' @describeIn scale_colour_branded
#' Alias to scale_colour_branded
scale_color_branded <- scale_colour_branded
#' @describeIn scale_colour_branded
#' Define scale function for fill
#' @importFrom ggplot2 discrete_scale
scale_fill_branded <- function(primary = "blue", other = "red", direction = 1,
                               ...) {
  ggplot2::discrete_scale(aesthetics = "fill", scale_name = "branded",
                          palette = branded_pal(primary, other, direction), ...)
}
#' Select appropriate color/fill scale based on number of sensitive groups
#' @param x FairPkg object.
#' @param type Type to return, either "color" or "fill".
select_scale <- function(x, type = "color") {
  type <- match.arg(arg = type, choices = c("color", "fill"))
  n_sens <- x$input$n_sens
  if (n_sens > 6) {
    if (type == "color") scale_color_discrete else scale_fill_discrete
  } else {
    if (type == "color") scale_color_branded else scale_fill_branded
  }
}
