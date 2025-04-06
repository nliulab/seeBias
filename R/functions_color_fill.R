#' Modified fill palette baesd on ggsci::pal_jama
#' @importFrom scales manual_pal
pal_jama_fill <- function(palette = c("default"), alpha = 1) {
  palette <- match.arg(palette)
  if (alpha > 1L || alpha <= 0L) stop("alpha must be in (0, 1]")
  raw_cols_original <- ggsci:::ggsci_db$jama[[palette]]
  # Color based on ggsci::pal_jama("default", alpha = 0.8)
  raw_cols_rgb <- cbind(c(107, 119, 124),
                        c(220, 169, 120),
                        c(106, 178, 217),
                        c(184, 117, 115),
                        c(159, 191, 174),
                        c(138, 136, 172),
                        c(155, 151, 141))
  rownames(raw_cols_rgb) <- c("red", "green", "blue")
  colnames(raw_cols_rgb) <- names(raw_cols_original)
  alpha_cols <- rgb(red = raw_cols_rgb[1L, ], green = raw_cols_rgb[2L, ],
                    blue = raw_cols_rgb[3L, ], alpha = alpha * 255L,
                    names = names(raw_cols_original),
                    maxColorValue = 255L)
  scales::manual_pal(unname(alpha_cols))
}
#' @describeIn pal_jama_fill Fill scale based on pal_jama_fill
#' @import ggsci
#' @importFrom ggplot2 discrete_scale
scale_fill_jamma_alpha <- function(palette = c("default"), alpha = 1, ...) {
  palette <- match.arg(palette)
  if (ggsci:::is_ggplot2_350()) {
    ggplot2::discrete_scale("fill", palette = pal_jama_fill(palette, alpha), ...)
  }
  else {
    ggplot2::discrete_scale("fill", scale_name = "jama", palette =
                              pal_jama_fill(palette, alpha), ...)
  }
}
#' Select appropriate color/fill scale based on number of sensitive groups
#' @param x seeBias object.
#' @param type Type to return, either "color" or "fill".
#' @importFrom ggsci scale_color_jama scale_fill_jama
#' @importFrom ggplot2 scale_color_discrete scale_fill_discrete
select_scale <- function(x, type = "color") {
  type <- match.arg(arg = type, choices = c("color", "fill"))
  n_sens <- x$input$n_sens
  if (n_sens > 7) {
    if (type == "color") scale_color_discrete else scale_fill_discrete
  } else {
    if (type == "color") ggsci::scale_color_jama else scale_fill_jamma_alpha # ggsci::scale_fill_jama
  }
}

