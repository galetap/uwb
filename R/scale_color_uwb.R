#' scale_color_uwb
#'
#' @param name A name of the palette
#' @param n Number of colors
#' @param add_na Add color for unknown category?
#'
#' @export
#'
scale_color_uwb  <-
  function(name, n, add_na = FALSE) {
  if (missing(n)) {
    ggplot2::scale_color_manual(
      values = uwb_palettes(name, type = "discrete"))
  } else {
    ggplot2::scale_color_manual(
      values = uwb_palettes(name, n, type = "continuous"))
  }
}
