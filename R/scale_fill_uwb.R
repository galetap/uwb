#' scale_fill_uwb
#'
#' @param name A name of the palette
#' @param n Number of colors
#' @param add_na Add color for unknown category?
#'
#' @export
#'
scale_fill_uwb <-
  function(name = 'quali', n, add_na = FALSE) {
  if (missing(n)) {
    ggplot2::scale_fill_manual(
      values = uwb_palettes(name, type = "discrete", add_na = add_na))
  } else {
    ggplot2::scale_fill_manual(
      values = uwb_palettes(name, n, type = "continuous", add_na = add_na))
  }
}
