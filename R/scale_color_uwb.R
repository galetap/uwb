#' Color scale with a UWB color palette
#'
#' A ggplot2 color scale that uses one of the built-in UWB color palettes (see
#' [uwb_scales]). Use it like any other ggplot2 scale, e.g.
#' `+ scale_color_uwb("faks")`.
#'
#' @param name Name of the palette (an element of [uwb_scales]).
#' @param n Number of colors. If supplied, a continuous palette of `n` colors is
#'   interpolated; if omitted, the discrete palette is used.
#' @param add_na Add an extra color for an unknown/NA category? Default `FALSE`.
#'
#' @returns A ggplot2 color scale.
#' @seealso [scale_fill_uwb()], [uwb_palettes()], [uwb_scales]
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
