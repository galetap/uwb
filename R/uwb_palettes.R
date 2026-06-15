#' Build a UWB color palette
#'
#' Creates a vector of colors from one of the built-in UWB color scales (see
#' [uwb_scales]). Used internally by [scale_fill_uwb()] and [scale_color_uwb()],
#' but can also be called directly to build custom color sequences.
#'
#' @param name Name of the palette (an element of `all_palettes`). Default
#'   `"quali"`.
#' @param n Number of colors. If omitted, the full palette is used (minus one
#'   when `add_na = TRUE`).
#' @param all_palettes List of available palettes. Defaults to the built-in
#'   `uwb_scales`.
#' @param type `"discrete"` to take the first `n` colors as-is, or
#'   `"continuous"` to interpolate `n` colors across the palette.
#' @param add_na Append an extra color for an unknown/NA category? Default
#'   `FALSE`.
#'
#' @returns A character vector of hex colors (of class `"palette"`).
#' @seealso [scale_fill_uwb()], [scale_color_uwb()], [uwb_scales]
#' @export
#'
#' @examples
#' uwb_palettes(name = "quali", n = 3, type = "discrete")
#' uwb_palettes(n = 5, add_na = TRUE)
#'
uwb_palettes  <-
  function(name = 'quali', n,
           all_palettes = .uwb_scales,
           type = c("discrete", "continuous"),
           add_na = FALSE) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    # n = length(palette)
    n = ifelse(add_na == TRUE, length(palette) - 1, length(palette))
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  if (add_na == TRUE) {
    out = c(out, .c_nor)
  }
  structure(out, name = name, class = "palette")
}
