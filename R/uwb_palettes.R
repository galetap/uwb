#' Create a uwb pallete
#'
#'create a palette from the provided colors (from the list uwb_scales)
#' @param name A name of the palette
#' @param n Number of colors
#' @param all_palettes DNK
#' @param type discrete or continuous
#' @param add_na Add color for unknown category?
#'
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
