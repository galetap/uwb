#' Line + point trend plot for a single variable
#'
#' Draws a trend line with points and value labels from a prepared tibble. The
#' category axis (`xvar`) is treated as the time variable.
#'
#' @param dat A prepared data frame. Must include the columns `xvar` (time),
#'   `yvar`, `labvar_full`, `title`, `subtitle`, `caption`.
#'
#' @returns A ggplot object (also printed).
#' @export
#'
plot_trend  <- function(dat) {
  p =
    ggplot(dat, aes(y = yvar, x = xvar, group = 1)) +
    geom_line(size = 1, color = .uwb_scales$quali[1]) +
    geom_point(size = .uwb_vals$pointsize,
               color = .uwb_scales$quali[1]) +
    geom_text(aes(label = labvar_full),
              color = "white",
              size = .uwb_vals$labsize) +
    labs(y = "", x = "", fill = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
    theme_uwb()
  print(p)
}
