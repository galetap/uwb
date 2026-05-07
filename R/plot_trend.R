#' Plot trend - single var
#'
#' xvar is the time var.
#' @param dat A data frame.
#'
#' @returns Plot.
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
