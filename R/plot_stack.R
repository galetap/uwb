#' Plot stacked percentages by groupvar
#'
#' @param dat A data frame.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#'
#' @returns Plot.
#' @export
#'
plot_stack  <-
  function(dat, horiz = TRUE){
  if (horiz){
    dat = dat |>
      mutate(xvar = fct_rev(xvar))
  }
  p = ggplot(dat,aes(y = yvar,x = xvar, fill = zvar)) +
    geom_col(width = 0.85, colour = .uwb_vals$barcol,
             linewidth = 0.5 * .uwb_vals$linesize,
             position = position_stack(reverse = TRUE)) +
    scale_fill_uwb("quali") +
    geom_text(aes(x = fct_rev(xvar), y = pos, label = labvar),
              colour = "white",
              size = .uwb_vals$labsize) +
    ylim(0, 100.1) + # Slightly more than 100 to make sure all values are shown
    labs(y = "%", x = "", fill = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
    theme_uwb()

  if (horiz) {
    p = p + coord_flip() +
      theme_uwb_horiz()
  }
  print(p)
}
