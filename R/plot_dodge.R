#' Plot dodge percentages by groupvar
#'
#' @param dat A data frame.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#'
#' @returns Plot.
#' @export
#'
plot_dodge <-
  function(dat, horiz = TRUE){
  d <- dat |>
    mutate(labvar_out = case_when(pos_single > yvar ~ labvar_single))

  if (horiz){
    d <- d |> mutate(xvar = fct_rev(xvar))
  }

  p <- ggplot(d, aes(y = yvar, x = xvar, fill = fct_rev(zvar))) +
    geom_col(width = 0.85, colour = .uwb_vals$barcol,
             linewidth = 0.5 * .uwb_vals$linesize,
             position = position_dodge(width = 0.85, preserve = "single")) +
    # Text labs inside bars
    geom_text(aes(x = xvar, y = pos_single, label = labvar),
              position = position_dodge(width = 0.85, preserve = "single"),
              colour = "white", size = .uwb_vals$labsize) +
    # Text labs outside bars
    geom_text(aes(x = xvar, y = pos_single,
                  label = labvar_out, color = fct_rev(zvar)),
              position = position_dodge(width = 0.85,
                                        preserve = "single"),
              size = .uwb_vals$labsize) +
    labs(y="",x = "", fill = '',
         title = d$title[1],
         subtitle = d$subtitle[1],
         caption = d$caption[1]) +
    scale_fill_uwb("quali") +
    scale_color_uwb("quali") +
    guides(fill = guide_legend(reverse = TRUE), color = "none") +
    theme_uwb_horiz()

  if (horiz) {
    p <- p + coord_flip() +
      theme_uwb_horiz()
  }
  print(p)
}
