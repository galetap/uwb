#' Dodge (side-by-side) bar chart for grouped data
#'
#' Draws side-by-side bars from a prepared tibble, typically the output of
#' [prep_gr()], [prep_mc_gr()] or [mean_bat_gr()]. Bars are grouped and coloured
#' by `zvar`.
#'
#' @param dat A prepared data frame. Must include the columns `xvar`, `yvar`,
#'   `zvar`, `labvar`, `labvar_single`, `pos_single`, `title`, `subtitle`,
#'   `caption`.
#' @param horiz `TRUE` for a horizontal chart (default), `FALSE` for a vertical
#'   chart.
#'
#' @returns A ggplot object (also printed).
#' @export
#'
#' @examples
#' prep_gr(example_data, typ, fak) |> plot_dodge()
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
