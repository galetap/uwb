#' Point chart highlighting groups with extreme values
#'
#' Plots the total value per category together with the individual groups,
#' jittered around each category. Groups whose value differs from the total by
#' more than `out` are highlighted (and labelled); the others are drawn as faint
#' shadow points. The prepared data must contain both the groups and a total
#' (e.g. via `add_total = TRUE` in [prep_gr()] or [mean_bat_gr()]). The default
#' colour scheme is `faks`.
#'
#' @param dat A prepared data frame. Must include the columns `xvar`, `yvar`,
#'   `zvar`, `zvar_df`, `labvar`, `title`, `subtitle`, `caption`.
#' @param horiz `TRUE` for a horizontal chart, `FALSE` for a vertical chart
#'   (default).
#' @param total Label of the total group (level of `zvar_df`). Default `"ZCU"`.
#' @param c_total Colour of the total point/line. Defaults to the last `faks`
#'   colour.
#' @param add_line Add a line connecting the total values (for trends)? Default
#'   `TRUE`.
#' @param out Threshold: groups differing from the total by more than this are
#'   highlighted as outliers.
#' @param jitter_w Horizontal jitter width for the group points. Default `0.3`.
#' @param alpha_shadow Transparency of the non-highlighted shadow points.
#'   Default `0.15`.
#' @param seed Random seed for reproducible jitter. Default `123456`.
#'
#' @returns A ggplot object.
#' @export
#'
plot_out  <-
  function(dat, horiz = FALSE, total = "Z\u010cU",
           c_total = .uwb_scales$faks[10], add_line = TRUE,
           out, jitter_w = 0.3, alpha_shadow = 0.15, seed = 123456) {

  if(horiz) {
    d <- dat |> mutate(xvar = fct_rev(xvar))
  } else {
    d <- dat
  }

  d <- d |>
    mutate(
      diff = abs(yvar - ifelse(length(yvar[zvar_df == total]) > 0,
                               yvar[zvar_df == total], NA)),
      yvar_show = case_when(zvar_df != total & diff > out ~ yvar),
      yvar_shadow = case_when(zvar_df != total & diff <= out ~ yvar),
      labvar_show = case_when(zvar_df != total & diff > out ~ labvar),
      yvar_total = case_when(zvar_df == total ~ yvar),
      labvar_total = case_when(zvar_df == total ~ labvar)
    ) |>
    group_by(xvar) |>
    mutate(
      # Create fixed jitter offset based on group position
      group_id = as.numeric(factor(zvar)),
      n_groups = n_distinct(zvar),
      jitter_offset = (group_id - (n_groups + 1) / 2) * (jitter_w / n_groups)
    ) |>
    ungroup()

  p <-
    ggplot(data = d, aes(y = yvar,
                         x = as.numeric(xvar) + jitter_offset,
                         color = zvar)) +
    geom_point(aes(y = yvar_shadow),    # shadow points for groups
               size = 0.65 * .uwb_vals$pointsize,
               color = "black", alpha = alpha_shadow) +
    # optional trend line
    {if(add_line) geom_line(aes(y = yvar_total,
                                x = as.numeric(xvar),
                                group = fak),
                            linewidth = .uwb_vals$linesize,
                            color = c_total)} +
    geom_point(aes(y = yvar_total,
                   x = as.numeric(xvar)),
               size = .uwb_vals$pointsize, color = c_total) + # total
    geom_point(aes(y = yvar_show),  # outlier groups
               size = 0.65 * .uwb_vals$pointsize) +
    geom_text(aes(y = yvar_total,
                  x = as.numeric(xvar),
                  label = labvar_total),
              size = 0.85 * .uwb_vals$labsize, color = "white") +  # total
    geom_text(aes(y = yvar_show, label = labvar_show), # outlier groups
              size = 0.45 * .uwb_vals$labsize, color = "white", check_overlap = TRUE) +
    scale_color_uwb("faks") +
    scale_x_continuous(breaks = 1:length(levels(droplevels(d$xvar))),
                       labels = levels(droplevels(d$xvar))) +
    labs(y = "", x = "", color = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
    theme_uwb()

  if(horiz){
    p <-
      p +
      coord_flip() +
      theme_uwb_horiz() +
      theme(axis.ticks.x = element_blank())
  }

  return(p)
}
