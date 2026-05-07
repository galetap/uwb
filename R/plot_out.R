#' Plot points: total value + groups with highlighted outliers (jittered)
#' Default color scheme is faks because this is the most common use case
#' Data has to be grouped in order to compute differences from the main trend, the default grouping is by xvar
#' Data has to include groups + total
#'
#' @param dat A data frame.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#' @param total DNK
#' @param c_total DNK
#' @param add_line TRUE is for trends, it adds a trend line
#' @param out DNK
#' @param jitter_w DNK
#' @param alpha_shadow DNK
#' @param seed Random seed number.
#'
#' @returns Plot
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
      labvar_show = case_when(zvar_df != total & diff > out ~ labvar_full),
      yvar_total = case_when(zvar_df == total ~ yvar),
      labvar_total = case_when(zvar_df == total ~ labvar_full)
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
