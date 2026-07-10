#' Ribbon (Sankey-bump) plot of group shares over time
#'
#' Draws a smooth ribbon (Sankey-bump) plot from a prepared tibble, showing how
#' the share of each group (`zvar`) evolves across the time axis (`xvar`). Each
#' ribbon is labelled at its end. `xvar` must be a factor.
#'
#' @param dat A prepared data frame. Must include the columns `xvar` (time,
#'   factor), `yvar`, `zvar` (group), `labvar`, `title`, `subtitle`, `caption`.
#' @param space Vertical space between ribbons. Default `5`.
#' @param fill Vector of fill colours for the ribbons. Defaults to the `quali`
#'   scale.
#' @param margin Right-hand margin (and label wrapping width) reserved for the
#'   ribbon-end annotations. Default `15`.
#' @param lab_min Minimum ribbon size (percentage) below which the label is
#'   suppressed (set to `NA`). Default `.uwb_vals$lim_stack_no`.
#'
#' @returns A ggplot object.
#' @export
#'
plot_ribbon <-
  function(dat, space = 5, fill = .uwb_scales$quali, margin = 15,
           lab_min = .uwb_vals$lim_stack_no){
  d = dat |>
    # Special vars for ggsankey
    group_by(zvar) |>
    mutate(next_x = lead(xvar),
           next_node = lead(zvar)) |>
    ungroup()

  # Space between ribbons
  #space = space

  preplot =
    ggplot() +
    ggsankey::geom_sankey_bump(
      data = d,
      aes(x = xvar, node = zvar, fill = factor(zvar), group = zvar, value = yvar,
          label = zvar, next_x = next_x, next_z = next_node),
      space = space, color = "transparent", smooth = 15,
      type = "sankey"
    ) +
    scale_fill_manual(values = fill) +
    theme_uwb() +
    labs(x = "", y = "", fill = "",
         title = d$title[1],
         subtitle = d$subtitle[1],
         caption = d$caption[1]) +
    theme(legend.position = "bottom") +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())

  # Position of labels (inside ribbon boxes)
  x_len = length(levels(d$xvar))

  plotdata =
    ggplot_build(preplot) |>
    purrr::pluck("data")

  pos_label <-
    plotdata[[1]]  |>
    mutate(x = round(x, 1)) |>
    filter(x %in% 1:x_len) |>
    distinct() |>
    group_by(x, group) |>
    mutate(ypos = mean(y), ymin = min(y), ymax = max(y)) |>
    select(-y) |>
    ungroup() |>
    unique() |>
    left_join(tibble(x = 1:x_len, xvar = levels(d$xvar))) |>
    left_join(d |> select(xvar, yvar, zvar, labvar) |> rename(label = zvar))

  # Ribbon ends
  rib_ends =
    pos_label |>
    filter(x == 1 | x == x_len) |>
    mutate(xmin = case_when(x == 1 ~ 0.75,
                            x == x_len ~ x_len - 0.1),
           xmax = case_when(x == 1 ~ 1.1,
                            x == x_len ~ x_len + 0.25),
    )

  # Position of ribbon labels (annotation at the end of the ribbon)
  last =
    rib_ends |>
    filter(x == x_len) |>
    mutate(label = str_wrap(label, margin))

  # Final plot
  preplot +
    geom_rect(data = rib_ends, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = rib_ends$fill) +
  geom_text(data = pos_label, aes(x = xvar, y = ypos,
                                  label = ifelse(abs(yvar) < lab_min, NA, as.character(labvar))),
              color = "white", size = 0.85 * .uwb_vals$labsize) +
    annotate("text", x = x_len + 0.35, y = last$ypos, label = last$label,
             color = last$fill, size = .uwb_vals$labsize,
             hjust = 0, lineheight = 0.75) +
    guides(fill = "none") +
    coord_cartesian(clip = "off") +   # This allows text outside the panel
    theme(plot.margin = unit(c(1, 0.6 * margin, 1, 0), "lines"),
          plot.title =
            ggtext::element_textbox_simple(margin = unit(c(0, -0.6 * margin, 1, 0), "lines") )) # negative right margin pushes text outward
}

