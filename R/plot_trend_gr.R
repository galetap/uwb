#' Trend plot for several groups
#'
#' Draws a multi-line trend plot from a prepared tibble: the category axis
#' (`xvar`) is the time variable and `zvar` defines the groups (one line each).
#'
#' @param dat A prepared data frame. Must include the columns `xvar` (time),
#'   `yvar`, `zvar` (group), `labvar`, `title`, `subtitle`, `caption`.
#' @param total Optional name of the group (level of `zvar`) to emphasise with
#'   larger points (e.g. the overall total).
#' @param add_points Add points and value labels on top of the lines? Default
#'   `TRUE`.
#'
#' @returns A ggplot object.
#' @export
#'
plot_trend_gr  <-
  function(dat, total, add_points = TRUE) {
    if (missing(total)) { # Larger size of total points
      ddat = dat |>
        mutate(pointsize = .uwb_vals$pointsize)
    } else {
      ddat = dat |>
        mutate(pointsize = case_when(zvar == total ~ .uwb_vals$pointsize + 5,
                                     TRUE ~ .uwb_vals$pointsize))
    }

    p =
      ggplot(ddat, aes(y = yvar, x = xvar, group = zvar, color = zvar )) +
      geom_line(size = 1, alpha = 1) +
      scale_color_uwb("quali") +
      labs(y = "", x = "", color = "",
           title = dat$title[1],
           subtitle = dat$subtitle[1],
           caption = dat$caption[1]) +
      theme_uwb()

    if (add_points) {
      p =
        p + geom_point(size = ddat$pointsize, alpha = 0.95) +
        geom_text(aes(label = labvar),
                  color = "white", size = 0.75 * .uwb_vals$labsize,
                  check_overlap = TRUE) +
        guides(color = guide_legend(override.aes = list(size = 0.5 * .uwb_vals$pointsize)))
    }

    return(p)
  }
