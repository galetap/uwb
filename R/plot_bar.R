#' Bar chart of a single categorical variable
#'
#' Draws a bar chart from a prepared tibble, typically the output of
#' [prep_single()], [prep_mc()] or [mean_bat()].
#'
#' @param dat A prepared data frame. Must include the columns `xvar`, `yvar`,
#'   `labvar_single`, `cvar`, `cvar_text`, `pos_single`, `title`, `subtitle`,
#'   `caption`.
#' @param horiz `TRUE` for a horizontal chart (default), `FALSE` for a vertical
#'   chart.
#'
#' @returns A ggplot object (also printed).
#' @export
#'
#' @examples
#' prep_single(example_data, fak) |> plot_bar()
#'
plot_bar  <-
  function(dat, horiz = TRUE){
    if (horiz){
      d = dat |> mutate(xvar = fct_rev(xvar))
    } else {
      d = dat
    }

    p = ggplot(d,aes(y = yvar,x = xvar)) +
      geom_bar(stat = "identity", width = 0.85,
               colour = .uwb_vals$barcol, fill = d$cvar) +
      geom_text(aes(x = xvar, y = pos_single, label = labvar_single),
                colour = d$cvar_text, size = .uwb_vals$labsize) +
      labs(y = "%", x = "",
           title = d$title[1],
           subtitle = d$subtitle[1],
           caption = d$caption[1]) +
      scale_x_discrete() +
      theme_uwb() +
      theme(axis.ticks.y = element_blank()
      )
    if (horiz) {
      p =
        p +
        coord_flip() +
        theme_uwb_horiz()
    }

    print(p)

  }
