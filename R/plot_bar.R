#' Barplot of a single categorical variable
#'
#' @param dat A data frame to plot.
#' It has to include columns named: xvar, yvar, labvar (text labels for yvar), cvar(fill color), title, subtitle, caption.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#'
#' @returns Plot
#' @export
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
      scale_x_discrete(labels = scales::label_wrap(.uwb_vals$chrnum)) +
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
