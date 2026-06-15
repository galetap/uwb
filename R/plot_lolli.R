#' lollipop plot of single categorical variable
#'
#' @param dat A data frame.
#' It has to include columns named: xvar, yvar, labvar (text labels for yvar),
#' cvar(fill color), title, subtitle, caption.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#'
#' @return Plot
#' @export
#'
plot_lolli  <-
  function(dat, horiz = TRUE){
    if(horiz){
      d = dat |> mutate(xvar = fct_rev(xvar))
    } else {
      d = dat
    }
    p = ggplot(d, aes(y = yvar, x = xvar)) +
      geom_segment(aes(x = xvar, xend = xvar, y = 0, yend = yvar),
                   colour = .uwb_vals$lollistick) +
      geom_point(size = .uwb_vals$pointsize, shape = 21,
                 color = "white", fill = d$cvar) +
      geom_text(aes(x = xvar, y = yvar, label = labvar_single),
                colour = "white", size = .uwb_vals$labsize) +
      labs(y = "%", x = "",
           title = d$title[1],
           subtitle = d$subtitle[1],
           caption = d$caption[1]) +
      scale_x_discrete(labels = scales::label_wrap(.uwb_vals$chrnum)) +
      theme_uwb() +
      theme(axis.ticks.y = element_blank()
      )
    if(horiz){
      p = p + coord_flip() +
        theme_uwb_horiz() + theme(axis.ticks.x = element_blank())
    }
    print(p)
  }
