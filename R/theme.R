#' Default uwb theme
#'
#' @param theme Main theme to be adjusted.
#' @noRd
#'
# theme_uwb -----------------
# I used to base it on theme_minimal, but it did not work together with element_markdown,
# theme_void is better as a starting point.
theme_uwb <-  function(theme = theme_void()){
  # Font
  font = .uwb_vals$font
  theme +
    theme(
      # Axis lines + ticks
      axis.ticks.y = element_line(color = .uwb_vals$axiscol,
                                  linetype = "solid",
                                  linewidth = .uwb_vals$axissize),
      axis.line.y = element_line(color = .uwb_vals$axiscol,
                                 linetype = "solid",
                                 linewidth = .uwb_vals$axissize),
      # Legend
      legend.position = "bottom",
      legend.text =
        ggtext::element_markdown(size = 0.85 * .uwb_vals$tsize,
                                 family = .uwb_vals$font),
      # Text elements
      plot.title =
        ggtext::element_textbox_simple(size = 1.25 * .uwb_vals$tsize,
                                       family = font,
                                       lineheight = .uwb_vals$lineheight_tit),
      plot.title.position = "plot",
      plot.subtitle =
        ggtext::element_textbox_simple(size = 0.85 * .uwb_vals$tsize,
                                       family = font,
                                       hjust = 0),
      plot.caption =
        ggtext::element_textbox_simple(size = 0.85 * .uwb_vals$tsize,
                                       family = font,
                                       hjust = 0,
                                       lineheight = .uwb_vals$lineheight),
      plot.caption.position = "plot",
      strip.text =
        element_text(size = .uwb_vals$tsize,
                     family = font,
                     margin = margin(b = 1.5)),
      axis.title =
        ggtext::element_markdown(color = .uwb_vals$axiscol,
                                 size = .uwb_vals$tsize,
                                 family = font),
      axis.text =
        ggtext::element_markdown(size = .uwb_vals$tsize,
                                 family = .uwb_vals$font,
                                 lineheight = .uwb_vals$lineheight),
      axis.text.y =
        ggtext::element_markdown(color = .uwb_vals$axiscol,
                                 margin = margin(r = 5, l = 5, t = 5, b = 5))
    )
}

# theme_uwb_horiz --------------------
theme_uwb_horiz  <- function(theme = theme_uwb()) {
  theme +
    theme(
      axis.line.x = element_line(color = .uwb_vals$axiscol,
                                 linetype = "solid",
                                 linewidth = .uwb_vals$axissize),
      axis.line.y = element_blank(),
      axis.text.y =
        ggtext::element_markdown(color = "black",
                                 margin = margin(r = 5, l = 5, t = 5, b = 5)),
      axis.text.x =
        ggtext::element_markdown(color = .uwb_vals$axiscol,
                                 margin = margin(r = 5, l = 5, t = 5, b = 5)),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = .uwb_vals$axiscol,
                                  linetype = "solid",
                                  linewidth = .uwb_vals$axissize)
    )
}
