#' UWB ggplot2 theme
#'
#' A minimal ggplot2 theme with the visual defaults of the University of West
#' Bohemia (fonts, text sizes, axis lines and ticks) as stored in [uwb_vals].
#' It is applied automatically inside the package's `plot_*()` functions, but
#' can also be added directly to any manually built ggplot.
#'
#' @param theme Base theme to be adjusted. Defaults to [ggplot2::theme_void()],
#'   which works better than `theme_minimal()` together with `ggtext`
#'   markdown elements.
#'
#' @return A ggplot2 theme object that can be added to a plot with `+`.
#' @seealso [theme_uwb_horiz()], [uwb_vals]
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(example_data, aes(x = fak)) +
#'   geom_bar(fill = c_zcu) +
#'   labs(x = "", y = "Count") +
#'   theme_uwb()
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

#' UWB ggplot2 theme for plots with flipped axes
#'
#' A variant of [theme_uwb()] intended for plots with flipped coordinates
#' (e.g. horizontal bar charts produced with `coord_flip()`). It draws the
#' axis line and ticks on the x axis instead of the y axis.
#'
#' @param theme Base theme to be adjusted. Defaults to [theme_uwb()].
#'
#' @return A ggplot2 theme object that can be added to a plot with `+`.
#' @seealso [theme_uwb()], [uwb_vals]
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(example_data, aes(x = fak)) +
#'   geom_bar(fill = c_zcu) +
#'   labs(x = "", y = "Count") +
#'   coord_flip() +
#'   theme_uwb_horiz()
#'
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