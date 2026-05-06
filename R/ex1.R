#' Bar plot for a single variable
#'
#' (preferably categorical var or numeric var with few values).
#'
#' @param dat A name of dataframe.
#' @param var A name of variable.
#'
#' @returns Horizontal barplot.
#' @export
#'
#' @examples
#' ex1(ggplot2::mpg, manufacturer)

# Function
ex1 <-  function(dat, var){
    nsize_listwise = nrow(dat |> drop_na({{var}}))
    vartitle = names(dat |> select({{var}}))

    exx1 =
      dat |>
      count({{var}}, .drop = FALSE) |>
      mutate(yvar_n = n,
             yvar = n/sum(n)*100,
             xvar = as_factor({{var}}),
             labvar = paste0(round(yvar,0),'% (',yvar_n,')'),
             nsize = sum(n))

    p =
      ggplot(exx1,
             aes(y = yvar,x = fct_rev(xvar))) +
      geom_col(width = 0.85, colour = .uwb_vals$barcol, fill = "#eff34d") +
      geom_text(aes(x = xvar, y = ifelse(yvar < 12,6,(yvar - 6)), label = labvar),
                colour = "black", size = 0.75 * .uwb_vals$labsize) +
      labs(y = "", x = "",
           title = paste0(vartitle,', N=', nsize_listwise,'/',nrow(dat)),
           subtitle = '',
           caption = '') +
      coord_flip() +
      theme_uwb_horiz() +
      theme(axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.text.x = element_blank())

    return(p)
  }
