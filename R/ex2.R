#' Visual crosstabulation of two vars
#'
#' @param dat A name of dataframe.
#' @param var1 A name of the first variable, which goes into rows
#' @param var2 A name of the second variable, which goes into columns
#'
#' @returns Crosstabulated barplot.
#' @export
#'
#' @examples
#' ex2(ggplot2::mpg, manufacturer, cyl)
#'
ex2 <- function(dat, var1, var2){
  exx2 =
    dat |>
    count({{var1}},{{var2}})

  p =
    ggplot(exx2, aes(y = {{var1}},
                     x = {{var2}})) +
    geom_tile(aes(alpha = n), fill = "#eff34d") +
    geom_text(aes(label = n), color = 'black') +
    theme_uwb() +
    theme(legend.position = 'none')

  return(p)
}
