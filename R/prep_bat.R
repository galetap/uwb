#' Prepare the distribution of a battery of items
#'
#' A battery is a block of items sharing the same (usually ordered) response
#' scale. For each item this computes the percentage falling into each response
#' category: the item name becomes `xvar` and the response value becomes `zvar`.
#' Meant for [plot_stack()] (one stacked bar per item).
#'
#' @param dat A data frame in wide format (one column per item).
#' @param vars The battery item columns. Defaults to all columns of `dat`.
#' @param drop_na Drop missing answers before computing percentages? Default
#'   `TRUE`.
#' @param x_wrap Wrap long item labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping item labels.
#' @param x_nsize Append the sample size (`n=`) to item labels? Default `FALSE`.
#'
#' @returns A tibble with `yvar` (percentage per response option), `xvar` (item),
#'   `zvar` (response value), and label columns.
#' @export
#'
#' @examples
#' prep_bat(example_data, vars = c("bat1", "bat2", "bat3", "bat4", "bat5"))
#'
prep_bat  <-
  function(dat, vars = names(dat), drop_na = TRUE,
           x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, x_nsize = FALSE) {

  bat = dat |>
    pivot_longer(cols = all_of(vars))

  if (drop_na) {
    bat = bat |> drop_na(value)
  }

  bat = bat |>
    group_by(name) |>
    count(value,.drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           xvar_df = name,
           zvar = value,
           nsize = sum(n)) |>
    filter(yvar > 0) |>
    generate_textlabs() |>
    impute_labs() |>
    ungroup()

  bat_try = try(bat |> mutate(xvar_df = lab), silent = TRUE) #try to rename items with lab from codebook
  if (!inherits(bat_try, "try-error")) bat <- bat_try
  bat = bat |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize)

  return(bat)

}
