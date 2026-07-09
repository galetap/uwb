#' Prepare the distribution of a single categorical variable
#'
#' Computes the distribution (in percent) of one categorical variable and
#' returns a plot-ready tibble for [plot_bar()] or [plot_lolli()].
#'
#' @param dat A data frame.
#' @param var The categorical variable (unquoted column name).
#' @param drop_na Drop missing values of `var` before computing percentages?
#'   Default `TRUE`.
#' #' @param order Reorder categories by frequency (largest first)? Default `FALSE`.
#' @param show_n Show "N=###" in subtitle? Default `TRUE`.
#' @param x_wrap Wrap long item labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping item labels.
#' 
#' @returns A tibble with one row per category (`yvar` = percentage, `n` = count,
#'   `xvar` = plot-ready factor, plus label columns).
#' @export
#'
#' @examples
#' prep_single(example_data, fak)
#' prep_single(example_data, fak, order = TRUE)
#'
prep_single  <-
  function(
      dat, var, drop_na = TRUE, order = FALSE, show_n = TRUE,
      x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum) {
    
    single = dat
    if (drop_na) {
      single = single |> tidyr::drop_na({{var}})
    }
    single = single |>
      count({{var}},.drop = TRUE) |>
      mutate(yvar = n/sum(n) * 100,
             xvar_df = as_factor({{var}}),
             nsize = sum(n),
             name = names(dat |> select({{var}}))
      ) |>
      filter(yvar > 0) |>
      polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = FALSE) |> 
      generate_textlabs() |>
      impute_labs() 
     
    if (order) {
      single = single |> mutate(xvar = fct_reorder(xvar, -n))
    }
    if (show_n) {
    single = single |>
      mutate(subtitle = paste0("N = ", nsize))
    }
    return(single)
  }
