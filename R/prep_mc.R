#' Prepare the distribution of a multiple choice question
#'
#' Summarises a block of multiple choice items (one column per option, usually
#' 0/1 coded). For each item it reports the share of respondents whose value
#' equals `chosen`. With 0/1 items, `chosen = 1` gives the percentage who chose
#' each option. Meant for [plot_bar()].
#'
#' @param dat A data frame.
#' @param vars The item columns to summarise. Defaults to all columns of `dat`.
#' @param chosen The value that counts as "selected" (e.g. `1` for 0/1 items).
#' @param drop_na Drop missing values before computing percentages? Default
#'   `TRUE`.
#' @param order Reorder items by frequency? Default `FALSE`.
#' @param x_wrap Wrap long item labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping item labels.
#' @param x_nsize Append the sample size (`n=`) to item labels? Default `TRUE`.
#'
#' @returns A tibble with one row per item (`yvar` = percentage choosing it,
#'   `xvar` = item label, plus label columns).
#' @export
#'
#' @examples
#' prep_mc(example_data, vars = c("mc1", "mc2", "mc3", "mc4", "mc5"), chosen = 1)
#'
prep_mc  <- function(
    dat, vars = names(dat), chosen, drop_na = TRUE, order = FALSE,
    x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, x_nsize = TRUE) {

  mc = dat
  mc = mc |>
    tidyr::pivot_longer(cols = all_of(vars)) |>
    dplyr::group_by(name) |>
    dplyr::count(value, .drop = FALSE) |>
    dplyr::mutate(yvar = n / sum(n) * 100,
           xvar_df = name,
           nsize = sum(n)) |>
    dplyr::filter(yvar > 0) |>
    dplyr::ungroup() |>
    generate_textlabs() |>
    impute_labs() |>
    dplyr::filter(value == chosen)
  if (order) {
    mc =
      mc |>
      dplyr::mutate(xvar_df = forcats::fct_reorder(xvar_df, -n))
  }

  if ("lab" %in% names(mc)) {
    mc <- mc |> dplyr::mutate(xvar_df = lab)
  }

  mc <- mc |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize)

  return(mc)
}
