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
#' @param drop_na Drop rows with NA in all items? Default `TRUE`.
#' @param order Reorder items by frequency? Default `FALSE`.
#' @param x_wrap Wrap long item labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping item labels.
#' @param show_n Show "N=###" in subtitle? Default `TRUE`.
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
    x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, show_n = TRUE) {

  mc <- dat

  if (drop_na) {
    mc <- mc |>
      dplyr::filter(!dplyr::if_all(dplyr::all_of(vars), is.na))
  }

  mc = mc |>
    tidyr::pivot_longer(cols = all_of(vars)) |>
    dplyr::group_by(name) |>
    dplyr::count(value, .drop = FALSE) |>
    dplyr::mutate(
      yvar = n / sum(n) * 100,
      xvar_df = name,
      nsize = sum(n)
      ) |>
    dplyr::filter(yvar > 0) |>
    dplyr::ungroup() |>
    generate_textlabs() |>
    impute_labs() |>
    dplyr::filter(value == chosen) 
  
  if(show_n){
     mc <- mc |> dplyr::mutate(subtitle = paste0("N=", nsize))
  }

  if ("lab" %in% names(mc)) {
    mc <- mc |> dplyr::mutate(xvar_df = lab)
  }

  if (order) {
    mc <- mc |>
      dplyr::mutate(xvar_df = forcats::fct_reorder(as.character(xvar_df), -n))
  }

  mc <- mc |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = FALSE)

  return(mc)
}
