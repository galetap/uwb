#' Multiple choice data prep
#'
#' (= batttery with 2 options: chosen vs. not chosen)
#' @param dat A data frame
#' @param vars Variables
#' @param chosen DNK
#' @param drop_na Drop NA or not?
#' @param order Order results or not?
#' @param x_wrap DNK
#' @param x_chrnum DNK
#' @param x_nsize DNK
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' prep_mc(ggplot2::mpg |> dplyr::select(manufacturer), chosen = "audi")
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
      dplyr::mutate(xvar_df = forcats::fct_reorder(xvar, -n))
  }

  if ("lab" %in% names(mc)) {
    mc <- mc |> dplyr::mutate(xvar_df = lab)
  }

  mc <- mc |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize)

  return(mc)
}
