#' Build the final plot-ready category factor
#'
#' Auxiliary helper used by the `prep_*()` functions. Turns a "raw" category
#' column (`var_origin`) into the final plot-ready factor (`var_final`): it
#' converts the column to a factor preserving the original order, optionally
#' wraps long labels onto several lines, and optionally appends the group size
#' as `n = ...`, colour-coded by how reliable the size is.
#'
#' @param dat A data frame (optionally containing an `nsize` column).
#' @param var_origin Name of the raw category column. Default `"xvar_df"`.
#' @param var_final Name of the resulting plot-ready factor column. Default
#'   `"xvar"`.
#' @param wrap Wrap long labels onto several lines? Default `TRUE`.
#' @param chrnum Characters per line when wrapping labels.
#' @param nsize Append the group size (`n = ...`) to each label, colour-coded by
#'   reliability? Default `TRUE`.
#'
#' @returns The input data frame with the `var_final` factor column added.
#' @export
#'
#' @examples
#' polish_var(
#'   data.frame(
#'     xvar_df = c("A very long faculty name that needs wrapping", "Short"),
#'     nsize = c(15, 320)
#'   ),
#'   chrnum = 20
#' )
#'
polish_var <- function(dat, var_origin = "xvar_df", var_final = "xvar",
                       wrap = TRUE, chrnum = .uwb_vals$chrnum, nsize = TRUE){
  d <- dat |>
    dplyr::mutate(
      vvar = forcats::as_factor(.data[[var_origin]]),
      vvar_nr = as.numeric(vvar)
    )

  if (wrap) {
    d <- d |>
      dplyr::mutate(vvar = stringr::str_wrap(vvar, width = chrnum) |>
                      stringr::str_replace_all("\n", "<br>"))
  }

  if(nsize) {
    d <- d |>
      dplyr::mutate(vvar = dplyr::case_when(
        nsize < 20 ~ glue::glue("{vvar}<br><span style = 'color:{.uwb_vals$c_nsize1}'>n = {nsize}</span>"),
        nsize < 30 ~ glue::glue("{vvar}<br><span style = 'color:{.uwb_vals$c_nsize2}'>n = {nsize}</span>"),
        TRUE ~ glue::glue("{vvar}<br><span style = 'color:{.uwb_vals$c_nsize3}'>n = {nsize}</span>")
      ))
  }

  d <- d |>
    dplyr::mutate(vvar = factor(vvar, levels = unique(vvar[order(vvar_nr)]))) |> # This works fine with grouped data
    dplyr::rename({{var_final}} := vvar) |>
    dplyr::select(-vvar_nr)
}
