#' Generate final version xvar or zvar to be used in plots
#'
#'It allows wrapping long text and addding nsize info
#'result_name:
#'xvar_df/zvar = default, basic text version as found in the data set
#'xzvar_nr = helper var with level number, to be used for reordering the modified version, removed
#'xvar/zvar = final version that goes to the plot (wrapped or not, with or without nsize)

#' @param dat A data frame
#' @param var_origin DNK
#' @param var_final DNK
#' @param wrap DNK
#' @param chrnum DNK
#' @param nsize DNK
#'
#' @returns DNK
#' @export
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
