#' Add text-label columns for plotting
#'
#' Auxiliary helper used by the `prep_*()` and `mean_*()` functions. Takes a data
#' frame with a `yvar` column and adds the columns used by `geom_text()` in the
#' plot functions: `labvar` (rounded value), `labvar_single` (label for
#' single-series bars, small values shown as `"<1"`), `pos` / `pos_single` (label
#' positions), and `cvar` / `cvar_text` (label colours for contrast).
#' Suppression of tiny slices (setting labels to `NA`) is handled inside
#' `plot_stack()`, `plot_dodge()`, and `plot_ribbon()` using
#' `.uwb_vals$lim_stack_no`.
#'
#' @param dat A data frame containing a `yvar` column.
#' @param round_places Number of decimals for the rounded labels. Default `0`.
#'
#' @returns The input data frame with the added text-label columns.
#' @export
#'
#' @examples
#' generate_textlabs(data.frame(yvar = c(5, 42, 0.3, 88)))
#'
generate_textlabs <- function(dat, round_places = 0){
  labdat = dat |>
    mutate(
      labvar = round(yvar, round_places),
      labvar_single =
        as.character(ifelse(yvar < 0.5, "<1", paste0(labvar, ""))), # for plotting in barplots with single columns
      pos_single =
        ifelse(yvar < .uwb_vals$lim_single_pos, yvar + max(yvar) / 100 * 5, 0.5 * yvar),
      cvar = .uwb_scales$quali[1],
      cvar_text =
        case_when(pos_single < yvar ~ 'white',
                  pos_single > yvar ~ .uwb_scales$quali[1]),
      pos = cumsum(yvar) - 0.5 * yvar
    )
}
