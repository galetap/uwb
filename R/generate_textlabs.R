#' Generate vars to be used for geom_text
#'
#' @param dat A data frame
#' @param round_places Number of decimals
#'
#' @returns A data frame
#' @export
#'
generate_textlabs <- function(dat, round_places = 0){
  labdat = dat |>
    mutate(
      labvar_full = round(yvar, round_places),
      labvar_single =
        as.character(ifelse(yvar < 0.5, "<1", paste0(labvar_full, ""))), # for plotting in barplots with single columns
      pos_single =
        ifelse(yvar < .uwb_vals$lim_single_pos, yvar + max(yvar) / 100 * 5, 0.5 * yvar),
      cvar = .uwb_scales$quali[1],
      cvar_text =
        case_when(pos_single < yvar ~ 'white',
                  pos_single > yvar ~ .uwb_scales$quali[1]),
      labvar =
        as.character(ifelse(abs(yvar) < .uwb_vals$lim_stack_no, NA, paste0(labvar_full,""))), # to be compatible with plot_stack/plot_dodge
      pos = cumsum(yvar) - 0.5 * yvar
    )
}
