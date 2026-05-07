#' Grouped multiple choice data prep
#'
#' grvar becomes xvar
#'
#' @param dat A data frame
#' @param vars A variable
#' @param grvar A grouping variable
#' @param chosen A value
#' @param drop_na Drop NA or not?
#' @param add_total Add total or not?
#' @param lab_total DNK
#' @param x_wrap DNK
#' @param x_chrnum DNK
#' @param x_nsize DNK
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' "DNK"
#'
prep_mc_gr <- function(
    dat, vars = names(dat), grvar, chosen,  drop_na = TRUE,
    add_total = FALSE, lab_total = "Z\u010cU",
    x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, x_nsize = TRUE) {
  mc <- dat |>
    select(c({{grvar}}, all_of(vars))) |>
    pivot_longer(cols = all_of(vars)) |>
    group_by(name, {{grvar}}) |>
    count(value, .drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           zvar = name,
           nsize = sum(n),
           xvar = paste0({{grvar}}, "\nn=", nsize),
           xvar_df = {{grvar}},) |>
    filter(yvar > 0) |>
    filter(value == chosen) |>
    ungroup() |>
    generate_textlabs() |>
    impute_labs()
  if (add_total) {
    total = prep_mc(dat = dat, vars = vars,
                         chosen = chosen, drop_na = drop_na) |>
      mutate(zvarno = lab_total,
             zvar = paste0(lab_total, "\nn=", nsize))
    mc = mc |> bind_rows(total)
  }
  mc <- try(mc |> mutate(zvar = lab), silent = T) #try to rename items with lab from codebook
  mc <-
    mc |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize)

  return(mc)
}
