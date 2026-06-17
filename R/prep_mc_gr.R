#' Prepare a multiple choice question by a grouping variable
#'
#' Computes the selection percentages of [prep_mc()] within each level of
#' `grvar`. The grouping variable goes on the category axis (`xvar`); the items
#' become the secondary category (`zvar`). Meant for [plot_stack()] or
#' [plot_dodge()].
#'
#' @param dat A data frame.
#' @param vars The item columns to summarise. Defaults to all columns of `dat`.
#' @param grvar The grouping variable (unquoted column name).
#' @param chosen The value that counts as "selected" (e.g. `1` for 0/1 items).
#' @param drop_na Drop missing values before computing percentages? Default
#'   `TRUE`.
#' @param add_total Append an overall column (all respondents) for comparison?
#'   Default `FALSE`.
#' @param lab_total Label of the total column. Default `"ZCU"`.
#' @param x_wrap Wrap long `xvar` labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping `xvar` labels.
#' @param x_nsize Append the group size (`n=`) to the category labels? Default
#'   `TRUE`.
#'
#' @returns A tibble with `yvar` (percentage choosing each item within group),
#'   `xvar`, `zvar`, and label columns.
#' @export
#'
#' @examples
#' prep_mc_gr(example_data,
#'            vars = c("mc1", "mc2", "mc3", "mc4", "mc5"),
#'            grvar = fak, chosen = 1)
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
  mc_try <- try(mc |> mutate(zvar = lab), silent = TRUE) #try to rename items with lab from codebook
  if (!inherits(mc_try, "try-error")) mc <- mc_try
  mc <-
    mc |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize)

  return(mc)
}
