#' Mean scores of a battery by two grouping variables
#'
#' Computes item means for every combination of two grouping variables. The
#' first grouping variable (`grvar`) becomes `xvar` and the second (`grvar2`)
#' becomes `zvar`. Meant for [plot_dodge()] with facets.
#'
#' @param dat A data frame.
#' @param vars The numeric item columns.
#' @param grvar The first grouping variable, placed on the category axis.
#' @param grvar2 The second grouping variable, used as the secondary category.
#' @param round_places Number of decimals used for the rounded label. Default
#'   `0`.
#' @param na_lab Character values to treat as missing when parsing numbers.
#'   Default `c("NA", "Bez odpovedi")`.
#'
#' @returns A tibble with `yvar` (mean), `xvar`, `zvar`, and label columns.
#' @export
#'
#' @examples
#' mean_bat_gr2(example_data,
#'              vars = c("num1", "num2", "num3", "num4", "num5"),
#'              grvar = fak, grvar2 = pohlavi)
#'
mean_bat_gr2 <- function(
    dat, vars, grvar, grvar2, round_places = 0,
    add_total = FALSE, lab_total = "Z\u010cU",
    na_lab = c("NA", "Bez odpov\u011bd\u010fi"),
    show_nsize = TRUE, x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum,
    z_wrap = TRUE, z_chrnum = .uwb_vals$chrnum) {

  means = dat

  # Add total as a pseudo-group before the main processing
  if (add_total) {
    total_rows <- means |>
      mutate({{grvar}} := lab_total)  # Create duplicate rows with total label
    means <- bind_rows(means, total_rows)
  }

  means = means |>
    select(c({{grvar}}, {{grvar2}}, all_of(vars))) |>
    pivot_longer(cols = all_of(vars)) |>
    mutate(
      value_chr = as.character(value),
      value_chr = if_else(value_chr %in% na_lab, NA_character_, value_chr),
      value_num = parse_number(value_chr)) |>
    drop_na(value_num) |>
    group_by(name, {{grvar}}, {{grvar2}}) |>
    summarise(
      yvar = mean(value_num, na.rm = T),
      nsize = n()) |>
    mutate(
      xvar_df = {{grvar}},
      zvar_df = {{grvar2}}
    ) |>
    ungroup() |>
    impute_labs() |>
    generate_textlabs(round_places = round_places)

  means <- means |>
    polish_var(var_origin = "xvar_df", var_final = "xvar", wrap = x_wrap, chrnum = x_chrnum, nsize = show_nsize) |>
    polish_var(var_origin = "zvar_df", var_final = "zvar", wrap = z_wrap, chrnum = z_chrnum, nsize = FALSE)

  return(means)
}
