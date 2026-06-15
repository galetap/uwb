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
mean_bat_gr2  <-
  function(dat, vars, grvar, grvar2, round_places = 0,
           na_lab = c("NA", "Bez odpov\u011bd\u010fi")) {
    means = dat |>
      select(c({{grvar}}, {{grvar2}}, all_of(vars))) |>
      pivot_longer(cols = all_of(vars)) |>
      mutate(value_chr = as.character(value),
             value_chr = if_else(value_chr %in% na_lab, NA_character_, value_chr),
             value_num = parse_number(value_chr)) |>
      drop_na(value_num) |>
      group_by(name, {{grvar}}, {{grvar2}}) |>
      summarise(yvar = mean(value_num, na.rm = T),
                nsize = n()) |>
      ungroup() |>
      impute_labs() |>
      mutate(labvar_full = round(yvar, round_places),
             xvar = paste0({{grvar}}, "\nn=", nsize),
             zvar = {{grvar2}}
      )
    #means = try(means |> mutate(zvar = lab), silent = T) #try to rename items with lab from codebook
  }
