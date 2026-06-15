#' Mean scores of a battery by one grouping variable
#'
#' Computes item means within each level of `grvar`. The grouping variable
#' becomes the category axis (`xvar`, with `n=` appended) and the item name
#' becomes the secondary category (`zvar`). Meant for [plot_dodge()] or
#' [plot_lolli()].
#'
#' @param dat A data frame.
#' @param vars The numeric item columns.
#' @param grvar The grouping variable (unquoted column name).
#' @param round_places Number of decimals used for the rounded label. Default
#'   `0`.
#' @param add_total Append an overall group for comparison against the total?
#'   Default `FALSE`.
#' @param lab_total Label of the total group. Default `"ZCU"`.
#' @param na_lab Character values to treat as missing when parsing numbers.
#'   Default `c("NA", "Bez odpovedi")`.
#'
#' @returns A tibble with `yvar` (mean within group), `xvar`, `zvar`, and label
#'   columns.
#' @export
#'
#' @examples
#' mean_bat_gr(example_data,
#'             vars = c("num1", "num2", "num3", "num4", "num5"),
#'             grvar = fak)
#'
mean_bat_gr <-
  function(dat, vars, grvar, round_places = 0,
           add_total = FALSE, lab_total = "Z\u010cU",
           na_lab = c("NA", "Bez odpov\u011bd\u010fi")) {
    means = dat |>
      select(c({{grvar}}, all_of(vars))) |>
      pivot_longer(cols = all_of(vars)) |>
      mutate(value_chr = as.character(value),
             value_chr = if_else(value_chr %in% na_lab, NA_character_, value_chr),
             value_num = parse_number(value_chr)) |>
      drop_na(value_num) |>
      group_by(name, {{grvar}}) |>
      summarise(yvar = mean(value_num, na.rm = T),
                nsize = n()) |>
      mutate(labvar_full = round(yvar, round_places),
             xvar = paste0({{grvar}}, "\nn = ", nsize),
             xvar_df = {{grvar}},
             zvar = name) |>
      ungroup() |>
      impute_labs()

    if (add_total) {
      total = mean_bat(dat = dat, vars = vars, round_places = round_places,
                       na_lab = na_lab) |>
        #select(xvar, yvar, nsize, labvar_full) |>
        mutate(zvar = xvar,
               xvar = as.factor(paste0(lab_total, "\nN=", nsize)),
               xvar_df = lab_total)
      means = bind_rows(means, total)
    }

    means = try(means |> mutate(zvar = lab), silent = T) #try to rename items with lab from codebook

  }
