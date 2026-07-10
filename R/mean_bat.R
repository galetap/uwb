#' Mean scores of a battery of numeric items
#'
#' Computes the mean (and sample size) of each numeric item in `vars`, returning
#' `yvar` as the mean instead of a percentage. Values are parsed with
#' [readr::parse_number()], treating the strings in `na_lab` as missing. Meant
#' for [plot_lolli()] or [plot_bar()].
#'
#' @param dat A data frame.
#' @param vars The numeric item columns.
#' @param round_places Number of decimals used for the rounded label
#'   (`labvar`). Default `0`.
#' @param na_lab Character values to treat as missing when parsing numbers.
#'   Default `c("NA", "Bez odpovedi")`.
#'
#' @returns A tibble with one row per item (`yvar` = mean, `nsize` = sample size,
#'   `xvar` = item, plus label columns).
#' @export
#'
#' @examples
#' mean_bat(example_data, vars = c("num1", "num2", "num3", "num4", "num5"))
#'

mean_bat <-
  function(dat, vars, round_places = 0,
           na_lab = c("NA", "Bez odpov\u011bd\u010fi")) {
    means = dat |>
    select(all_of(vars)) |>
    pivot_longer(cols = all_of(vars)) |>
    mutate(value_chr = as.character(value),
           value_chr = if_else(value_chr %in% na_lab, NA_character_, value_chr),
           value_num = parse_number(value_chr)) |>
    drop_na(value_num) |>
    group_by(name) |>
    summarise(yvar = mean(value_num, na.rm = T),
              nsize = n()) |>
    mutate(xvar = name) |>
    generate_textlabs() |>
    impute_labs() |>
    mutate(labvar = round(yvar, round_places),
           labvar_single = as.character(labvar)
    ) |>
    ungroup()
}
