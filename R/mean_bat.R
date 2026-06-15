#' Means of a battery
#'
#' @param dat A data frame
#' @param vars A variable
#' @param round_places Rounding number
#' @param na_lab DNK
#'
#' @returns A tibble
#' @export
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
    mutate(labvar_full = round(yvar, round_places),
           labvar_single = as.character(labvar_full)
    ) |>
    ungroup()
}
