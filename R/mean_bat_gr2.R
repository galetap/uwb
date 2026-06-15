#' Means of a battery, grouped by two variables
#'
#' @param dat A data frame
#' @param vars A variable
#' @param grvar The first grouping variable
#' @param grvar2 The second grouping variable
#' @param round_places Rounding number
#' @param na_lab DNK
#'
#' @returns A tibble
#' @export
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
