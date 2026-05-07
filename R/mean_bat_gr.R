#' Means of a battery, grouped
#'
#' @param dat A data frame
#' @param vars A variable
#' @param grvar A grouping variable
#' @param round_places Rounding number
#' @param add_total DNK
#' @param lab_total DNK
#' @param na_lab DNK
#'
#' @returns A tibble
#' @export
#'
mean_bat_gr <-
  function(dat, vars, grvar, round_places = 0,
           add_total = FALSE, lab_total = "Z\u010cU",
           na_lab = c("NA", "Bez odpov\u011bd\u010fi")) {
    means = dat |>
      select(c({{grvar}}, all_of(vars))) |>
      pivot_longer(cols = all_of(vars)) |>
      mutate(value_num = parse_number(as.character(value), na = na_lab)) |>
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
