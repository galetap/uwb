#' Grouped battery data prep
#'
#'zvar is value for battery items
# zzvar is grouping var
# xvar is names of battery item

#' @param dat A data frame
#' @param vars A variable
#' @param grvar A grouping variable
#' @param add_total DNK
#' @param lab_total DNK
#'
#' @returns A tibble
#' @export
#'
prep_bat_gr  <-
  function(dat, vars = names(dat), grvar,
           add_total = FALSE, lab_total = "Z\u010cU") {
    bat = dat |>
      select(c({{grvar}}, all_of(vars))) |>
      pivot_longer(cols = all_of(vars)) |>
      group_by(name, {{grvar}}) |>
      count(value,.drop = FALSE) |>
      mutate(yvar = n/sum(n)*100,
             xvar = name,
             zvar = value,
             nsize = sum(n),
             zzvar = paste0({{grvar}}, "\nn=", nsize),
             zzvarno = {{grvar}},) |>
      filter(yvar > 0) |>
      generate_textlabs() |>
      impute_labs() |>
      ungroup()

    if (add_total) {
      total = prep_bat(dat = dat, vars = vars) |>
        mutate(zzvarno = lab_total,
               zzvar = paste0(lab_total, "\nn=", nsize))
      bat = bat |> bind_rows(total)
    }

    bat = try(bat |> mutate(xvar = lab), silent = T) #try to rename items with lab from codebook

    return(bat)

  }
