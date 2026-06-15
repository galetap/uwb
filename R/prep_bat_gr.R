#' Prepare the distribution of a battery of items by a grouping variable
#'
#' Adds a grouping variable to [prep_bat()]. The response value is `zvar`, the
#' item name is `xvar`, and the grouping variable (`zzvar` / `zzvarno`) is kept
#' for faceting. Meant for [plot_stack()] with facets.
#'
#' @param dat A data frame in wide format (one column per item).
#' @param vars The battery item columns. Defaults to all columns of `dat`.
#' @param grvar The grouping variable (unquoted column name).
#' @param add_total Append an overall ("ZCU") group for comparison? Default
#'   `FALSE`.
#' @param lab_total Label of the total group. Default `"ZCU"`.
#'
#' @returns A tibble with `yvar` (percentage per response option), `xvar` (item),
#'   `zvar` (response value), `zzvarno` (group, for faceting), and label columns.
#' @export
#'
#' @examples
#' prep_bat_gr(example_data,
#'             vars = c("bat1", "bat2", "bat3", "bat4", "bat5"),
#'             grvar = pohlavi)
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
