#' Prepare the distribution of a categorical variable by two grouping variables
#'
#' Extends [prep_gr()] with a second grouping variable. `grvar` becomes the
#' category axis (`xvar`), `var` becomes the stacking variable (`zvar`), and
#' `grvar2` is kept (as `zzvar_df`) for use as a facet. Meant for [plot_stack()]
#' with facets.
#'
#' @param dat A data frame.
#' @param var The categorical variable (unquoted column name).
#' @param grvar The first grouping variable, placed on the category axis.
#' @param grvar2 The second grouping variable, kept for faceting.
#' @param drop_na Drop missing values of `var` before computing percentages?
#'   Default `TRUE`.
#' @param add_total Append a pseudo-group containing all respondents? Default
#'   `FALSE`.
#' @param lab_total Label of the total pseudo-group. Default `"ZCU"`.
#' @param grvar_to_x Place `grvar` on the category axis (`xvar`) and `var` as the
#'   secondary category (`zvar`)? Default `TRUE`; set `FALSE` to swap roles.
#' @param show_nsize Append the group size (`n=`) to the category labels (`xvar`)?
#'   Default `TRUE`.
#' @param x_wrap Wrap long `xvar` labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping `xvar` labels.
#' @param z_wrap Wrap long `zvar` labels onto several lines? Default `TRUE`.
#' @param z_chrnum Characters per line when wrapping `zvar` labels.
#' @param zz_wrap Wrap long `zzvar` labels onto several lines? Default `TRUE`.
#' @param zz_chrnum Characters per line when wrapping `zzvar` labels.
#'
#' @returns A tibble with `yvar` (percentage), `xvar`, `zvar`, `zzvar_df` (facet),
#'   and label columns.
#' @export
#'
#' @examples
#' prep_gr2(example_data, typ, fak, pohlavi)
#'
prep_gr2 <- function(
    dat, var, grvar, grvar2, drop_na = TRUE, add_total = FALSE, lab_total = "Z\u010cU",
    show_nsize = TRUE, x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum,
    z_wrap = TRUE, z_chrnum = .uwb_vals$chrnum,
    zz_wrap = TRUE, zz_chrnum = .uwb_vals$chrnum) {
    
  groups <- dat
  if (drop_na) {
    groups = groups |> drop_na({{var}})
  }

  # Add total as a pseudo-group before the main processing
  if (add_total) {
    total_rows <- groups |>
      mutate({{grvar}} := lab_total)  # Create duplicate rows with total label
    groups <- bind_rows(groups, total_rows)
  }

  groups <- groups |>
    group_by({{grvar}},{{grvar2}}) |>
    count({{var}},.drop = FALSE) |>
    mutate(
      yvar = n / sum(n) * 100,
      name = names(dat |> select({{var}})),
      nsize = sum(n), 
      xvar_df = {{grvar}},
      zvar_df = {{var}},
      zzvar_df = {{grvar2}}
    )

  groups = groups |>
    mutate(name = names(dat |> select({{var}}))) |>
    polish_var(var_origin = "xvar_df", var_final = "xvar",
      wrap = x_wrap, chrnum = x_chrnum, nsize = show_nsize) |>
    polish_var(var_origin = "zvar_df", var_final = "zvar",
      wrap = z_wrap, chrnum = z_chrnum, nsize = FALSE) |>
    polish_var(var_origin = "zzvar_df", var_final = "zzvar",
      wrap = zz_wrap, chrnum = zz_chrnum, nsize = FALSE) |>
    generate_textlabs() |>
    impute_labs() |>
    ungroup()
  }
