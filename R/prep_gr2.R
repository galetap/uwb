#' Prepare the distribution of a categorical variable by two grouping variables
#'
#' Extends [prep_gr()] with a second grouping variable. `grvar` becomes the
#' category axis (`xvar`), `var` becomes the stacking variable (`zvar`), and
#' `grvar2` is kept (as `zzvarno`) for use as a facet. Meant for [plot_stack()]
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
#' @param show_nsize Append the group size (`n=`) to the category labels?
#'   Default `TRUE`.
#' @param x_wrap Wrap long `xvar` labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping `xvar` labels.
#' @param z_wrap Wrap long `zvar` labels onto several lines? Default `TRUE`.
#' @param z_chrnum Characters per line when wrapping `zvar` labels.
#'
#' @returns A tibble with `yvar` (percentage), `xvar`, `zvar`, `zzvarno` (facet),
#'   and label columns.
#' @export
#'
#' @examples
#' prep_gr2(example_data, typ, fak, pohlavi)
#'
prep_gr2 <-
  function(dat, var, grvar, grvar2, drop_na = TRUE, add_total = FALSE,
           lab_total = "Z\u010cU",
           grvar_to_x = TRUE, show_nsize = TRUE, x_wrap = TRUE,
           x_chrnum = .uwb_vals$chrnum,
           z_wrap = TRUE, z_chrnum = .uwb_vals$chrnum){
  groups = dat
  if (drop_na) {
    groups = groups |> drop_na({{var}})
  }

  # Add total as a pseudo-group before the main processing
  if (add_total) {
    total_rows <- groups |>
      mutate({{grvar}} := lab_total)  # Create duplicate rows with total label
    groups <- bind_rows(groups, total_rows)
  }

  groups = groups |>
    group_by({{grvar}},{{grvar2}}) |>
    count({{var}},.drop = FALSE) |>
    mutate(yvar = n / sum(n) * 100,
           xvar_df = {{grvar}},
           zvar_df = {{var}},
           zzvar_df = {{grvar2}},
           nsize = sum(n))

  # Switch the xvar and zvar if grvar_to_x is TRUE
  if (grvar_to_x) {
    groups <- groups |>  mutate(xvar_df = {{grvar}}, zvar_df = {{var}})
    x_nsize =  show_nsize
    z_nsize = FALSE # This overwrites the show_nsize option bacause it is meaningless to place nsizes when zvar is not grvar
  } else{
    x_nsize = FALSE
    z_nsize = show_nsize
  }

  groups = groups |>
    mutate(name = names(dat |> select({{var}}))) |>
    polish_var(var_origin = "xvar_df", var_final = "xvar",
                    wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize) |>
    polish_var(var_origin = "zvar_df", var_final = "zvar",
                    wrap = z_wrap, chrnum = z_chrnum, nsize = z_nsize) |>
    generate_textlabs() |>
    impute_labs() |>
    ungroup()
}
