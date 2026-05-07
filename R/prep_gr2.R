#' Grouped data prep - two levels of grouping
#'
#' grvar becomes xvar? grvar2 is macro grouping var for facets
#' @param dat A data frame
#' @param var A variable
#' @param grvar The first grouped variable
#' @param grvar2 The second grouped variable
#' @param drop_na Drop NA or not?
#' @param add_total Add total or not?
#' @param lab_total DNK
#' @param grvar_to_x DNK
#' @param show_nsize DNK
#' @param x_wrap DNK
#' @param x_chrnum DNK
#' @param z_wrap DNK
#' @param z_chrnum DNK
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' prep_gr2(ggplot2::mpg, cyl, manufacturer, fl)
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
