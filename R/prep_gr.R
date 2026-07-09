#' Prepare the distribution of a categorical variable by one grouping variable
#'
#' Computes the distribution (in percent) of `var` within each level of
#' `grvar`; percentages sum to 100% inside each group. The result is meant for
#' [plot_stack()] or [plot_dodge()].
#'
#' @param dat A data frame.
#' @param var The categorical variable (unquoted column name).
#' @param grvar The grouping variable (unquoted column name).
#' @param drop_na Drop missing values of `var` before computing percentages?
#'   Default `TRUE`.
#' @param add_total Append a pseudo-group containing all respondents for
#'   comparison against the total? Default `FALSE`.
#' @param lab_total Label of the total pseudo-group. Default `"ZCU"`.
#' @param show_nsize Append the group size (`n=`) to the category labels?
#'   Default `TRUE`.
#' @param x_wrap Wrap long `xvar` labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping `xvar` labels.
#' @param z_wrap Wrap long `zvar` labels onto several lines? Default `TRUE`.
#' @param z_chrnum Characters per line when wrapping `zvar` labels.
#'
#' @returns A tibble with `yvar` (percentage within group), `xvar`, `zvar`, and
#'   label columns.
#' @export
#'
#' @examples
#' prep_gr(example_data, typ, fak)
#' prep_gr(example_data, typ, fak, add_total = TRUE)
#'
prep_gr <-
  function(dat, var, grvar, drop_na = TRUE, add_total = FALSE, lab_total = "Z\u010cU",
           show_nsize = TRUE, 
           x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum,
           z_wrap = TRUE, z_chrnum = .uwb_vals$chrnum) {

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
      group_by({{grvar}}) |>
      count({{var}}, .drop = FALSE) |>
      mutate(
        yvar = n/sum(n)*100,
        name = names(dat |> select({{var}})),
        nsize = sum(n), # Data are grouped by grvar at this point
        xvar_df = {{var}},
        zvar_df = {{grvar}}
      ) |>
      filter(n > 0)

    groups = groups |>
      ungroup() |>
      group_by(xvar_df) |>
      polish_var(var_origin = "xvar_df", var_final = "xvar",
        wrap = x_wrap, chrnum = x_chrnum, nsize = show_nsize) |>
      polish_var(var_origin = "zvar_df", var_final = "zvar",
        wrap = z_wrap, chrnum = z_chrnum, nsize = FALSE) |>
      generate_textlabs() |>
      impute_labs() |>
      ungroup()

    return(groups)
  }
