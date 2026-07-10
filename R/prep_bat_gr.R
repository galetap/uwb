#' Prepare the distribution of a battery of items by a grouping variable
#'
#' Adds a grouping variable to [prep_bat()]. The response value is `zvar`, the
#' item name is `xvar`, and the grouping variable (`zzvar` / `zzvar_df`) is kept
#' for faceting. Meant for [plot_stack()] with facets.
#'
#' @param dat A data frame in wide format (one column per item).
#' @param vars The battery item columns. Defaults to all columns of `dat`.
#' @param drop_na Drop missing answers before computing percentages? 
#' As a result, the percentage base might differ across items within the same group. Default `TRUE`.
#' @param grvar The grouping variable (unquoted column name).
#' @param add_total Append an overall ("ZCU") group for comparison? Default
#'   `FALSE`.
#' @param lab_total Label of the total group. Default `"ZCU"`.
#' @param show_nsize Append the group size (`n=`) to the category labels (`xvar`)?
#'   Default `TRUE`.
#' @param x_wrap Wrap long `xvar` labels onto several lines? Default `TRUE`.
#' @param x_chrnum Characters per line when wrapping `xvar` labels.
#' @param z_wrap Wrap long `zvar` labels onto several lines? Default `TRUE`.
#' @param z_chrnum Characters per line when wrapping `zvar` labels.
#' @param show_nsize_zzvar Append the group size (`n=`) to the facet labels (`zzvar`)?
#'   Default `TRUE`.
#' @param zz_wrap Wrap long `zzvar` labels onto several lines? Default `TRUE`.
#' @param zz_chrnum Characters per line when wrapping `zzvar` labels.
#' 
#' @returns A tibble with `yvar` (percentage per response option), `xvar` (item),
#'   `zvar` (response value), `zzvar_df` (group, for faceting), and label columns.
#' @export
#'
#' @examples
#' prep_bat_gr(example_data,
#'             vars = c("bat1", "bat2", "bat3", "bat4", "bat5"),
#'             grvar = pohlavi)
#'
prep_bat_gr <- function(
    dat, vars = names(dat), grvar, drop_na = TRUE,
    add_total = FALSE, lab_total = "Z\u010cU",
    show_nsize = TRUE, x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum,
    z_wrap = TRUE, z_chrnum = .uwb_vals$chrnum,
    show_nsize_zzvar = FALSE, zz_wrap = TRUE, zz_chrnum = .uwb_vals$chrnum) {
  
  bat <- dat 
  
  # Add total as a pseudo-group before the main processing
    if (add_total) {
      total_rows <- groups |>
        mutate({{grvar}} := lab_total)  # Create duplicate rows with total label
      bat <- bind_rows(bat, total_rows)
    }
  
  bat <- bat |>
    select(c({{grvar}}, all_of(vars))) |>
    pivot_longer(cols = all_of(vars)) 

  if (drop_na) {
    bat = bat |> drop_na(value)
  }

    bat <- bat |>
    group_by(name, {{grvar}}) |>
    count(value,.drop = FALSE) |>
    mutate(
      yvar = n/sum(n)*100,
      xvar_df = name,
      zvar_df = value,
      nsize = sum(n),
      zzvar_df = {{grvar}},) |>
      filter(yvar > 0) 
  
  bat_try = try(bat |> mutate(xvar_df = lab), silent = TRUE) #try to rename items with lab from codebook
    if (!inherits(bat_try, "try-error")) bat <- bat_try
  
  bat <- bat |> 
    group_by(xvar_df, name) |> 
    polish_var(var_origin = "xvar_df", var_final = "xvar",
      wrap = x_wrap, chrnum = x_chrnum, nsize = show_nsize) |>
    polish_var(var_origin = "zvar_df", var_final = "zvar",
      wrap = z_wrap, chrnum = z_chrnum, nsize = FALSE) |>
    generate_textlabs() |>
    impute_labs() |>
    ungroup()

  bat <- bat |> 
    rename(nsize_x = nsize) |> 
    group_by(zzvar_df) |> 
    mutate(nsize = sum(n)) |> 
    polish_var(var_origin = "zzvar_df", var_final = "zzvar",
      wrap = zz_wrap, chrnum = zz_chrnum, nsize = show_nsize_zzvar) |>
    rename(nsize_z = nsize) |> 
    ungroup()

    return(bat)

}
