#' Mean scores of a battery by one grouping variable
#'
#' Computes item means within each level of `grvar`. The grouping variable
#' becomes the category axis (`xvar`, with `n=` appended) and the item name
#' becomes the secondary category (`zvar`). Meant for [plot_dodge()] or
#' [plot_lolli()].
#'
#' @param dat A data frame.
#' @param vars The numeric item columns.
#' @param grvar The grouping variable (unquoted column name).
#' @param round_places Number of decimals used for the rounded label. Default
#'   `0`.
#' @param add_total Append an overall group for comparison against the total?
#'   Default `FALSE`.
#' @param lab_total Label of the total group. Default `"ZCU"`.
#' @param na_lab Character values to treat as missing when parsing numbers.
#'   Default `c("NA", "Bez odpovedi")`.
#'
#' @returns A tibble with `yvar` (mean within group), `xvar`, `zvar`, and label
#'   columns.
#' @export
#'
#' @examples
#' mean_bat_gr(example_data,
#'             vars = c("num1", "num2", "num3", "num4", "num5"),
#'             grvar = fak)
#'
mean_bat_gr <- function(
    dat, vars, grvar, round_places = 0,
    add_total = FALSE, lab_total = "Z\u010cU",
    na_lab = c("NA", "Bez odpov\u011bd\u010fi"),
    show_nsize = TRUE, x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, 
    z_wrap = TRUE, z_chrnum = .uwb_vals$chrnum) {
    
  means = dat 
    
  # Add total as a pseudo-group before the main processing
  if (add_total) {
    total_rows <- means |>
      mutate({{grvar}} := lab_total)  # Create duplicate rows with total label
    means <- bind_rows(means, total_rows)
  }
    
  means = means |>
    select(c({{grvar}}, all_of(vars))) |>
    pivot_longer(cols = all_of(vars)) |>
    mutate(
      value_chr = as.character(value),
      value_chr = if_else(value_chr %in% na_lab, NA_character_, value_chr),
      value_num = parse_number(value_chr)) |>
    drop_na(value_num) |>
    group_by(name, {{grvar}}) |>
    summarise(
      yvar = mean(value_num, na.rm = T),
      nsize = n()) |>
    mutate(
      xvar_df = {{grvar}},
      zvar_df = name
    ) |>
    ungroup() |>
    impute_labs() |> 
    generate_textlabs(round_places = round_places) 

  means_try = try(means |> mutate(zvar_df = lab), silent = TRUE) #try to rename items with lab from codebook
  if (!inherits(means_try, "try-error")) means <- means_try

  means <- means |>
    polish_var(var_origin = "xvar_df", var_final = "xvar", wrap = x_wrap, chrnum = x_chrnum, nsize = show_nsize) |> 
    polish_var(var_origin = "zvar_df", var_final = "zvar", wrap = z_wrap, chrnum = z_chrnum, nsize = FALSE) 

  return(means)
  }
