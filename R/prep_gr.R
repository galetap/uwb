#' Grouped data prep with or without total
#'
#' var becomes zvar, grvar becomes xvar, it can be switched by grvar_to_x = FALSE
#'
#' @param dat A data frame
#' @param var A variable
#' @param grvar A group variable
#' @param drop_na Drop NA or not?
#' @param add_total T/F
#' @param lab_total DNK
#' @param grvar_to_x DNK
#' @param show_nsize DNK
#' @param x_wrap DNK
#' @param x_chrnum DNK
#' @param z_wrap DNK
#' @param z_chrnum DNK
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' prep_gr(ggplot2::mpg, class, cyl)
#'
prep_gr <-
  function(dat, var, grvar, drop_na = TRUE, add_total = FALSE, lab_total = "Z\u010cU",
           grvar_to_x = TRUE, show_nsize = TRUE,
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
      ungroup() |>
      group_by(xvar_df) |>
      polish_var(var_origin = "xvar_df", var_final = "xvar",
                      wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize) |>
      polish_var(var_origin = "zvar_df", var_final = "zvar",
                      wrap = z_wrap, chrnum = z_chrnum, nsize = z_nsize) |>
      generate_textlabs() |>
      impute_labs() |>
      ungroup()

    return(groups)
  }
