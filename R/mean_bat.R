#' Mean scores of a battery of numeric items
#'
#' Computes the mean (and sample size) of each numeric item in `vars`, returning
#' `yvar` as the mean instead of a percentage. Values are parsed with
#' [readr::parse_number()], treating the strings in `na_lab` as missing. Meant
#' for [plot_lolli()] or [plot_bar()].
#'
#' @param dat A data frame.
#' @param vars The numeric item columns.
#' @param round_places Number of decimals used for the rounded label
#'   (`labvar`). Default `0`.
#' @param na_lab Character values to treat as missing when parsing numbers.
#'   Default `c("NA", "Bez odpovedi")`.
#' @param order Reorder items by frequency? Default `FALSE`.
#' @param show_n Show sample size? Default `TRUE`. The information is placed into subtitle 
#' (if identical for all items) or as inside the xvar (item-specific).

#' @returns A tibble with one row per item (`yvar` = mean, `nsize` = sample size,
#'   `xvar` = item, plus label columns).
#' @export
#'
#' @examples
#' mean_bat(example_data, vars = c("num1", "num2", "num3", "num4", "num5"))
#'

mean_bat <-
  function(dat, vars, round_places = 0,
    na_lab = c("NA", "Bez odpov\u011bd\u010fi"), order = FALSE, 
    x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, show_n = TRUE) {
    means <- dat |>
    select(all_of(vars)) |>
    pivot_longer(cols = all_of(vars)) |>
    mutate(value_chr = as.character(value),
           value_chr = if_else(value_chr %in% na_lab, NA_character_, value_chr),
           value_num = parse_number(value_chr)) |>
    drop_na(value_num) |>
    group_by(name) |>
    summarise(yvar = mean(value_num, na.rm = T),
              nsize = n()) |>
    mutate(xvar_df = name) |>
    generate_textlabs(round_places = round_places) |>
    impute_labs() 
    
    ns <- means$nsize |> unique()

    # If show_n: identical N across all items → subtitle once; varying N → per-item label
    if (show_n) {
      if (length(ns) == 1) {
        means <- means |> dplyr::mutate(subtitle = paste0("N=", ns))
        nsize_arg <- FALSE
      } else {
        nsize_arg <- TRUE
      }
    } else {
      nsize_arg <- FALSE
    }

    if ("lab" %in% names(means)) {
      means <- means |> dplyr::mutate(xvar_df = lab)
    }

    if (order) {
      means <- means |>
        dplyr::mutate(xvar_df = forcats::fct_reorder(as.character(xvar_df), -yvar))
    }

    means <- means |>
      polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = nsize_arg) |>
      dplyr::ungroup()

    means
  }
