#' Batteries data prep
#'
#' Dat is wide data, it is pivoted longer inside the function
#'
#' @param dat A data frame in a wide format
#' @param vars A variable
#' @param drop_na Drop NA or not?
#' @param x_wrap DNK
#' @param x_chrnum DNK
#' @param x_nsize DNK
#'
#' @returns A tibble
#' @export
#'
prep_bat  <-
  function(dat, vars = names(dat), drop_na = TRUE,
           x_wrap = TRUE, x_chrnum = .uwb_vals$chrnum, x_nsize = FALSE) {

  bat = dat |>
    pivot_longer(cols = all_of(vars))

  if (drop_na) {
    bat = bat |> drop_na(value)
  }

  bat = bat |>
    group_by(name) |>
    count(value,.drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           xvar_df = name,
           zvar = value,
           nsize = sum(n)) |>
    filter(yvar > 0) |>
    generate_textlabs() |>
    impute_labs() |>
    ungroup()

  bat = try(bat |> mutate(xvar_df = lab), silent = T) #try to rename items with lab from codebook
  bat = bat |>
    polish_var(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize)

  return(bat)

}
