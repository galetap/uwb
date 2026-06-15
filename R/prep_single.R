#' Prepare percentage data of single var
#'
#' @param dat A data frame
#' @param var A variable
#' @param order Should the results be ordered?
#' @param drop_na Drop NA or not?
#'
#' @returns A tibble with enhanced frequency table.
#' @export
#'
#' @examples
#' prep_single(ggplot2::mpg, cyl)
#'
prep_single  <-
  function(dat, var, order = FALSE, drop_na = TRUE){
    single = dat
    if (drop_na) {
      single = single |> tidyr::drop_na({{var}})
    }
    single = single |>
      count({{var}},.drop = TRUE) |>
      mutate(yvar = n/sum(n) * 100,
             xvar = as_factor({{var}}),
             nsize = sum(n),
             nsize_raw = nrow(dat),
             nsize_listwise = nrow(dat |> drop_na({{var}})),
             name = names(dat |> select({{var}}))
      ) |>
      filter(yvar > 0) |>
      generate_textlabs() |>
      impute_labs()
    if (order) {
      single = single |> mutate(xvar = fct_reorder(xvar, -n))
    }
    single = single |>
      mutate(subtitle = paste0("N=", nsize))
    return(single)
  }
