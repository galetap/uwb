#' Prep MC data in long format
#'
#' total + grouped by faculty
#'
#' @param dat A data frame
#'
#' @returns A tibble
#' @export
#'
prep_mc_long_faks  <- function(dat) {

  groups_nsize = dat |>
    select(ID) |>
    unique() |>
    left_join(id_faks) |>
    count(fak) |>
    rename(nsize = n)

  groups = dat |>
    purrr::set_names(c("ID", "xvar")) |>
    left_join(id_faks) |>
    group_by(fak) |>
    count(xvar) |>
    mutate(zvar = fak) |>
    left_join(groups_nsize)

  mc = dat |>
    purrr::set_names(c("ID", "xvar")) |>
    count(xvar) |>
    mutate(nsize = length(unique(dat$ID)),
           zvar = "Z\u010cU") |>
    bind_rows(groups) |>
    mutate(yvar = n/nsize * 100,
           name = names(dat)[2]
    ) |>
    filter(yvar > 0) |>
    generate_textlabs() |>
    impute_labs()
}
