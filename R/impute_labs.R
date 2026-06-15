#' Import plot labels from the codebook
#'
#' Auxiliary helper used by the `prep_*()` and `mean_*()` functions. Looks for a
#' `codebook` object in the environment and joins its `title`, `subtitle`,
#' `caption`, and `lab` columns onto `dat` by the `name` column. If no `codebook`
#' is found, those columns are added as empty strings.
#'
#' @param dat A data frame with a `name` column.
#'
#' @returns The input data frame with `title`, `subtitle`, `caption` (and `lab`
#'   when a codebook is present) columns added.
#' @export
#'
#' @examples
#' impute_labs(data.frame(name = c("num1", "num2")))
#'
impute_labs  <- function(dat) {
  if (exists("codebook")) {
    dat = dat |> left_join(codebook)
  }
  else {
    dat = dat |>
      mutate(title = "",
             subtitle = "",
             caption = "")
  }
}
