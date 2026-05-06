#' Impute labs from codebook
#'
#' Internal data prep functions (used in multiple data prep procedures)
#'
#' @param dat A data frame
#'
#' @returns A data frame
#' @export
#'
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
