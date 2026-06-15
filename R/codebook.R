#' Example codebook
#'
#' A data frame providing plot labels for the [example_data] dataset. Used to
#' demonstrate the codebook workflow in the `uwb` package: when a `codebook`
#' object is present in the calling environment, the `prep_*()` functions
#' automatically import `title`, `subtitle`, `caption`, and `lab` from it.
#'
#' @format A data frame with one row per variable in [example_data] and the following columns:
#' \describe{
#'   \item{name}{Variable name as it appears in the dataset; used to match rows to plot data}
#'   \item{label}{Full variable label or question wording}
#'   \item{lab}{Short item label for battery and multiple choice variables; `NA` for other variables}
#'   \item{title}{Plot title text}
#'   \item{subtitle}{Sample description (e.g. "UWB graduates, N = 450")}
#'   \item{caption}{Footnote text}
#' }
#' @seealso [example_data] for the matching survey dataset; [impute_labs()] for the function that reads from a codebook.
"codebook"