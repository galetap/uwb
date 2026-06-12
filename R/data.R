#' Example survey dataset
#'
#' A data frame used to demonstrate the data preparation and plotting
#' functions of the `uwb` package. Rows are respondents; columns cover
#' the main variable types used by the package's `prep_*()` and `plot_*()`
#' functions.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{fak}{Faculty (factor, single choice)}
#'   \item{rok}{Academic year (factor, single choice)}
#'   \item{pohlavi}{Gender (factor, single choice)}
#'   \item{typ}{Type of study (factor, single choice)}
#'   \item{num1, num2, num3, num4, num5}{Numeric ratings (e.g. satisfaction on a 0–10 scale)}
#'   \item{mc1, mc2, mc3, mc4, mc5}{Multiple choice items (0/1 coded: 1 = chosen)}
#'   \item{bat1, bat2, bat3, bat4, bat5}{Battery items with identical ordered response options (e.g. agree / neutral / disagree)}
#' }
#' @seealso [codebook] for matching plot labels; [prep_single()], [prep_bat()], [prep_mc()] for data preparation functions.
"data"