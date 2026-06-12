# Public-facing objects exported by the uwb package.
# Values are defined directly here (not as aliases of sysdata.rda internals)
# so they are available at byte-compile time.
# uwb_vals is the exception: it is wired up in .onLoad (R/zzz.R) so that
# the public name shares the same environment as the internal .uwb_vals,
# preserving reference semantics for user modifications.


# Faculty names ----------------------------------------------------------------

#' Faculty codes and names
#'
#' A named character vector mapping short faculty codes to their full Czech
#' names. The names of the vector are the short codes used in data (e.g.
#' `"FAV"`); the values are the full names used as plot labels. The order
#' follows alphabetical order of the codes, matching [c_faks].
#'
#' @format A named character vector of length 9.
#' @seealso [parts], [c_faks]
#' @export
faks <- c(
  FAV  = "Fakulta aplikovan\u00fdch v\u011bd",
  FDU  = "Fakulta designu a um\u011bn\u00ed Ladislava Sutnara",
  FEK  = "Fakulta ekonomick\u00e1",
  FEL  = "Fakulta elektrotechnick\u00e1",
  FF   = "Fakulta filozofick\u00e1",
  FPE  = "Fakulta pedagogick\u00e1",
  FPR  = "Fakulta pr\u00e1vnick\u00e1",
  FST  = "Fakulta strojn\u00ed",
  FZS  = "Fakulta zdravotnick\u00fdch studi\u00ed"
)


#' Non-faculty institute codes and names
#'
#' A named character vector mapping short codes for non-faculty university
#' institutes to their full names. The names of the vector are the short codes
#' used in data; the values are the full names used as plot labels.
#'
#' @format A named character vector.
#' @seealso [faks], [c_parts]
#' @export
parts <- c(
  NTC  = "N\u00e1rodn\u00ed technologick\u00e9 centrum",
  UJP  = "\u00dastav jazykov\u00e9 p\u0159\u00edpravy",
  RTI  = "Regiontech Innovation",
  RICE = "V\u00fdzkumn\u00fd a inovac\u010dn\u00ed centrum",
  NTIS = "N\u00e1rodn\u00ed technologick\u00e1 infrastruktura pro spole\u010dnost"
)


# Colors -----------------------------------------------------------------------

#' Official university color
#'
#' The official dark-blue color of the University of West Bohemia as a hex
#' string. Used as the default single color in plots.
#'
#' @format A character scalar (hex color string).
#' @seealso [c_faks], [c_parts]
#' @export
c_zcu <- "#31539D"


#' Official faculty colors
#'
#' A named character vector with the official colors of the nine faculties, in
#' the same alphabetical order as [faks]. Can be used directly with
#' `scale_fill_manual(values = c_faks)` to assign official colors to faculties.
#'
#' @format A named character vector of length 9 (hex color strings).
#' @seealso [faks], [c_parts], [c_zcu]
#' @export
c_faks <- c(
  FAV = "#CEAA1B",
  FDU = "#C7362D",
  FEK = "#D67C1C",
  FEL = "#293D83",
  FF  = "#5DB3DA",
  FPE = "#99BC39",
  FPR = "#8A172E",
  FST = "#4C8CCB",
  FZS = "#1C966A"
)


#' Official non-faculty institute colors
#'
#' A named character vector with the official colors of the non-faculty
#' university institutes, in the same order as [parts].
#'
#' @format A named character vector (hex color strings).
#' @seealso [parts], [c_faks], [c_zcu]
#' @export
c_parts <- c(
  NTC  = "#80217E",
  UJP  = "#31539D",
  RTI  = "#31539D",
  RICE = "#CEAA1B",
  NTIS = "#31539D"
)


# Visual settings --------------------------------------------------------------

#' Visual settings for uwb plots
#'
#' An environment storing default values that control the visual appearance of
#' themes and plots — font family, text sizes, line widths, point sizes, and
#' fixed colors used outside aesthetic mappings. The `plot_*()` functions and
#' `theme_uwb()` read these values internally, so changing a value in
#' `uwb_vals` affects all subsequent plots without any additional arguments.
#'
#' @format An environment. See `as.list(uwb_vals)` for all elements.
#' @examples
#' as.list(uwb_vals)
#' # Change base text size globally:
#' # uwb_vals$tsize <- 12
#' @export
uwb_vals <- NULL  # wired to .uwb_vals in .onLoad so reference semantics are preserved


#' Built-in color scale definitions
#'
#' A named list of color hex vectors defining the built-in color scales. Used
#' internally by [scale_fill_uwb()] and [scale_color_uwb()], but can also be
#' inspected or used directly to build custom color sequences.
#'
#' @format A named list of character vectors (hex color strings). Available
#' scales: `quali`, `faks`, `uwb_faks`, `mono`, `mono_rev`, `monored`,
#' `monored_rev`, `long`, `long_rev`, `bi`, `bi_rev`.
#' @seealso [scale_fill_uwb()], [scale_color_uwb()], [uwb_palettes()]
#' @export
uwb_scales <- NULL  # wired to .uwb_scales in .onLoad