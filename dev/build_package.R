# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

setwd("d:/USERS/marsti/Documents/GitHub/uwb")

# Install roxygen2 if not available
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}

# Regenerate documentation
roxygen2::roxygenise()
cat("\nDocumentation regenerated.\n")

# Build a tarball (this also builds vignettes) then install from it.
# R CMD INSTALL on a source directory skips vignette building.
pkg_dir  <- normalizePath("d:/USERS/marsti/Documents/GitHub/uwb")
build_result <- system(
  paste0('"', file.path(R.home("bin"), "R"), '" CMD build "', pkg_dir, '"')
)

if (build_result != 0) {
  cat("\nBuild failed with exit code:", build_result, "\n")
  stop("Aborting.")
}

# Find the tarball just created (most recent .tar.gz in working dir)
tarball <- sort(Sys.glob("uwb_*.tar.gz"), decreasing = TRUE)[1]
cat("\nInstalling from:", tarball, "\n")

result <- system(
  paste0('"', file.path(R.home("bin"), "R"), '" CMD INSTALL "', tarball, '"')
)

if (result == 0) {
  cat("\nPackage installed successfully.\n")
} else {
  cat("\nInstallation failed with exit code:", result, "\n")
}
