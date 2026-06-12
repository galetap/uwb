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

# Install the package using R CMD INSTALL (no devtools needed)
result <- system(
  paste0('"', file.path(R.home("bin"), "R"), '" CMD INSTALL "',
         normalizePath("d:/USERS/marsti/Documents/GitHub/uwb"), '"')
)

if (result == 0) {
  cat("\nPackage installed successfully.\n")
} else {
  cat("\nInstallation failed with exit code:", result, "\n")
}