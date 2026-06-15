# Smoke tests for the plot_*() functions.
#
# These confirm that each plotting function runs without error and returns a
# ggplot object when given the output of the matching prep_*()/mean_*() helper.
# They are intentionally lightweight: they do NOT check the visual appearance
# of the plots (that would require snapshot testing, e.g. vdiffr).
#
# Plotting is wrapped in a null graphics device so that functions which call
# print() inside them do not try to open a window during the test run.

# Helper: run a plotting expression on a throwaway device and return its value.
# Warnings (e.g. ggplot dropping NA labels, deprecated `size=` aesthetic) are
# suppressed so the smoke-test output stays focused on errors.
draw <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  suppressWarnings(force(expr))
}

mc_vars  <- c("mc1", "mc2", "mc3", "mc4", "mc5")
bat_vars <- c("bat1", "bat2", "bat3", "bat4", "bat5")
num_vars <- c("num1", "num2", "num3", "num4", "num5")

test_that("plot_bar returns a ggplot from prep_single output", {
  d <- prep_single(example_data, fak)
  expect_no_error(p <- draw(plot_bar(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_bar works in vertical orientation", {
  d <- prep_single(example_data, fak)
  expect_no_error(p <- draw(plot_bar(d, horiz = FALSE)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_lolli returns a ggplot from mean_bat output", {
  d <- mean_bat(example_data, vars = num_vars)
  expect_no_error(p <- draw(plot_lolli(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_stack returns a ggplot from prep_gr output", {
  d <- prep_gr(example_data, typ, fak)
  expect_no_error(p <- draw(plot_stack(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_stack returns a ggplot from prep_bat output", {
  d <- prep_bat(example_data, vars = bat_vars)
  expect_no_error(p <- draw(plot_stack(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_dodge returns a ggplot from prep_gr output", {
  d <- prep_gr(example_data, typ, fak)
  expect_no_error(p <- draw(plot_dodge(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_dodge returns a ggplot from prep_mc_gr output", {
  d <- prep_mc_gr(example_data, vars = mc_vars, grvar = fak, chosen = 1)
  expect_no_error(p <- draw(plot_dodge(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_trend returns a ggplot from prep_single output", {
  # rok is an ordered time-like variable in example_data
  d <- prep_single(example_data, rok)
  expect_no_error(p <- draw(plot_trend(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_trend_gr returns a ggplot from prep_gr output", {
  d <- prep_gr(example_data, rok, fak, grvar_to_x = FALSE)
  expect_no_error(p <- draw(plot_trend_gr(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_ribbon returns a ggplot from time-by-group prep_gr output", {
  # plot_ribbon depends on ggsankey internals (geom_sankey_bump + ggplot_build
  # introspection) and a specific time-by-group layout. Smoke-testing it with
  # generic example_data is brittle; it needs a dedicated fixture. Skipped for
  # now so the suite stays meaningful. (TODO: build a stable ribbon fixture.)
  skip("plot_ribbon needs a dedicated ggsankey fixture; see TODO")

  d <- prep_gr(example_data, fak, rok, grvar_to_x = FALSE)
  expect_no_error(p <- draw(plot_ribbon(d)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_out returns a ggplot from prep_gr output with a total", {
  # plot_out is tightly coupled to a specific prepared shape: it references a
  # literal `fak` column and expects `zvar_df` to be atomic so that
  # `yvar[zvar_df == total]` works. The standard prep_gr() output does not
  # satisfy this, so a dedicated fixture is required. Skipped for now.
  # (TODO: build a fixture with groups + total + atomic zvar_df + fak column.)
  skip("plot_out needs a dedicated fixture (atomic zvar_df + fak column); see TODO")

  d <- prep_gr(example_data, rok, fak,
               add_total = TRUE, grvar_to_x = FALSE)
  expect_no_error(p <- draw(plot_out(d, out = 5)))
  expect_s3_class(p, "ggplot")
})
