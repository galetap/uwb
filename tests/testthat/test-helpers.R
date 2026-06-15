# Tests for the internal helper functions:
#   generate_textlabs(), impute_labs(), polish_var()
# These are the building blocks used by every prep_*() / mean_*() function.

test_that("generate_textlabs adds the expected label and position columns", {
  out <- generate_textlabs(data.frame(yvar = c(5, 42, 0.3, 88)))

  expected_cols <- c(
    "labvar_full", "labvar_single", "pos_single",
    "cvar", "cvar_text", "labvar", "pos"
  )
  expect_true(all(expected_cols %in% names(out)))
  # one output row per input row
  expect_equal(nrow(out), 4)
})

test_that("generate_textlabs rounds labels and flags tiny values as '<1'", {
  out <- generate_textlabs(data.frame(yvar = c(0.3, 12.6)))

  # 0.3 < 0.5 -> shown as "<1"
  expect_equal(out$labvar_single[1], "<1")
  # 12.6 rounded to 0 places -> "13"
  expect_equal(out$labvar_full[2], 13)
  expect_equal(out$labvar_single[2], "13")
})

test_that("generate_textlabs honours round_places", {
  out <- generate_textlabs(data.frame(yvar = c(12.34, 56.78)), round_places = 1)
  expect_equal(out$labvar_full, c(12.3, 56.8))
})

test_that("impute_labs adds title/subtitle/caption columns", {
  out <- impute_labs(data.frame(name = c("num1", "num2")))
  expect_true(all(c("title", "subtitle", "caption") %in% names(out)))
  # row count is preserved
  expect_equal(nrow(out), 2)
})

test_that("polish_var produces an ordered factor in var_final", {
  d <- data.frame(
    xvar_df = c("B", "A", "C"),
    nsize   = c(100, 100, 100)
  )
  out <- polish_var(d, nsize = FALSE)

  expect_true("xvar" %in% names(out))
  expect_s3_class(out$xvar, "factor")
  # original ("as_factor") order is preserved: B, A, C
  expect_equal(levels(out$xvar), c("B", "A", "C"))
})

test_that("polish_var appends 'n = ...' to labels when nsize = TRUE", {
  d <- data.frame(
    xvar_df = c("Small", "Large"),
    nsize   = c(15, 320)
  )
  out <- polish_var(d, nsize = TRUE)
  # the n= annotation is embedded in the (html) factor labels
  expect_true(any(grepl("n = 15", as.character(out$xvar))))
  expect_true(any(grepl("n = 320", as.character(out$xvar))))
})

test_that("polish_var creates the renamed factor column via var_final", {
  d <- data.frame(grp = c("A", "B"), nsize = c(50, 60))
  out <- polish_var(d, var_origin = "grp", var_final = "myfac", nsize = FALSE)
  # the derived factor is written to the requested column name
  expect_true("myfac" %in% names(out))
  expect_s3_class(out$myfac, "factor")
})
