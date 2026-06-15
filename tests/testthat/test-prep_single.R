# Tests for prep_single(): single categorical variable -> percentages.

test_that("prep_single returns a tibble with the expected columns", {
  out <- prep_single(example_data, fak)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "yvar", "n", "nsize", "subtitle") %in% names(out)))
})

test_that("prep_single percentages sum to 100", {
  out <- prep_single(example_data, fak)
  expect_equal(sum(out$yvar), 100)
})

test_that("prep_single returns one row per category", {
  out <- prep_single(example_data, fak)
  # one row per distinct (non-missing) category of fak, derived from the data
  expected_n <- dplyr::n_distinct(tidyr::drop_na(example_data, fak)$fak)
  expect_equal(nrow(out), expected_n)
  expect_gt(nrow(out), 0)
})

test_that("prep_single counts add up to the (non-missing) sample size", {
  out <- prep_single(example_data, fak)
  expect_equal(sum(out$n), out$nsize[1])
})

test_that("prep_single order = TRUE sorts categories by descending frequency", {
  out <- prep_single(example_data, fak, order = TRUE)
  # xvar factor levels should be ordered so that counts are descending
  counts_in_level_order <- out$n[match(levels(out$xvar), as.character(out$xvar))]
  expect_false(is.unsorted(rev(counts_in_level_order)))
})

test_that("prep_single subtitle reports the sample size", {
  out <- prep_single(example_data, fak)
  expect_equal(out$subtitle[1], paste0("N=", out$nsize[1]))
})