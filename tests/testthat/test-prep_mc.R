# Tests for prep_mc() and prep_mc_gr(): multiple-choice (0/1) item blocks.

mc_vars <- c("mc1", "mc2", "mc3", "mc4", "mc5")

test_that("prep_mc returns one row per item", {
  out <- prep_mc(example_data, vars = mc_vars, chosen = 1)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), length(mc_vars))
})

test_that("prep_mc keeps only the 'chosen' value", {
  out <- prep_mc(example_data, vars = mc_vars, chosen = 1)
  expect_true(all(out$value == 1))
})

test_that("prep_mc yvar equals the observed share choosing each item", {
  out <- prep_mc(example_data, vars = mc_vars, chosen = 1)

  # Compare against a hand-computed percentage for mc1.
  expected_mc1 <- mean(example_data$mc1 == 1) * 100
  got_mc1 <- out$yvar[out$name == "mc1"]
  expect_equal(got_mc1, expected_mc1)
})

test_that("prep_mc has the expected plot-ready columns", {
  out <- prep_mc(example_data, vars = mc_vars, chosen = 1)
  expect_true(all(c("xvar", "yvar", "nsize") %in% names(out)))
})

test_that("prep_mc_gr returns percentages grouped by grvar", {
  out <- prep_mc_gr(example_data, vars = mc_vars, grvar = fak, chosen = 1)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "zvar", "yvar") %in% names(out)))
  expect_true(all(out$value == 1))
})

test_that("prep_mc_gr covers every item and every group", {
  out <- prep_mc_gr(example_data, vars = mc_vars, grvar = fak, chosen = 1)
  # all items present
  expect_setequal(unique(out$name), mc_vars)
  # all faculty groups present on the category axis
  expect_setequal(as.character(unique(out$xvar_df)),
                  as.character(unique(example_data$fak)))
  expect_gt(nrow(out), 0)
})
