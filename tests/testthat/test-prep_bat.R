# Tests for prep_bat() and prep_bat_gr(): batteries of shared-scale items.

bat_vars <- c("bat1", "bat2", "bat3", "bat4", "bat5")

test_that("prep_bat returns xvar (item), zvar (response) and yvar", {
  out <- prep_bat(example_data, vars = bat_vars)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "zvar", "yvar", "nsize") %in% names(out)))
})

test_that("prep_bat percentages sum to 100 within each item", {
  out <- prep_bat(example_data, vars = bat_vars)
  sums <- tapply(out$yvar, out$xvar_df, sum)
  expect_true(all(abs(sums - 100) < 1e-6))
})

test_that("prep_bat has one row per item x response option", {
  out <- prep_bat(example_data, vars = bat_vars)
  # every battery item should appear in the output
  expect_setequal(unique(out$name), bat_vars)
  # rows = items x observed response options; non-empty and a multiple of items
  expect_gt(nrow(out), 0)
  expect_equal(nrow(out) %% length(bat_vars), 0)
})

test_that("prep_bat_gr keeps the grouping variable for faceting", {
  out <- prep_bat_gr(example_data, vars = bat_vars, grvar = pohlavi)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "zvar", "yvar", "zzvarno") %in% names(out)))
})

test_that("prep_bat_gr percentages sum to 100 within each item x group", {
  out <- prep_bat_gr(example_data, vars = bat_vars, grvar = pohlavi)
  sums <- tapply(out$yvar, interaction(out$xvar, out$zzvarno, drop = TRUE), sum)
  expect_true(all(abs(sums - 100) < 1e-6))
})

test_that("prep_bat_gr add_total = TRUE appends an overall group", {
  base  <- prep_bat_gr(example_data, vars = bat_vars, grvar = pohlavi)
  total <- prep_bat_gr(example_data, vars = bat_vars, grvar = pohlavi,
                       add_total = TRUE)
  expect_gt(dplyr::n_distinct(total$zzvarno), dplyr::n_distinct(base$zzvarno))
})