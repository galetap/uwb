# Tests for mean_bat(), mean_bat_gr() and mean_bat_gr2(): item means.

num_vars <- c("num1", "num2", "num3", "num4", "num5")

test_that("mean_bat returns one row per item with yvar = mean", {
  out <- mean_bat(example_data, vars = num_vars)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), length(num_vars))
  expect_true(all(c("xvar", "yvar", "nsize") %in% names(out)))
})

test_that("mean_bat yvar equals the arithmetic mean of each item", {
  out <- mean_bat(example_data, vars = num_vars)

  expected_num1 <- mean(example_data$num1, na.rm = TRUE)
  got_num1 <- out$yvar[out$name == "num1"]
  expect_equal(got_num1, expected_num1)
})

test_that("mean_bat nsize equals the count of non-missing values", {
  out <- mean_bat(example_data, vars = num_vars)
  expected_n <- sum(!is.na(example_data$num1))
  got_n <- out$nsize[out$name == "num1"]
  expect_equal(got_n, expected_n)
})

test_that("mean_bat treats na_lab strings as missing", {
  d <- data.frame(item = c("5", "7", "no answer", "9"))
  out <- mean_bat(d, vars = "item", na_lab = "no answer")
  # the "no answer" value is dropped, leaving the mean of 5, 7, 9
  expect_equal(out$yvar, mean(c(5, 7, 9)))
  expect_equal(out$nsize, 3)
})

test_that("mean_bat_gr returns means grouped by grvar", {
  out <- mean_bat_gr(example_data, vars = num_vars, grvar = fak)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "zvar", "yvar", "nsize") %in% names(out)))
  # one row per item x distinct (non-missing) group, derived from the data
  n_groups <- dplyr::n_distinct(tidyr::drop_na(example_data, fak)$fak)
  expect_equal(nrow(out), length(num_vars) * n_groups)
  expect_gt(nrow(out), 0)
})

test_that("mean_bat_gr yvar matches a hand-computed group mean", {
  out <- mean_bat_gr(example_data, vars = num_vars, grvar = fak)
  # pick the first group actually present in the data (don't hard-code a label)
  g <- as.character(out$xvar_df[1])
  expected <- mean(example_data$num1[as.character(example_data$fak) == g],
                   na.rm = TRUE)
  got <- out$yvar[out$name == "num1" & as.character(out$xvar_df) == g]
  expect_equal(got, expected)
})

test_that("mean_bat_gr2 returns means for two grouping variables", {
  out <- mean_bat_gr2(example_data, vars = num_vars,
                      grvar = fak, grvar2 = pohlavi)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "zvar", "yvar", "nsize") %in% names(out)))
})