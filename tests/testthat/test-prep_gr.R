# Tests for prep_gr() and prep_gr2(): categorical variable by grouping variable(s).

test_that("prep_gr returns a tibble with xvar, zvar and yvar", {
  out <- prep_gr(example_data, typ, fak)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("xvar", "zvar", "yvar", "nsize") %in% names(out)))
})

test_that("prep_gr percentages sum to 100 within each group", {
  out <- prep_gr(example_data, typ, fak)

  # With grvar_to_x = TRUE (default), grvar (fak) is on xvar and percentages
  # of var (typ) sum to 100 within each fak group.
  sums <- tapply(out$yvar, out$xvar_df, sum)
  expect_true(all(abs(sums - 100) < 1e-6))
})

test_that("prep_gr add_total = TRUE adds a total pseudo-group", {
  base  <- prep_gr(example_data, typ, fak)
  total <- prep_gr(example_data, typ, fak, add_total = TRUE)

  # the total version has more distinct groups on the category axis
  expect_gt(dplyr::n_distinct(total$xvar_df), dplyr::n_distinct(base$xvar_df))
  # and the total label is present
  expect_true(any(grepl("\u010cU", as.character(total$xvar_df))))
})

test_that("prep_gr grvar_to_x = FALSE swaps the xvar / zvar roles", {
  to_x   <- prep_gr(example_data, typ, fak, grvar_to_x = TRUE)
  swap   <- prep_gr(example_data, typ, fak, grvar_to_x = FALSE)

  # when grvar_to_x = TRUE, xvar_df comes from grvar (fak)
  expect_setequal(as.character(unique(to_x$xvar_df)),
                  as.character(unique(example_data$fak)))
  # when grvar_to_x = FALSE, xvar_df comes from var (typ)
  expect_setequal(as.character(unique(swap$xvar_df)),
                  as.character(unique(example_data$typ)))
})

test_that("prep_gr2 returns a facet column (zzvar_df)", {
  out <- prep_gr2(example_data, typ, fak, pohlavi)
  expect_s3_class(out, "tbl_df")
  expect_true("zzvar_df" %in% names(out))
  expect_true(all(c("xvar", "zvar", "yvar") %in% names(out)))
})

test_that("prep_gr2 percentages sum to 100 within each group x facet", {
  out <- prep_gr2(example_data, typ, fak, pohlavi)
  sums <- tapply(out$yvar, interaction(out$xvar_df, out$zzvar_df, drop = TRUE), sum)
  expect_true(all(abs(sums - 100) < 1e-6))
})