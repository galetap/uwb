# Tests

This package uses [`testthat`](https://testthat.r-lib.org/) (edition 3) for
automated testing. Tests live in `tests/testthat/` with one `test-*.R` file per
group of related functions.

## What is currently covered

The first testing increment covers the **data-preparation and helper
functions** (the deterministic, tibble-producing part of the package):

| Test file              | Functions covered                                  |
| ---------------------- | -------------------------------------------------- |
| `test-helpers.R`       | `generate_textlabs()`, `impute_labs()`, `polish_var()` |
| `test-prep_single.R`   | `prep_single()`                                    |
| `test-prep_gr.R`       | `prep_gr()`, `prep_gr2()`                          |
| `test-prep_mc.R`       | `prep_mc()`, `prep_mc_gr()`                        |
| `test-prep_bat.R`      | `prep_bat()`, `prep_bat_gr()`                      |
| `test-mean_bat.R`      | `mean_bat()`, `mean_bat_gr()`, `mean_bat_gr2()`    |
| `test-plots.R`         | `plot_bar()`, `plot_lolli()`, `plot_stack()`, `plot_dodge()`, `plot_trend()`, `plot_trend_gr()` (smoke tests) |

Tests use the bundled `example_data` dataset as a fixture and assert on concrete
output: expected columns, row counts, percentages summing to 100 (within groups
where relevant), and means/counts matching hand-computed values.

The plot tests are **smoke tests**: they confirm each `plot_*()` function runs
without error and returns a `ggplot` object (using the matching `prep_*()` /
`mean_*()` output as input). They do **not** check the visual appearance of the
plots. `plot_ribbon()` and `plot_out()` are currently **skipped** — they are
tightly coupled to specific prepared-data shapes (ggsankey internals; a literal
`fak` column and an atomic `zvar_df`) and need dedicated fixtures before they
can be smoke-tested reliably.

## How to run the tests

From an R session at the package root:

```r
# run the whole test suite while developing
devtools::test()

# full package check (runs tests + examples + documentation) before merging
devtools::check()
```

If you only installed `testthat` (not the full `devtools`), use:

```r
testthat::test_local()
```

Or from the command line:

```sh
Rscript -e "devtools::test('.')"
```

## Future increments (not yet implemented)

- **Smoke tests for the `plot_*()` functions** — confirm each returns a `ggplot`
  object and runs without error.
- **Tests for the colour/scale utilities** (`scale_fill_uwb()`,
  `scale_color_uwb()`, `uwb_palettes()`, `uwb_scales()`, `uwb_vals()`).
- **Sanity checks for the bundled data** (`example_data`, `codebook`).
- **(Optional) `vdiffr` visual snapshot tests** for the plots.
- **(Optional) GitHub Actions CI** to run the tests automatically on every push
  and pull request (`usethis::use_github_action("check-standard")`).

> Note: `prep_mc_long_faks()` is intentionally not tested yet — it depends on an
> external `id_faks` object and an `ID` column that are not present in the
> bundled `example_data`.