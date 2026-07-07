# Suggesting improvements to `uwb`

We keep the process for collecting and handling improvements as simple as
possible, using only GitHub Issues.

> **Every idea or bug becomes a GitHub Issue. We work through the open Issues gradually.**

---

## 1. Have an idea or found a bug? Open an Issue

Open a **GitHub Issue** in the repo: <https://github.com/galetap/uwb/issues>

- **Title** — the idea in a few words (e.g. *"plot_bar should allow custom colors"*).
- **Body** — one or two sentences: what you want and why.

General rule: **if it isn't an Issue, it doesn't exist.** Don't keep ideas in
email or private notes — put them straight into an Issue so nothing gets lost.

## 2. Work through the Issues gradually

The list of **open Issues** is the to-do list. When there is time to work on the
package, pick an Issue that seems useful and solve it. 

## 3. Close the Issue when it's done

When an Issue is finished, **close it**. The easiest way is to reference it in
the commit message so GitHub closes it automatically:

```
git commit -m "Add custom colors to plot_bar. Closes #12"
```

The list of open Issues stays short, and closed Issues are the record of what
has been done.

---

## Development notes

### Vignettes must use the `knitr` engine, not `quarto`

The package vignettes are written as `.Rmd` files and built with the **`knitr`**
engine (`VignetteBuilder: knitr` in `DESCRIPTION`, and
`%\VignetteEngine{knitr::rmarkdown}` in each vignette header). **Do not switch
the vignettes back to `quarto` / `.qmd`.**

**Why.** When the package is installed with vignettes, `R CMD build` first 
installs the package into a temporary library and then
renders every vignette. The `quarto` engine renders each vignette in a 
separate R process that does not reliably have that temporary library on its
`.libPaths()`. As a result `library(uwb)` inside the vignette fails during the
build and the whole install aborts.

`knitr` renders vignettes in-process during `R CMD build`, so `library(uwb)`
always resolves to the package being built. This makes the one-step
`install_github(..., build_vignettes = TRUE)` work.

If you edit or add a vignette, keep the `.Rmd` + knitr header format:

```yaml
---
title: "Your title"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Your title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

### Data files must use a lowercase `.rda` extension

Datasets in `data/` must be saved as `name.rda` (e.g. `codebook.rda`), **not**
`name.Rda`. R's lazy-data mechanism only recognizes the extensions `.rda`,
`.RData`, and `.rdata`; a capitalized `.Rda` is silently ignored, so the object
never becomes available. Prefer `usethis::use_data(codebook, overwrite = TRUE)`,
which writes the file with the correct name and extension automatically.

### Don't delete `.gitignore` files in subfolders

The repository has more than one `.gitignore`: the root `.gitignore` plus a
folder-level one in `vignettes/` (and possibly others). **This is intentional —
do not delete the subfolder ones.**

Git supports multiple `.gitignore` files, and each one applies to the folder it
lives in (and its subfolders). The rules stack on top of the parent rules, and
patterns are interpreted relative to the file's own location.

The `vignettes/.gitignore` scopes patterns like `*.html`, `*.R`, and `*_files/`
to the `vignettes/` folder so that generated vignette build artifacts are
ignored without affecting your real package source. Those same patterns
could not live in the root `.gitignore` — a root-level `*.R` would ignore every
R source file in `R/`. Keeping them folder-scoped is why the separate file
exists.
