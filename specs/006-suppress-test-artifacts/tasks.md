---
created: 2026-07-30T14:30:00Z
agent: claude-sonnet-5
git_hash: 0c702721cdd64161a5bdefc50570018c59290ee2
---

# Tasks: suppress-test-artifacts

## T006-1: Add shared null-device helper
- [ ] T006-1: In `R/capture-fn-output.R` (or a new `R/utils.R` if that reads cleaner), add
  an internal helper `with_null_device(expr)` that opens `grDevices::pdf(nullfile())`,
  registers `on.exit(grDevices::dev.off(), add = TRUE)`, then evaluates and returns `expr`.
  Document with `#' @noRd`. No exported API changes.

## T006-2: Wrap example-scraping execution with the null device
- [ ] T006-2: In `R/example-objects.R::example_objects()`, wrap the existing
  `tryCatch(source(tmp, echo = FALSE, local = env, max.deparse.length = Inf), error = ...)`
  call (~line 137) with `with_null_device()` from T006-1, so any plot triggered by a
  package's own documented example code (which runs unconditionally, regardless of
  `test = TRUE/FALSE`) is discarded instead of reaching whatever device is currently active.

## T006-3: Wrap mutation-test execution with the null device
- [ ] T006-3: In `R/capture-fn-output.R::log_all_msgs()`, wrap the existing
  `eval(call(this_fn), envir = en)` / `do.call(this_fn, params, quote = TRUE)` branch
  (~lines 17-23) with `with_null_device()` from T006-1. This is the single choke point
  reached via `catch_all_msgs()` from all 9 other call sites, so no other file needs
  changing for this part.

## T006-4: Add knitr-aware progress-bar suppression
- [ ] T006-4: In `R/autotest-functions.R`, where `progress == "bar"` currently triggers
  `cli::cli_progress_bar()`/`cli::cli_progress_update()`/`cli::cli_progress_done()`
  (~lines 114-146), add a check at the top of `autotest_package()`: if
  `progress == "bar"` and `isTRUE(getOption("knitr.in.progress"))`, reset
  `progress <- "none"` before the rest of the function runs. Empirically confirm (via an
  actual `rmarkdown::render()` invocation, not just reasoning) that this option is TRUE
  during this project's `make knitr`/`vignettes/makefile knitr` targets before relying on
  it as the detection mechanism; if it is not reliably set, fall back to checking
  `!cli::is_dynamic_tty()` instead, or both.

## T006-5: Audit for other arbitrary-code-execution sites
- [ ] T006-5: Search the rest of the codebase (beyond `example_objects()` and
  `log_all_msgs()`) for any other place that evaluates a target package's own code
  (`source()`, `eval()`, `do.call()`, `Rd2ex()`-derived scripts, `donttest`/`dontrun`
  handling, etc.) and could similarly trigger stray graphics-device output. Wrap any
  additional site found with `with_null_device()` from T006-1, or explicitly document in
  a code comment why it's safe to leave unwrapped.

## T006-6: Remove dead fig.path from both vignettes
- [ ] T006-6: Remove the `fig.path = "README-"` line from the `knitr::opts_chunk$set()`
  call in both `vignettes/autotest.Rmd` (~line 27) and `vignettes/autotest-control.Rmd`
  (~line 29), since T006-2/T006-3 mean no plots reach any device during these chunks'
  `autotest_package()` calls, making this copy-pasted, misleadingly-named option dead
  configuration. Let knitr use its own per-file default fig.path instead.

## T006-7: Harden root makefile clean target
- [ ] T006-7: Update the `clean:` target in the root `makefile` (currently
  `rm -rf *.html *.png README_cache docs/`) to also remove `Rplots.pdf` as a defensive
  backstop, in case any future example/test code still manages to produce one despite
  T006-2/T006-3.

## T006-8: Harden vignettes makefile clean target
- [ ] T006-8: Update the `clean:` target in `vignettes/makefile` (currently
  `rm -rf *.html *.png`) to also remove `*.md` and `Rplots.pdf`, since the `knitr` target
  in that same makefile renders `*.md` files that have never been meant to be tracked
  (confirmed via `git ls-files vignettes/`).

## T006-9: Ignore vignette-rendered markdown
- [ ] T006-9: Add a `.gitignore` entry (e.g. `vignettes/*.md`) so `vignettes/autotest.md`
  and `vignettes/autotest-control.md` — pure local-preview byproducts of
  `vignettes/makefile`'s `knitr` target — are never accidentally staged or flagged as
  untracked clutter again.

## T006-10: Verify and re-render a clean README
- [ ] T006-10: Delete the 9 currently-untracked `README-stats-var-*.png` files from the
  repo root. With T006-1 through T006-4 in place, run `make knitr` from the repo root and
  confirm: (a) no new PNG/PDF files appear afterward, (b) the rendered `README.md` contains
  no literal ANSI/escape-sequence noise (e.g. no `[K` sequences), and (c) the diff against
  the currently-committed `README.md` reflects only the intended, clean re-render. Commit
  the resulting `README.md`.

## T006-11: Add regression test coverage
- [ ] T006-11: Extend `tests/testthat/test-statspkg.R` (which already exercises
  `autotest_package(test = FALSE/TRUE, progress = "tests")` against `stats::var`) with
  assertions that `grDevices::dev.list()` gains no new open devices and that no
  `Rplots.pdf` file appears in the test working directory after each call. Add a small
  additional test that calls `autotest_package()` with `progress = "bar"` under a mocked
  `options(knitr.in.progress = TRUE)` and asserts it falls back to non-bar (silent)
  behavior per T006-4.
