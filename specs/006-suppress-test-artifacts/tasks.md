---
created: 2026-07-30T14:30:00Z
agent: claude-sonnet-5
git_hash: 0c702721cdd64161a5bdefc50570018c59290ee2
---

# Tasks: suppress-test-artifacts

## T006-1: Add shared null-device helper
- [x] T006-1: In `R/capture-fn-output.R` (or a new `R/utils.R` if that reads cleaner), add
  an internal helper `with_null_device(expr)` that opens `grDevices::pdf(nullfile())`,
  registers `on.exit(grDevices::dev.off(), add = TRUE)`, then evaluates and returns `expr`.
  Document with `#' @noRd`. No exported API changes.

## T006-2: Wrap example-scraping execution with the null device
- [x] T006-2: In `R/example-objects.R::example_objects()`, wrap the existing
  `tryCatch(source(tmp, echo = FALSE, local = env, max.deparse.length = Inf), error = ...)`
  call (~line 137) with `with_null_device()` from T006-1, so any plot triggered by a
  package's own documented example code (which runs unconditionally, regardless of
  `test = TRUE/FALSE`) is discarded instead of reaching whatever device is currently active.

## T006-3: Wrap mutation-test execution with the null device
- [x] T006-3: In `R/capture-fn-output.R::log_all_msgs()`, wrap the existing
  `eval(call(this_fn), envir = en)` / `do.call(this_fn, params, quote = TRUE)` branch
  (~lines 17-23) with `with_null_device()` from T006-1. This is the single choke point
  reached via `catch_all_msgs()` from all 9 other call sites, so no other file needs
  changing for this part.

## T006-4: Add knitr-aware progress-bar suppression
- [x] T006-4: In `R/autotest-functions.R`, where `progress == "bar"` currently triggers
  `cli::cli_progress_bar()`/`cli::cli_progress_update()`/`cli::cli_progress_done()`
  (~lines 114-146), add a check at the top of `autotest_package()`: if
  `progress == "bar"` and `isTRUE(getOption("knitr.in.progress"))`, reset
  `progress <- "none"` before the rest of the function runs. Empirically confirm (via an
  actual `rmarkdown::render()` invocation, not just reasoning) that this option is TRUE
  during this project's `make knitr`/`vignettes/makefile knitr` targets before relying on
  it as the detection mechanism; if it is not reliably set, fall back to checking
  `!cli::is_dynamic_tty()` instead, or both.

## T006-5: Audit for other arbitrary-code-execution sites
- [x] T006-5: Search the rest of the codebase (beyond `example_objects()` and
  `log_all_msgs()`) for any other place that evaluates a target package's own code
  (`source()`, `eval()`, `do.call()`, `Rd2ex()`-derived scripts, `donttest`/`dontrun`
  handling, etc.) and could similarly trigger stray graphics-device output. Wrap any
  additional site found with `with_null_device()` from T006-1, or explicitly document in
  a code comment why it's safe to leave unwrapped.

  Found and wrapped 5 additional direct `do.call (x$fn, ...)` invocation sites that
  bypass `catch_all_msgs()`/`log_all_msgs()` entirely: `R/input-double.R::double_noise()`
  (2 call sites), `R/input-int.R` (2 call sites, integer-noise comparison), and
  `R/test-return-object.R::capture_return_object()` and `R/test-rect-fns.R` (2 call
  sites: rectangular-class conversion and class-extension checks). All other
  `do.call`/`eval`/`parse` occurrences found by the audit (e.g. `do.call (rbind, ...)`,
  `do.call (paste0 (...), list (1))`, `do.call (typetracer::trace_package, args)`) invoke
  fixed internal utility functions rather than arbitrary target-package code, so were
  left unwrapped.

## T006-6: Remove dead fig.path from both vignettes
- [x] T006-6: Remove the `fig.path = "README-"` line from the `knitr::opts_chunk$set()`
  call in both `vignettes/autotest.Rmd` (~line 27) and `vignettes/autotest-control.Rmd`
  (~line 29), since T006-2/T006-3 mean no plots reach any device during these chunks'
  `autotest_package()` calls, making this copy-pasted, misleadingly-named option dead
  configuration. Let knitr use its own per-file default fig.path instead.

## T006-7: Harden root makefile clean target
- [x] T006-7: Update the `clean:` target in the root `makefile` (currently
  `rm -rf *.html *.png README_cache docs/`) to also remove `Rplots.pdf` as a defensive
  backstop, in case any future example/test code still manages to produce one despite
  T006-2/T006-3.

## T006-8: Harden vignettes makefile clean target
- [x] T006-8: Update the `clean:` target in `vignettes/makefile` (currently
  `rm -rf *.html *.png`) to also remove `*.md` and `Rplots.pdf`, since the `knitr` target
  in that same makefile renders `*.md` files that have never been meant to be tracked
  (confirmed via `git ls-files vignettes/`).

## T006-9: Ignore vignette-rendered markdown
- [x] T006-9: Add a `.gitignore` entry (e.g. `vignettes/*.md`) so `vignettes/autotest.md`
  and `vignettes/autotest-control.md` — pure local-preview byproducts of
  `vignettes/makefile`'s `knitr` target — are never accidentally staged or flagged as
  untracked clutter again.

## T006-10: Verify and re-render a clean README
- [x] T006-10: Delete the 9 currently-untracked `README-stats-var-*.png` files from the
  repo root. With T006-1 through T006-4 in place, run `make knitr` from the repo root and
  confirm: (a) no new PNG/PDF files appear afterward, (b) the rendered `README.md` contains
  no literal ANSI/escape-sequence noise (e.g. no `[K` sequences), and (c) the diff against
  the currently-committed `README.md` reflects only the intended, clean re-render. Commit
  the resulting `README.md`.

  First re-render surfaced a real bug: `grDevices::nullfile` doesn't exist (`nullfile()` is
  a base-R function, not exported from `grDevices`) -- fixed in `with_null_device()`. Second
  re-render still produced all 9 PNGs; traced this to the actual root cause: the plotting
  example code runs inside `typetracer::trace_package()` (external dependency, called via
  `do.call()` in `R/typetrace-package.R`), which evaluates every documented example
  in-process via `eval(parse(text = ex))`. Its own `options(device = NULL)` plot-suppression
  attempt only prevents a *new* device from auto-opening -- it has no effect when a device
  (e.g. knitr's per-chunk recording device) is already active, which is always the case
  during a knit. Wrapped the `do.call(typetracer::trace_package, args)` call site itself
  with `with_null_device()` (no need to touch the external package's source), which fixed
  it. Confirmed via re-render: zero PNG/PDF files left afterward, no `[K`/ANSI noise in
  `README.md`, and the tibble row counts/messages exactly match the pre-fix noisy render
  (21/11/3 rows), confirming no unintended change to actual test output/content.

  `vignettes/` could not be fully end-to-end verified in this environment: `make knitr`
  there fails at an unrelated, pre-existing chunk (`DT::datatable()` via `webshot`/PhantomJS,
  "webshot.js returned failure value: 1" -- a sandbox/headless-browser limitation, not
  related to this fix) before reaching the vignette's later `autotest_package(test = TRUE)`
  chunks. Confirmed no PNG/PDF artifacts were left by the point reached (through a completed
  `autotest_package()` call), which uses the identical code path just verified end-to-end
  via the root README.

  Per repo-wide git safety convention (never commit without explicit user instruction), the
  resulting clean `README.md` and all code changes are left staged/uncommitted for the
  user's own review rather than auto-committed here.

## T006-11: Add regression test coverage
- [x] T006-11: Extend `tests/testthat/test-statspkg.R` (which already exercises
  `autotest_package(test = FALSE/TRUE, progress = "tests")` against `stats::var`) with
  assertions that `grDevices::dev.list()` gains no new open devices and that no
  `Rplots.pdf` file appears in the test working directory after each call. Add a small
  additional test that calls `autotest_package()` with `progress = "bar"` under a mocked
  `options(knitr.in.progress = TRUE)` and asserts it falls back to non-bar (silent)
  behavior per T006-4.

  Added the device/Rplots.pdf assertions to the existing "autotest var" test, and a new
  "autotest_package progress falls back under knitr" test (captures message-stream output
  and asserts no progress-bar/ANSI patterns appear, rather than `expect_silent()`, since
  `var`/`cor` example code legitimately prints other console output unrelated to progress).

  This surfaced a real, positive side effect worth recording: this environment has no
  active/default graphics device at all (`plot()` errors with "no active or default
  device" outside of any knitr/RStudio context), so *before* this stage's fix, any package
  example that called a plotting function part-way through would silently abort there,
  truncating every subsequent call in that example from being traced at all. With the
  fix, such examples now run to completion. This changed the existing "autotest rnorm"
  test's behaviour: `stats::rnorm` is traced from many more example files than before and
  now passes all its tests cleanly (`autotest_package()` legitimately returns `NULL`,
  which is its documented "success" value), which exposed a latent bug in that test's own
  assertion -- `nrow(x0) > nrow(x)` silently evaluates to `logical(0)` when `x` is `NULL`
  (since `nrow(NULL)` is `NULL`, not `0`), rather than failing loudly. Fixed by using
  `NROW(x)` (which correctly returns `0` for `NULL`) instead of `nrow(x)`.

  Full `tests/testthat` suite run to confirm no other regressions (see follow-up note).
