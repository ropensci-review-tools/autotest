---
created: 2026-07-30T11:27:52Z
agent: claude-sonnet-5
git_hash: c0e5114644814f853f64e820a5c12851bed87ca8
---

# Tasks: fix-progress-reporting

## T004-1: Replace autotest_package()'s quiet parameter with progress
- [x] T004-1: In `R/autotest-functions.R`, change `autotest_package()`'s signature from `quiet = FALSE` to `progress = c("bar", "tests", "none")`, resolved at the top of the function body via `progress <- match.arg(progress)`. Remove all other references to the old `quiet` parameter within this function's own body (the hardcoded `quiet = TRUE` argument passed to the internal `autotest_single_trace()` call stays completely unchanged — it is unrelated to this parameter and must not be touched).

## T004-2: Fix the progress denominator to count only example-sourced traces
- [x] T004-2: In `R/autotest-functions.R`, replace the current loop (`for (i in seq_along (trace_files))` which reads each file with `readRDS()` inside the loop and does `next` for non-`"examples"`-sourced traces) with: read all `trace_files` once via `trace_data_all <- lapply(trace_files, readRDS)`, filter via `trace_data_all <- trace_data_all[vapply(trace_data_all, function(d) identical(d$trace_source, "examples"), logical(1))]`, and use `n_total <- length(trace_data_all)` as the corrected total. Loop over `seq_along(trace_data_all)` instead, using `trace_data <- trace_data_all[[i]]` in place of the old `readRDS(trace_files[i])` call, removing the now-unneeded `next`/source check from inside the loop.

## T004-3: Implement progress = "bar" using a real cli progress bar
- [x] T004-3: In `R/autotest-functions.R`, before the (now-corrected) loop, add `if (progress == "bar") cli::cli_progress_bar(name = "Testing functions", total = n_total)`. Inside the loop, after the `autotest_single_trace()` call, add `if (progress == "bar") cli::cli_progress_update()`. After the loop, add `if (progress == "bar") cli::cli_progress_done()`.

## T004-4: Keep progress = "tests" as the current tick-per-trace behaviour, with the corrected total
- [x] T004-4: In the same loop, change the existing `if (!quiet) { message(cli::col_green(cli::symbol$tick, " [", i, " / ", length(trace_files), "]")) }` block to `if (progress == "tests") { message(cli::col_green(cli::symbol$tick, " [", i, " / ", n_total, "]")) }`, using the corrected `n_total` from T004-2 in place of the old `length(trace_files)`. `progress == "none"` should result in no output at all inside the loop (neither the bar update from T004-3 nor this message).

## T004-5: Update autotest_package()'s roxygen documentation
- [x] T004-5: In `R/autotest-functions.R`, replace the `#' @param quiet If 'FALSE', provide printed output on screen.` roxygen line (around line 26, in `autotest_package()`'s own doc block — not the identically-worded one in `autotest_single_trace()`'s doc block a few dozen lines below, which must stay untouched since that's a different, unrelated function) with a `#' @param progress` entry describing all three values: `"bar"` (default, a `cli` progress bar), `"tests"` (one tick line per function tested, showing `[i / n]`), and `"none"` (no progress output; equivalent to the old `quiet = TRUE`).

## T004-6: Regenerate/hand-edit man/autotest_package.Rd to match
- [x] T004-6: Update `man/autotest_package.Rd` to match the roxygen change from T004-5: replace the `quiet = FALSE` entry in the `\usage{}` block with `progress = c("bar", "tests", "none")`, and replace the `\item{quiet}{...}` entry with `\item{progress}{...}` documenting all three values, consistent with how prior stages in this repo have hand-edited `.Rd` files to match roxygen changes without requiring a full `roxygen2::roxygenise()` pass.

## T004-7: Update the four expect_message(autotest_package(...)) tests
- [x] T004-7: In `tests/testthat/test-statspkg.R`, add `progress = "tests"` as an explicit argument to each of the four `autotest_package(package = package, functions = functions, test = FALSE)` / `test = TRUE` calls that are wrapped in `expect_message(...)` (two in the `"autotest var"` test, two in the `"autotest rnorm"` test). This is required because `cli::cli_progress_bar()` (the new default, `progress = "bar"`) does not emit an R `message()` condition in a non-interactive context, so these tests would otherwise silently fail to observe any message.

## T004-8: Verify visually against geodist for all three progress values
- [x] T004-8: Run `autotest_package("../../hypertidy/geodist")` (mirroring `script.R`) three times, once with each of `progress = "bar"`, `progress = "tests"`, and `progress = "none"`. Confirm: `"bar"` shows an actual `cli` progress bar sized to the corrected total (32 for geodist, not 173); `"tests"` shows `✔ [i / 32]` ticks (not `/ 173`); `"none"` shows no per-trace progress output (acknowledging that unrelated messages from `preload_package()` and `typetracer::trace_package()`, e.g. "Loading geodist", are not suppressed by this parameter and were never suppressed by the old `quiet` parameter either).

## T004-9: Run the full test suite and confirm no other quiet= callers exist
- [x] T004-9: Run the complete test suite (`devtools::test()` or equivalent) and confirm no failures. Also re-confirm via `grep -rn "autotest_package" R/ tests/ vignettes/ 2>/dev/null` that no other file (besides `test-statspkg.R`, handled in T004-7) calls `autotest_package(..., quiet = ...)`, since that argument no longer exists and would now error with "unused argument".
