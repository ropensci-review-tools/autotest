---
created: 2026-07-30T11:18:54Z
agent: claude-sonnet-5
git_hash: c0e5114644814f853f64e820a5c12851bed87ca8
---

# Plan: fix-progress-reporting

## Overview
`script.R` runs `autotest_package("../../hypertidy/geodist")` and shows progress output like `✔ [2 / 173]` throughout, but far fewer than 173 lines are ever printed. Root-caused to `R/autotest-functions.R`'s `autotest_package()` loop, which reports progress against the count of *all* trace files (both example- and test-sourced) while silently skipping test-sourced ones before printing anything. Add a `progress` parameter (`"bar"` default, `"tests"` current behaviour, `"none"`), which fully replaces the existing `quiet` parameter, fix the denominator, and implement a real `cli` progress bar for the new default.

## Context
`autotest_package()` (stage 001, `specs/001-merge-typetracer/`) traces a package's examples and test suite together, but per Decision 1 of that stage, mutation/fuzz testing is driven only by example-sourced traces — test-sourced traces exist only to enrich parameter type/class information. The progress-reporting loop (`R/autotest-functions.R:90-121`) was never updated to reflect this split when it was introduced: it lists `trace_files` (all traces, both sources) and loops over all of them, but does `next` (skipping silently, no output) for any trace whose `trace_data$trace_source` is not `"examples"` — before the point where it prints `message(cli::col_green(cli::symbol$tick, " [", i, " / ", length(trace_files), "]"))`.

Confirmed by direct reproduction against `geodist`: `length(trace_files)` is 173 (32 example-sourced + 141 test-sourced), but only 32 ticks are ever printed — matching the reported symptom exactly.

### quiet → progress = "none": full scope check
The user directed that `progress = "none"` should fully replace `quiet` in `autotest_package()`, rather than the two co-existing as orthogonal parameters as originally drafted. A full-codebase check confirms this is safe and self-contained:
- Within `autotest_package()` itself, `quiet` currently has exactly one effect: gating the per-trace tick `message()` (line 115-120) — the same block this stage is already rewriting. It is *not* threaded through to anything else in this function's own body.
- `autotest_package()`'s internal call to `autotest_single_trace()` (line 111) already hardcodes `quiet = TRUE` unconditionally — it does **not** currently forward its own `quiet` argument there at all. So removing `autotest_package()`'s `quiet` parameter changes nothing about that call.
- `autotest_single_trace()` (internal, `@noRd`) and `autotest_obj()` (exported, `@export`) each have their own, independent `quiet` parameters, unrelated to this change. `autotest_single_trace()` passes its `quiet` value into `autotest_obj(quiet = quiet)`, which stores it as a class attribute (`x$quiet`) — confirmed via `grep` that this stored attribute is never read anywhere else in the package, so it currently only affects `autotest_single_trace()`'s own per-function-name message (a *different* message from the one this stage is fixing). These two functions' `quiet` parameters are **out of scope** and remain completely unchanged.

## Design Goals
1. Add a `progress` parameter to `autotest_package()` with values `"bar"` (new default), `"tests"` (preserves the current one-line-per-trace tick behaviour), and `"none"` (full silence, replacing the old `quiet = TRUE`), validated via `match.arg()`. **Remove the `quiet` parameter from `autotest_package()`'s signature entirely** — this is a breaking API change to a documented, exported function.
2. Fix the total/denominator for both `"bar"` and `"tests"` modes: it must reflect only the traces actually processed (example-sourced), not the full `trace_files` count. Achieved by reading all trace files once up front, filtering to `trace_source == "examples"`, and looping only over that filtered list — avoiding the double-I/O that would result from filtering inside the existing per-file-read loop.
3. Implement `progress = "bar"` using `cli::cli_progress_bar()`/`cli::cli_progress_update()`/`cli::cli_progress_done()`, sized to the corrected total from Goal 2. `cli` is already an Imports dependency (`DESCRIPTION`); no new dependency needed.
4. `progress = "tests"` keeps the current `message(cli::col_green(cli::symbol$tick, " [", i, " / ", n, "]"))` per-trace output, just with the corrected `n`.
5. `progress = "none"` suppresses this progress display entirely, exactly matching old `quiet = TRUE`'s effect within this function — no more, no less (see "Non-goal" below).
6. Update the four existing `expect_message(autotest_package(...))` assertions in `tests/testthat/test-statspkg.R` to pass `progress = "tests"` explicitly. Confirmed by direct testing that `cli::cli_progress_bar()` does not emit an R `message()` condition in a non-interactive/test context (it renders directly to the terminal/ANSI, conditionally on `cli::is_dynamic_tty()`), so leaving those tests on the new "bar" default would silently break them (`expect_message()` would fail to observe any message).
7. Update `autotest_package()`'s roxygen documentation and `man/autotest_package.Rd` to remove `@param quiet` and document `@param progress` (all three values).
8. **Non-goal / known limitation carried forward unchanged:** `progress = "none"` (like the old `quiet = TRUE`) only suppresses `autotest_package()`'s own per-trace progress display. It does not, and never did, suppress unrelated messages from `preload_package()` (e.g. the "Loading geodist" message) or from `typetracer::trace_package()` itself (package-install messages, "Tests can not be traced with testthat tests run in parallel..." warnings, etc.) — those were never gated by `autotest_package()`'s `quiet` parameter even before this change. Achieving true total silence across those sibling calls is out of scope for this stage.
9. Scope stays limited to `autotest`'s own progress-reporting code in `R/autotest-functions.R` and its tests — no changes to the sibling `typetracer` package's own printed output, and no changes to `autotest_single_trace()` or `autotest_obj()`'s own, independent `quiet` parameters (confirmed unaffected — see Context above).

## Proposed Approach
1. In `R/autotest-functions.R`, replace `autotest_package()`'s `quiet = FALSE` parameter with `progress = c("bar", "tests", "none")`, resolved via `match.arg(progress)`.
2. Replace the per-file-read-and-skip loop (lines ~90-121) with: read all `trace_files` once via `lapply(trace_files, readRDS)`, filter to entries where `trace_source == "examples"`, and use the filtered list's length as `n_total`.
3. Before the loop: if `progress == "bar"`, call `cli::cli_progress_bar(name = "Testing functions", total = n_total)` (exact format string to be finalized during implementation).
4. Inside the loop (now iterating only over the pre-filtered example trace data): call `autotest_single_trace()` as before (its own hardcoded `quiet = TRUE` argument is untouched); then, depending on `progress`: `"bar"` → `cli::cli_progress_update()`; `"tests"` → the existing tick `message()` with the corrected `i`/`n_total`; `"none"` → nothing.
5. After the loop: if `progress == "bar"`, `cli::cli_progress_done()`.
6. Update `tests/testthat/test-statspkg.R`'s four `expect_message(autotest_package(...))` calls to pass `progress = "tests"`.
7. Update roxygen docs (remove `@param quiet`, add `@param progress`) for `autotest_package()`, and regenerate/hand-edit `man/autotest_package.Rd` to match.
8. Verify visually against `geodist` (`script.R`'s own case) for all three `progress` values, confirming: `"bar"` shows a real progress bar sized to 32 (not 173); `"tests"` shows corrected `[i / 32]` ticks; `"none"` shows no progress output at all (modulo the known, unchanged limitation re: `preload_package()`/`typetracer` messages noted above).
9. Run the full test suite to confirm no regressions, and confirm no other file references `autotest_package(..., quiet = ...)` (none found in a full-repo check during planning).

## Open Questions
- Exact `cli::cli_progress_bar()` format string/label wording — left to implementation-time judgement, not a design blocker.
