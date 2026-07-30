---
created: 2026-07-30T11:47:00Z
agent: claude-sonnet-5
git_hash: 13ef3dc5c5b8e67aa5ec44ba762de523b66ab070
---

# Design Decisions: fix-progress-reporting

## Summary
Fixed `autotest_package()`'s progress reporting, which showed ticks against a denominator counting all trace files (both example- and test-sourced) while only ever printing output for the example-sourced subset actually processed. Added a `progress` parameter (`"bar"` default, `"tests"`, `"none"`) that fully replaces the previous `quiet` parameter, implemented a real `cli` progress bar, and corrected the total in both display modes.

## New Design Decisions

### Decision 1: progress = c("bar", "tests", "none") fully replaces quiet
**Chosen:** `autotest_package()`'s `quiet` parameter is removed; `progress` takes over its role via the `"none"` value, in addition to the new `"bar"` default and the `"tests"` mode preserving prior tick-per-trace output.
**Rationale:** A full-codebase check confirmed `quiet`, within `autotest_package()`, only ever gated the one message this stage rewrites — its call to `autotest_single_trace()` already hardcoded `quiet = TRUE` unconditionally. `autotest_single_trace()`'s own `quiet` parameter and the exported `autotest_obj()`'s `quiet` parameter are separate, independent APIs confirmed unaffected (the `autotest_obj` class attribute it populates is never read downstream).
**Tradeoffs:** Breaking change to a documented, exported parameter. `progress = "none"` does not suppress unrelated `preload_package()`/`typetracer::trace_package()` messages — a pre-existing limitation of `quiet = TRUE` too, carried forward unchanged.

### Decision 2: Correct denominator via up-front filtering
**Chosen:** All trace files are read once and filtered to `trace_source == "examples"` before the mutation-testing loop runs, rather than filtering (and silently skipping) inside the loop as before.
**Rationale:** Gives both `"bar"` and `"tests"` display modes an accurate total without introducing extra file I/O.
**Tradeoffs:** None material.

### Decision 3: cli::cli_progress_bar() as the new default display
**Chosen:** `progress = "bar"` uses `cli::cli_progress_bar()`/`cli::cli_progress_update()`/`cli::cli_progress_done()`, sized to the corrected total. `cli` was already an Imports dependency.
**Rationale:** Matches the user's explicit request for an actual progress bar as the new default, replacing the previous one-line-per-trace tick output.
**Tradeoffs:** `cli::cli_progress_bar()` does not emit an R `message()` condition in non-interactive contexts (confirmed by direct testing), so any test asserting on message output from `autotest_package()` must now explicitly request `progress = "tests"`.

## Integration with Prior Work
Builds on stage 001's trace-provenance filtering (example- vs. test-sourced traces): this stage's fix depends on that same `trace_source` field already being populated on every trace, just applying the distinction earlier (before the loop) rather than mid-loop.

## Issues Resolved
- `autotest_package()` reporting progress against an inflated total (`script.R`'s reported symptom against `geodist`: 173 shown vs. 32 actually processed): resolved and verified end-to-end for all three `progress` values.

## Deferred Items
None.

## Process Notes
- The plan initially proposed keeping `quiet` and the new `progress` parameter as two orthogonal switches; the user directed consolidating them (`progress = "none"` replacing `quiet` outright) before implementation began, which required an explicit full-codebase safety check (documented in the plan's Context section) before proceeding.
