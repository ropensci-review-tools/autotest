---
created: 2026-07-29T15:55:00Z
agent: claude-sonnet-5
git_hash: 7081f595f474b88def9360da398b002e2b4098d7
---

# Design Decisions: merge-typetracer

## Summary
Completed the pivot from the yaml/example-text-parsing test pipeline to typetracer-based tracing (issue #76): the dead yaml pipeline was removed, mutation testing was scoped to example-derived traces only, untested-function/parameter warnings were reimplemented against trace data, and the full test suite plus `devtools::check()` were brought to a clean pass — surfacing and fixing several real bugs along the way.

## New Design Decisions

### Decision 1: Example vs. test-suite trace provenance drives mutation testing, not type inference
**Chosen:** `autotest_package()` reads typetracer's per-trace `trace_source` field to run mutation/fuzz testing only against example-sourced traces, while parameter type/class inference still draws on all traces (examples and test suite alike).
**Rationale:** Directly resolves the spurious-error problem from issue #76 without needing any typetracer-side API change — the CRAN release already provides `trace_source`.
**Tradeoffs:** None material.

### Decision 2: Yaml pipeline removed outright, no compatibility shim
**Chosen:** `autotest_yaml()`, `autotest_single_yaml()`, `parse_yaml_template()`, and their exclusively-private helpers were deleted along with their tests; `examples_to_yaml()`/`at_yaml_template()` remain as they're still independently useful/exported.
**Rationale:** Matches the design intent already established when the typetracer branch was started; a shim would mean maintaining two parallel test-generation pipelines.
**Tradeoffs:** Breaking change to the public API (`autotest_yaml` removed).

### Decision 3: Untested-parameter detection reimplemented against trace data, with a subtlety around defaults
**Chosen:** A parameter counts as "demonstrated" by an example only if its traced value has a non-`"NULL"` `par_uneval` — typetracer records an entry for every formal of a traced function, including ones left at their default, so naively treating "appears in a trace" as "was demonstrated" produced false negatives.
**Rationale:** Discovered via a concrete case (`stats::cov`'s `y` parameter, never explicitly passed in its own documented examples, was not being flagged).
**Tradeoffs:** Relies on a `par_uneval == "NULL"` heuristic that is ambiguous for the rare case of a parameter explicitly passed as literal `NULL`.

### Decision 4: Fix root-cause performance/correctness bugs found during validation, in-scope
**Chosen:** Full-suite validation runs against `stats` uncovered a multi-gigabyte memory blowup. Root-caused to two issues and fixed at the source: typetracer's `pre_install()` reinstalled a traced package from scratch on every call within a session (fixed with a session-scoped install cache, deliberately restricted to named/installed packages, not local source directories, to avoid a separate stale-reload risk that surfaced when the cache was first widened); and `int_upper_limit()`/`int_lower_limit()` probed unbounded integer parameters directly at `.Machine$integer.max`, which for allocation-sized parameters (e.g. `rnorm`'s `n`) attempted tens of gigabytes before any error could be caught (fixed with a much smaller `safe_int_probe_ceiling`).
**Rationale:** Both are usability/safety issues in code this stage's work exercises directly, not hypothetical; confirmed explicitly before proceeding given they extend beyond the original trace-provenance scope.
**Tradeoffs:** `int_upper_limit()`'s reported "unbounded" ceiling is now capped at ~1e7 (one order of magnitude past the new 1e6 probe) rather than truly unbounded — a deliberate safety/precision tradeoff.

### Decision 5: Traced package must be explicitly reloaded before mutation testing
**Chosen:** `autotest_package()` now re-calls `preload_package()` immediately after tracing completes.
**Rationale:** `typetracer::trace_package()` unloads the traced package's namespace once tracing finishes and does not reload it into the session; packages with other loaded dependents (e.g. `stats`) stay resident regardless, masking the gap, but a standalone package with nothing else depending on it unloads cleanly, leaving the subsequent mutation-testing loop unable to find its functions.

## Integration with Prior Work
Builds directly on the project's design history (see `specs/000-design-history/design-decisions.md`, Phase 4): this stage completes the typetracer integration that phase started, resolving the specific blocking concern (spurious test-suite-sourced errors) recorded in issue #76.

## Issues Resolved
- Issue #76 (spurious errors from test-suite-sourced traces): resolved via trace-provenance filtering.
- Functional parity gap vs. `main` for untested-function/parameter warnings: restored against trace data.
- Multi-gigabyte memory blowup and multi-minute stalls when exercising `autotest_package()` against real packages: resolved.

## Deferred Items
- The typetracer-side fix (install caching) was verified working but left uncommitted in the sibling `typetracer` repository, per explicit instruction; formalizing and releasing it there is a follow-up outside this repo.
- The pre-existing S3 generic/method signature warnings surfaced by `devtools::check()` predate this stage and were left untouched as out of scope.

## Process Notes
- Two rounds of "investigate further vs. work around" decision points arose during validation (the typetracer reinstall cost, and the integer-range probing strategy); both were escalated for explicit confirmation before widening scope, rather than assumed.
- Test expectations in three test files needed updating (not just fixing) to reflect that the trace-based pipeline is more faithful to documented example behaviour than the old static yaml parser was, in ways that change legitimate output (e.g. now correctly flagging a documented error case that the old pipeline missed inside a `try()` block).
