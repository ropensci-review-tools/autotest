---
created: 2026-07-29T15:55:00Z
agent: claude-sonnet-5
git_hash: 7081f595f474b88def9360da398b002e2b4098d7
---

# Design Decisions: autotest

## Current Architecture
`autotest` automatically tests an R package's functions by tracing calls
made in its documented examples (and, for local source packages, its own
test suite) with the `typetracer` package, then running a family of
S3-dispatched mutation/fuzz tests against each traced call. Parameter
type/class information is inferred from all traces (examples and test
suite alike); actual mutation testing — and the untested-function/
untested-parameter warnings that report documentation coverage gaps — is
driven only by example-sourced traces, so that a package's own test suite
(which may deliberately trigger errors) cannot produce spurious autotest
failures. Results are returned as a `tibble`-derived `autotest_package`
object, one row per finding, with `type` in `error`/`warning`/
`diagnostic`/`message`/`dummy`.

## Key Decisions

### YAML as an early intermediate representation, later removed
**Outcome:** The original design normalized documented parameter
constraints into a YAML schema (`parse_yaml`, `examples_to_yaml`) as a
buffer between fragile Rd-parsing and the test-generation logic. This
pipeline was fully removed once typetracer-based tracing replaced it as
the source of parameter information; `examples_to_yaml()`/
`at_yaml_template()` remain as independently useful exported utilities,
but nothing downstream depends on parsing yaml back into R objects anymore.
**Rationale:** YAML decoupled documentation-parsing from test generation
early on, but static text-parsing of example code proved to be exactly
the "miles of code" that motivated switching to runtime type tracing.
**Roads not taken:** A compatibility shim keeping `autotest_yaml()`
working alongside the trace-based pipeline was considered and rejected —
maintaining two parallel pipelines indefinitely was judged not worth it
once the pivot was confirmed intentional.
**Stages:** 000 (design history), 001 (removal)

### S3 class/generic dispatch as the core extensibility mechanism
**Outcome:** The `autotest_obj` class and per-input-type S3 method
families (`test_single_*`, `test_rect_*`, `test_return_*`, etc.) remain
the mechanism for adding new parameter/test types, unchanged by the
typetracer migration — trace data is adapted into the same `autotest_obj`
shape the mutation-testing methods already expect.
**Rationale:** Established early (see stage 000) as the extensibility
point; the typetracer migration deliberately preserved this boundary so
only the *input* to test generation changed, not the dispatch mechanism.
**Roads not taken:** N/A — no serious alternative considered during the
typetracer migration; the boundary was kept stable by design.
**Stages:** 000 (established), 001 (preserved across the pipeline swap)

### Trace provenance (example vs. test-suite) gates mutation testing
**Outcome:** `typetracer` tags each trace with its source; `autotest`
uses that to ensure only example-derived calls are ever used to drive
fuzz/mutation testing, resolving a spurious-error problem that motivated
the typetracer migration in the first place (calls from within a
package's own test suite, some of which are deliberately written to
trigger errors, would otherwise appear as false autotest failures).
**Rationale:** Directly resolves the concern raised when the typetracer
migration was first proposed, without requiring any change to typetracer
itself — the needed field was already present in its CRAN release.
**Roads not taken:** Filtering by trace call-environment (`call_env`)
rather than the coarser `trace_source` field was considered and found
unnecessary — the source-level distinction is sufficient.
**Stages:** 001

### Root-cause fixes over workarounds for issues surfaced during validation
**Outcome:** Multi-gigabyte memory blowups discovered while validating
the typetracer migration were traced to two genuine bugs — typetracer
reinstalling a traced package from scratch on every call within a
session, and an integer-range test probing `.Machine$integer.max`
directly for parameters that control allocation size — and both were
fixed at the source rather than avoided in test code.
**Rationale:** These are real usability/safety issues for any consumer
of `autotest_package()`, not artifacts of the migration's test suite.
**Roads not taken:** Isolating each `autotest_package()` call in its own
subprocess, or simply avoiding the specific functions/packages that
triggered the issue in tests, were both considered and rejected in favor
of fixing the underlying cause.
**Stages:** 001

## Architectural Evolution
The project began (2020) as a documentation-driven, static text-parsing
system: scrape examples, convert to YAML, generate tests from the parsed
structure. That architecture matured through 2021 with an S3 class
hierarchy (`autotest_obj`) that remains the system's backbone today. After
a long low-activity maintenance period (2022–2025), 2026 brought a
deliberate architectural pivot (issue #76): replacing static example
parsing with runtime type tracing via the sibling `typetracer` package.
That pivot is now complete — the yaml pipeline has been fully removed,
trace provenance is used to keep mutation testing scoped to documented
examples, and the migration's validation work surfaced and fixed several
pre-existing performance/correctness issues that the old pipeline's
lighter execution model had never exercised.

## Important Roads Not Taken
- **Compatibility shim for the yaml pipeline** (stage 001): rejected in
  favor of a clean removal, since the pivot to typetracer was already the
  established direction, not a competing option.
- **Timeout-based guard for the integer-range probing fix** (stage 001):
  considered but rejected in favor of capping the probed value directly,
  since R's interrupt checks may not reliably preempt a tight C-level
  allocation loop — a timeout could fail to actually prevent the
  allocation it's meant to guard against.
- **Unrestricted session-level install caching in typetracer** (stage
  001): an initial fix cached every traced package's reinstall,
  including local source directories; this was narrowed to
  named/installed packages only after it was found to risk stale/corrupt
  reads for local packages whose source may change between calls.
