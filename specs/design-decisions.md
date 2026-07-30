---
created: 2026-07-30T12:12:00Z
agent: claude-sonnet-5
git_hash: e23c47590345d1509db01a803148fcd932a944c4
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

### YAML as an early intermediate representation, later fully removed
**Outcome:** The original design normalized documented parameter
constraints into a YAML schema (`parse_yaml`, `examples_to_yaml`) as a
buffer between fragile Rd-parsing and the test-generation logic. This
pipeline was fully removed once typetracer-based tracing replaced it as
the source of parameter information. What first looked like independently
useful exported utilities (`examples_to_yaml()`, `at_yaml_template()`)
turned out, on closer audit, to have no live consumer either — both were
removed, along with the yaml-era example-text-scraping pipeline
(`R/scrape-examples.R`) that fed them and its cascading dependents. No
yaml-authoring, yaml-generation, or yaml-era example-scraping code remains
anywhere in the package.
**Rationale:** YAML decoupled documentation-parsing from test generation
early on, but static text-parsing of example code proved to be exactly
the "miles of code" that motivated switching to runtime type tracing.
**Roads not taken:** A compatibility shim keeping `autotest_yaml()`
working alongside the trace-based pipeline was considered and rejected —
maintaining two parallel pipelines indefinitely was judged not worth it
once the pivot was confirmed intentional.
**Stages:** 000 (design history), 001 (initial removal), 002 (completed
removal of remaining orphans)

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

### Static call-graph audits must be confirmed by actual execution
**Outcome:** When removing dead code left over from the yaml pipeline,
`grep`-based confirmation that a symbol has no external callers is
necessary but not sufficient — verification now also requires
`devtools::load_all()`, the full test suite, and a direct
`autotest_package()` smoke test.
**Rationale:** A file-level deletion during stage 002 removed a function
(`preload_package()`) that was actually called live from
`autotest_package()` itself, breaking the core pipeline; this was only
caught by attempting to run `autotest_package()` directly, not by static
analysis of caller references.
**Roads not taken:** Relying on `grep`/static analysis alone was the
initial approach and was found insufficient in practice.
**Stages:** 002

### A single suspected bug can mask several related ones — verify end to end, not fix-and-assume
**Outcome:** `autotest_package()` reported spurious test failures against
`pkgstats` that `testthat::test_local()` did not. Planning identified one
root cause in `typetracer`'s tracer-injection header (mishandling
namespace-qualified `pkg::fn(...)` calls); fixing only that left the
real-world reproduction still failing. Continuing to re-run the actual
`autotest_package()` vs. `testthat::test_local()` comparison after each
fix — rather than stopping once the originally-planned fix was in place —
surfaced three further, related defects in the same header code (an unset
session option in subprocess contexts, a raw function value as call head,
and an independent recurrence of the same call-head assumption in a
different function), all specific to patterns `pkgstats` genuinely uses
(parallel workers, `callr::r_bg()` subprocesses).
**Rationale:** The plan's own stated verification goal (re-run the actual
target package, not just unit tests) is what caught this; had verification
stopped at "the planned fix now passes typetracer's own test suite," three
real defects would have shipped undetected.
**Roads not taken:** Declaring the fix complete after the first defect was
resolved, since it matched the originally-diagnosed root cause, was
briefly the working assumption at each stage until re-verification against
the real target package disproved it.
**Stages:** 003

## Architectural Evolution
The project began (2020) as a documentation-driven, static text-parsing
system: scrape examples, convert to YAML, generate tests from the parsed
structure. That architecture matured through 2021 with an S3 class
hierarchy (`autotest_obj`) that remains the system's backbone today. After
a long low-activity maintenance period (2022–2025), 2026 brought a
deliberate architectural pivot (issue #76): replacing static example
parsing with runtime type tracing via the sibling `typetracer` package.
That pivot is now complete — the yaml pipeline and its supporting
example-text-scraping code have been fully removed (stages 001–002),
trace provenance is used to keep mutation testing scoped to documented
examples, and the migration's validation work surfaced and fixed several
pre-existing performance/correctness issues that the old pipeline's
lighter execution model had never exercised. Validation against further
real packages beyond the original migration continues to surface and fix
genuine `typetracer` bugs at the source (stage 003), consistent with the
project's established practice of root-causing issues in the tracer
itself rather than working around them in `autotest`.

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
- **Treating `examples_to_yaml()`/`at_yaml_template()` as permanently
  exempt from cleanup** (stage 002): stage 001 had judged these
  independently useful and kept them; a closer audit found neither had
  any live consumer either, and both were removed.
- **Committing the stage 003 `typetracer` fix immediately** (stage 003):
  the fix, its regression tests, and full verification against `pkgstats`
  are all complete, but the change was left uncommitted in the sibling
  `typetracer` repository per explicit instruction; only `autotest`'s own
  stage records were committed here.
