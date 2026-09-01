---
created: 2026-09-01T13:25:04Z
agent: claude-sonnet-5
git_hash: 4180d2b22d96c1cc0b3a503bba57774b33145ac0
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
`diagnostic`/`message`/`dummy`. Every place that actually executes
arbitrary target-package/example code — both `typetracer`'s in-process
example tracing and this package's own mutation-test invocations — runs
under a discarding graphics device, so plotting side effects never leak
into an open device, a stray `Rplots.pdf`, or a caller's own recording
device (e.g. `knitr`'s, during documentation rendering).

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

### Progress-display parameters should be consolidated, not layered
**Outcome:** `autotest_package()`'s `quiet` parameter was removed outright and replaced by a single `progress` parameter (`"bar"` default, `"tests"`, `"none"`), rather than keeping `quiet` and a new progress-style parameter as two separate, orthogonal switches.
**Rationale:** A full-codebase check showed `quiet`, within `autotest_package()`, only ever gated the single progress message being fixed anyway, and its one internal call to `autotest_single_trace()` already hardcoded `quiet = TRUE` regardless — so consolidating carried no hidden behavioral risk. Two other, unrelated functions (`autotest_single_trace()`, `autotest_obj()`) keep their own independent `quiet` parameters untouched.
**Roads not taken:** The initial plan kept `quiet` and `progress` as two co-existing parameters; this was revised before implementation once the consolidation was confirmed safe.
**Stages:** 004

### A corrected progress total requires filtering before the loop, not inside it
**Outcome:** `autotest_package()`'s progress display previously reported totals against *all* trace files (both example- and test-sourced), while silently skipping test-sourced ones before printing anything — showing an inflated denominator (173 vs. 32 actually processed, for one real package). Fixed by filtering to example-sourced traces once, up front, rather than mid-loop.
**Rationale:** Filtering mid-loop is what produced the inflated total in the first place; filtering before the loop starts gives an accurate total for any display style without extra file I/O.
**Roads not taken:** N/A.
**Stages:** 004

### Unexported functions must be resolved via the namespace, not the attached environment
**Outcome:** `autotest`'s own function enumeration (`get_pkg_functions()`) deliberately includes unexported functions (e.g. S3 methods registered but not `export()`ed), since these are legitimate targets for tracing and testing. `typetracer`'s `inject_pkg_trace_fns()`/`uninject_pkg_trace_fns()` only resolved names via the attached `package:X` search-path environment, which excludes unexported objects — fixed by resolving via `asNamespace(package)` instead, which contains both.
**Rationale:** The mismatch meant any package with an unexported function on the traced-function list (a common case, given how routine unexported S3 methods are) would crash `autotest_package()` outright, not just produce a spurious finding.
**Roads not taken:** A `tryCatch`-guarded `get()`-then-`getFromNamespace()` two-tier fallback, mirroring `autotest`'s own pattern exactly, was implemented first and works; simplified to a direct `asNamespace()` lookup once confirmed that a single namespace-based lookup already covers both exported and unexported cases.
**Stages:** 005

### Plotting side effects are contained at every real-execution site, not per-document
**Outcome:** A single internal helper (`with_null_device()`) wraps every point where
arbitrary package/example code actually runs — `typetracer::trace_package()`'s in-process
example evaluation, and this package's own mutation-test/noise-comparison call sites — in
a discarding graphics device, rather than adding `fig.show = "hide"` to individual
`README.Rmd`/vignette chunks.
**Rationale:** A per-document fix would not address the general `Rplots.pdf` residue
reported when autotesting arbitrary real-world packages, and the actual root cause was
traced to `typetracer`'s in-process example execution rather than to anything in
`autotest`'s own example-scraping code.
**Roads not taken:** Patching the `typetracer` dependency directly was considered and
rejected — wrapping the single call site into it from `autotest`'s own code was sufficient,
since R's graphics device stack is respected regardless of which package opens/closes
devices.
**Stages:** 006

### `progress = "bar"` must be knitr-aware, not just tty-aware
**Outcome:** `autotest_package()` now falls back from `progress = "bar"` to `"none"`
whenever `isTRUE(getOption("knitr.in.progress"))`, in addition to `cli`'s own tty
detection.
**Rationale:** `cli`'s dynamic-tty detection operates at the file-descriptor level and can
still read as a real terminal inside a `knitr` chunk even though `knitr` has redirected
R-level output, leaking literal ANSI clear-line sequences into rendered documents;
`knitr.in.progress` was confirmed empirically to be a reliable, environment-independent
signal for this specific context.
**Stages:** 006

### CRAN readiness over dev-cycle conveniences
**Outcome:** The `Remotes: mpadge/typetracer` git-remote dependency was dropped from `DESCRIPTION`, the version was bumped from a dev-style `0.1.0.033` to the release version `0.1.1`, `checkmate` was adopted for argument validation, and substantial dead code (`R/text-parsing-fns.R`, `R/example-objects.R`, and other unused internal functions) was removed, alongside lint and documentation polish.
**Rationale:** Inferred from commit messages and the nature of the changes — `Remotes:` fields, dev version numbers, and dead code are standard CRAN-submission blockers/flags; this cluster of otherwise-unrelated cleanups converges on making the package CRAN-ready now that the typetracer migration (stages 000-006) is stable.
**Roads not taken:** N/A — no design alternatives were in tension here; this was maintenance convergence rather than a design decision with competing options.
**Stages:** 007 (auto-retrospective, untracked development)

### Coverage work exercises the public dispatch pipeline, extending the existing fixture pattern
**Outcome:** Unit-test coverage was raised package-wide (73.3% → 82.2%) by
extending `tests/local-pkg.R`'s synthetic-package fixture pattern
(`make_test_name()`, `make_test_logical()`, two new documented-range int
fixtures) and adding direct unit tests for two pure internal/exported
functions (`test_these_data()`, `summary.autotest_package()`) that don't
need the full pipeline. A single `test_data`-toggling test was found to
exercise the shared `if (!is.null(test_data))` guard present in every
`input-*.R`/`test-*.R` mutation function at once.
**Rationale:** Consistent with the established convention (stage 000) of
driving mutation-testing coverage through `autotest_package()`'s public
entry points and the `autotest_obj` S3 dispatch, rather than calling
internal generics directly.
**Roads not taken:** A `subst_for_logical(x = ...)` dead-code branch and a
`test_single_int_range` "actual range wider than documented" mismatch
branch were both found to be effectively unreachable through legitimate
test design (the former is never called with `x` by any caller; the latter
would require the probing algorithm to overshoot a documented boundary,
which it structurally never does) — left uncovered rather than force
contrived fixtures or fix the underlying dead code, which was out of this
stage's scope.
**Stages:** 008

### A genuine `typetracer` crash fixed at the source; a deeper self-recursion limitation left for a future stage
**Outcome:** Testing `expect_autotest_no_testdata()`/`expect_autotest_testdata()`
crashed with an invalid `rep(times = ...)` argument inside
`typetracer::join_test_trace_data()`, caused by a trailing test's own start
trace-number exceeding the global max trace number. Fixed by clamping the
span to zero, verified, and installed locally. A second, separate problem
then emerged: `here::here()` (which both functions hardcode) resolves once
per R session and cannot be redirected afterward, so calling either
function from inside `autotest`'s own test suite causes the suite to
recursively re-trace (and thus re-run) itself without bound. Both functions
remain untested in the suite as a result.
**Rationale:** Consistent with the project's established practice
(stage 003) of root-causing genuine `typetracer` bugs at the source rather
than working around them in `autotest`. The self-recursion problem,
however, would require a `types=` passthrough on `autotest_package()` — a
real production-signature change judged out of scope for a test-coverage
stage.
**Roads not taken:** Adding the `types=` passthrough now, to make these two
functions safely self-testable, was proposed and explicitly declined in
favor of a future, deliberate stage.
**Stages:** 008

### `fs` adopted package-wide as the sole path-manipulation layer
**Outcome:** All path-construction, path-inspection, and path-listing calls
in `R/` and `tests/testthat/test-statspkg.R` use `fs::` rather than base R
(`file.path`, `basename`, `dirname`, `normalizePath`, `file.exists`,
`dir.exists`, `file.remove`, `list.files`, and manual
`.Platform$file.sep`-based splitting); `fs` moved from an unused `Suggests`
entry to `Imports`. `tempfile()`/`tempdir()` remain base R, since `fs` only
manipulates paths that already exist, not creation.
**Rationale:** Consolidates path handling onto one consistent, more robust
API rather than base R's scattered path functions.
**Roads not taken:** Coercing every `fs::` return value back to plain
`character` at each call site was considered and rejected — `fs_path`
values behave as character in the comparisons/data frames used throughout
this codebase, so blanket defensive coercion was judged unnecessary.
**Stages:** 009

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
itself rather than working around them in `autotest`. Stage 006 extended
this same discipline to a documentation/build-hygiene concern (stray
plot files and progress-bar noise from `make knitr`), tracing it through
to `typetracer`'s in-process example execution and fixing it via a call-
site wrapper rather than patching the dependency or working around the
symptom per document. With the typetracer migration and its follow-on
fixes stable, the project's next phase (stage 007, an auto-retrospective
covering untracked maintenance work) turned to CRAN-submission readiness:
dropping the git-remote dependency on `typetracer`, moving to a release
version number, adopting `checkmate` for argument validation, and
clearing out dead code and lint issues accumulated since 2020. Stage 008
turned to test coverage itself, extending the established fixture pattern
to close several long-standing S3-dispatch coverage gaps and, in the
process, surfacing and fixing another genuine `typetracer` defect —
continuing the project's practice of treating validation/testing work as a
source of real upstream bug discovery, not just a box to check. Stage 009
was a focused internal refactor, consolidating path manipulation onto the
`fs` package throughout `R/` and the test suite; the conversion itself
surfaced two latent bugs in the mechanical base-R-to-`fs` mapping (a
full-path-vs-basename mismatch, and `regexp` matching full paths rather
than basenames), both caught only because each converted file was
re-verified against its tests rather than trusted by inspection —
consistent with the project's established discipline of verifying
behavioral equivalence empirically.

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
- **Keeping `quiet` alongside a new `progress` parameter** (stage 004):
  the initial plan for `autotest_package()`'s progress-reporting fix
  proposed both as separate, orthogonal switches; consolidated into
  `progress` alone (with `"none"` subsuming `quiet = TRUE`) once confirmed
  safe.
- **Per-document `fig.show = "hide"` chunk options** (stage 006):
  considered as the direct fix for stray plot files in rendered
  documentation, but rejected in favor of a root-cause fix at every actual
  code-execution site, since a per-chunk fix would not address the same
  bug's effect on arbitrary real-world packages (stray `Rplots.pdf`
  files), and would need re-applying to every new document.
- **Patching the `typetracer` dependency directly** (stage 006): the
  actual plot-generation root cause was traced into `typetracer::
  trace_package()`'s in-process example execution; wrapping the single
  call site into it from `autotest`'s own code was sufficient and
  preferred over modifying the external package.
- **Adding a `types=` passthrough to `autotest_package()`** (stage 008):
  proposed as the real fix that would let `expect_autotest_no_testdata()`/
  `expect_autotest_testdata()` avoid self-recursive test-suite tracing;
  declined as a production-signature change out of scope for a
  test-coverage stage, leaving both functions untested for now.
- **Forcing coverage of dead/unreachable branches** (stage 008): a
  `subst_for_logical()` dead-code path and a `test_single_int_range`
  mismatch branch that the probing algorithm structurally cannot reach
  were both left uncovered rather than writing contrived fixtures or
  fixing the underlying dead code, since neither was in scope.
- **Coercing `fs_path` objects to plain `character` at every call site**
  (stage 009): considered as a defensive measure against `fs::`'s S3-classed
  return values leaking into comparisons or output; rejected as unneeded
  verbosity, since `fs_path` already behaves as character throughout this
  codebase's actual usage.
