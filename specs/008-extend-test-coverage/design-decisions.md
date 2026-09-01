---
created: 2026-09-01T11:00:00Z
agent: claude-sonnet-5
git_hash: ce2a341c7bc9998e5896d3c2d880289062c69fd8
---

# Design Decisions: extend-test-coverage

## Summary
Raised unit-test coverage on `autotest`'s six weakest files ahead of CRAN
submission (package-wide 73.3% → 82.2%), extending the existing
`tests/local-pkg.R` synthetic-fixture pattern to reach previously-untested
S3 dispatch branches, and along the way fixed a genuine crash in the sibling
`typetracer` package's test-tracing code.

## New Design Decisions

### `R/methods.R`, `R/test-data.R` brought to full coverage via direct/fixture testing
**Chosen:** `summary.autotest_package()` (0% → 100%) is tested directly
against hand-built `autotest_package`-classed fixtures (no need to route
through the full pipeline, since it does no target-package execution).
`test_these_data()` (`R/test-data.R`, 41.7% → 100%) is unit-tested directly
by name — it is unexported but in-package, so no `:::` is required inside
`testthat` files.
**Rationale:** Both are pure functions operating on structured data; direct
testing is far cheaper than routing through `autotest_package()` and gives
exhaustive branch coverage.
**Proposed by:** agent

### New synthetic fixtures close the name/formula and logical dispatch gaps
**Chosen:** Added `make_test_name()` and `make_test_logical()` to
`tests/local-pkg.R` (following the existing `make_test_int()` pattern:
source file + matching `.Rd`), wired into `make_pkg()`. `R/input-name.R`
went from 22.6% to 93.6%; `R/input-logical.R` from 66.7% to 76.3%.
**Rationale:** No existing fixture in the suite had a name/formula- or
logical-typed parameter, so `test_single_name`/`test_negate_logical`/
`test_int_for_logical`/`test_char_for_logical`'s real-execution branches
(as opposed to their `.NULL`/dummy branches) were structurally unreachable.
A `name`-typed traced value requires the example to pass an expression that
*evaluates* to a name/symbol (e.g. `as.name("y")`), not merely an unquoted
bare symbol.
**Tradeoffs:** `input-logical.R`'s `test_negate_logical` and the
`subst_for_logical(x = ...)` non-`NULL` branch remain uncovered —
the latter is dead code (the `.autotest_obj` methods that build its
`.NULL` variant never thread `x` through to it), a latent bug noted here
but left unfixed as out of this stage's scope.
**Proposed by:** agent

### A single `test_data`-toggling test exercises the `test_data`-gated branch across every mutation-test function at once
**Chosen:** One additional `test_that()` block passes the demo package's own
dummy listing (`autotest_package(package=...)`, `test = FALSE`) back in as
`test_data`, with a single test switched off, then re-runs with `test =
TRUE`.
**Rationale:** Every `input-*.R`/`test-*.R` mutation function shares the same
`if (!is.null(test_data)) { ... }` guard; passing any non-NULL `test_data`
exercises that branch everywhere in one pass, which also raised
`R/input-int.R` (23.5% → 60.2%) and contributed further to
`R/input-logical.R` and `R/input-name.R`.
**Proposed by:** agent

### `R/input-int.R`: two new fixtures exercise `get_int_range()`'s documented-range comparison logic
**Chosen:** Added `test_int_range()` (documented "greater than 0 and less
than 100") and `test_int_negrange()` (documented "must be a negative
integer") to `make_test_int()`.
**Rationale:** The pre-existing `test_int()` fixture only exercised one
narrow branch of the ~320-line `test_single_int_range.autotest_obj`; these
two reach the has-neg-pos-skip branch and the numbers-present/no-mismatch
branch, plus much of `get_int_range()`/`int_upper_limit()`/
`int_lower_limit()`/`stepdown()`'s probing logic (via line execution, even
where the resulting diagnostic row is itself `NULL`).
**Tradeoffs:** The "actual range exceeds documented range" mismatch branch
was investigated and found effectively unreachable by design — the probing
algorithm only ever tightens toward a documented boundary via stepdown, it
never discovers an actual range *wider* than what was probed at the
documented boundary itself. Left uncovered rather than force a contrived
reproduction.
**Proposed by:** agent

### A real crash found and fixed in sibling package `typetracer`, but a deeper limitation left unresolved
**Chosen:** Testing `expect_autotest_no_testdata()`/`expect_autotest_testdata()`
(both call `autotest_package(here::here(), test = TRUE, ...)`) crashed with
`Error in rep(..., times = test_tr_end - test_tr_start + 1) : invalid
'times' argument` inside `typetracer::join_test_trace_data()`
(`pre-processing-r/typetracer`, `R/trace-package.R`) — a trailing test's own
start trace-number could exceed the global max trace number, making the
span negative. Fixed by clamping to `pmax(0L, ...)`, verified, and installed
locally so `autotest`'s normal test suite picks up the fix. A second,
distinct problem then surfaced: `here::here()` resolves once per R session
and is not redirectable via `setwd()`/`withr::with_dir()` afterward, so
calling either function from *within* `autotest`'s own test suite makes
`autotest_package()` trace (and thus re-execute) that same suite from
inside itself, including the very file containing the call — unbounded
self-recursion (runs observed exceeding 550s without completing). Adding a
`types=` passthrough to `autotest_package()`/`autotest_trace_package()` to
let these two functions skip self-tracing tests was proposed and declined
as a separate, deliberate feature addition rather than in-scope for a
coverage stage.
**Rationale:** The crash fix is small, low-risk, and independently
worthwhile regardless of whether these two functions ever get tested here.
The `types=` passthrough touches a public function's signature and is a
real capability addition, not a test-only change.
**Tradeoffs:** `expect_autotest_no_testdata()`/`expect_autotest_testdata()`
remain untested, capping `R/testthat-expections.R` at 42.3% (its two other
functions, `expect_autotest_no_err()`/`expect_autotest_no_warn()`, contain
no internal branching, so the pre-existing failure-path tests already gave
them full line coverage — the new success-path tests added real behavioral
verification but moved no coverage number). The `typetracer` fix is
committed nowhere yet — left uncommitted in that sibling repository per
this project's stage-003 precedent for cross-repo `typetracer` fixes.
**Proposed by:** joint

## Integration with Prior Work
Continues stage 007's CRAN-readiness thread from the test-coverage angle.
Reuses and extends the `tests/local-pkg.R` synthetic-package fixture
pattern established well before stage 000-era history, keeping the
convention (per `specs/design-decisions.md`) of exercising the S3-dispatch
machinery through its public entry points rather than internal generics
directly. Continues the project's established practice (stages 001, 003) of
root-causing genuine bugs surfaced during work at the source — here, in the
sibling `typetracer` package — rather than working around them.

## Issues Resolved
- `R/methods.R`, `R/test-data.R`: 0%/41.7% → 100%.
- `R/input-name.R`: 22.6% → 93.6%.
- `R/input-logical.R`: 66.7% → 76.3%.
- `R/input-int.R`: 23.5% → 60.2%.
- Package-wide: 73.3% → 82.2%.
- A crash in `typetracer::join_test_trace_data()` (invalid `rep(times =
  ...)` from a negative trace-span) fixed and locally installed.

## Deferred Items
- `R/testthat-expections.R` remains at 42.3%; testing its two
  self-referential functions safely requires a `types=` passthrough
  feature on `autotest_package()`, deferred to a future stage.
- `subst_for_logical(x = ...)`'s non-`NULL` branch is dead code (never
  reached, since callers never pass `x` through) — noted but not fixed.
- The `typetracer` crash fix needs a decision on committing/releasing it
  upstream in the sibling repository.

## Process Notes
- Scope was expanded mid-plan (before implementation started) to add
  `R/test-data.R` and `R/testthat-expections.R` at mpadge's request.
- Task 8 (`expect_autotest_no_testdata()`/`expect_autotest_testdata()`)
  required real empirical investigation — a crash was found and fixed, then
  a deeper architectural blocker (unbounded self-recursion via `here::here()`
  session-caching) was found *after* the crash fix, requiring a second
  explicit decision from mpadge on how to proceed. The task concluded with
  the crash fix kept and no new tests added for those two functions.
