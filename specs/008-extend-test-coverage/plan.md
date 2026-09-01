---
created: 2026-09-01T09:30:00Z
agent: claude-sonnet-5
git_hash: a571fbbd3611b205e1f960535e4cb4d22c8e9b8f
---

# Plan: extend-test-coverage

## Overview
Raise unit-test coverage on the package's weakest files ahead of CRAN
submission: `R/methods.R` (0% covered), the `input-*.R` parameter-
mutation family — `R/input-name.R` (22.6%), `R/input-int.R` (23.5%), and
`R/input-logical.R` (66.7%) — plus `R/test-data.R` (41.7%) and
`R/testthat-expections.R` (42.3%), rather than chasing an
across-the-board percentage target.

## Context
Stage 007 (auto-retrospective) captured a cluster of CRAN-readiness work:
dropping the `Remotes: typetracer` git dependency, bumping to release
version `0.1.1`, adopting `checkmate` for argument validation, and
removing dead code. This stage continues that CRAN-readiness push from
the test-coverage angle. `specs/design-decisions.md` documents the
S3-dispatch extensibility mechanism (`test_single_*`/`test_rect_*`/
`test_return_*` generics keyed on `autotest_obj`) as the stable core
architecture — new tests should exercise this dispatch machinery via its
existing entry points (`autotest_package()`, `autotest_types()`) rather
than calling internal S3 methods directly, consistent with how the
existing suite is written.

Current baseline, measured via `covr::package_coverage()`:
package-wide 73.3%. Per-file, the six weakest are `R/methods.R` (0%),
`R/input-name.R` (22.6%), `R/input-int.R` (23.5%), `R/test-data.R`
(41.7%), `R/testthat-expections.R` (42.3%), and `R/input-logical.R`
(66.7%) — all six are in this stage's scope.

The existing test suite already has a pattern for exercising the
input-mutation machinery end-to-end: `tests/local-pkg.R` builds a
minimal synthetic source package on disk (`make_pkg()`, composed from
`make_desc()`, `make_test_int()`, `make_test_rect()`,
`make_namespace()`), and `tests/testthat/test-local-pkg.R` runs
`autotest_package(package = package, test = TRUE)` against it and
asserts on the resulting `type`/`test` columns. `make_test_int()`
already contributes some `input-int.R` coverage, but only exercises one
narrow branch (`test_int(x)` erroring above `1e3`) of a large (~470
line) function with many internal mutation branches — the rest of
`input-int.R`'s low coverage comes from those unexercised branches, not
from a missing fixture. There is currently no `make_test_name()` or
`make_test_logical()` fixture, which is the direct cause of
`input-name.R` and `input-logical.R`'s low coverage: no synthetic
function in the local test package has a name/formula- or
logical-typed parameter for the dispatch to ever reach.

`R/methods.R` contains a single exported S3 method,
`summary.autotest_package()`, entirely untested. It operates purely on
the shape of an `autotest_package` result object (columns `fn_name`,
`type`, `content`; attributes `package`/`packageName`/`githash`) and
`message()`/`print()` output — it does not execute any target-package
code, so it can be tested directly against a constructed fixture object
without going through `autotest_package()` at all.

`R/test-data.R` contains one `@noRd` internal function,
`test_these_data(test_data, obj)`, called from every `input-*.R`/
`test-*.R` mutation-test site whenever a caller-supplied `test_data`
data.frame is present (`input-int.R`, `input-char.R`, `input-double.R`,
`input-logical.R`, `input-name.R`, `test-vector.R`,
`test-return-object.R`, `test-single-val.R` — fourteen call sites in
total), to decide whether a given fn/parameter/test combination should
run. Its narrowing-by-`fn_name`, narrowing-by-`parameter`, and
ambiguous-`stop()` branches are exercised only incidentally today, via
`test-testthat-expectation.R`'s one `test_data =` call against `stats::cov`.
Being unexported but in-package, it can be unit-tested directly by name
(no `:::` needed inside `testthat` files) with small hand-built
`test_data`/`obj` data.frames, rather than only indirectly through a
full `autotest_package()` run — much cheaper than routing every branch
through the pipeline.

`R/testthat-expections.R` defines five exported `testthat` custom
expectations: `expect_autotest_no_testdata()`, `expect_autotest_testdata()`,
`expect_autotest_no_err()`, `expect_autotest_no_warn()`, and
`expect_autotest_notes()` — meant for a target package's own test suite to
assert "autotesting this package is clean." Current coverage
(`test-testthat-expectation.R`) only exercises the *failure* path of
`expect_autotest_no_err()`/`expect_autotest_no_warn()` and the *success*
path of `expect_autotest_notes()` (with one failure path explicitly
disabled, referencing issue #61); `expect_autotest_no_testdata()` and
`expect_autotest_testdata()` are entirely untested. Both of those two call
`autotest_package(here::here(), test = TRUE, ...)` — i.e. they run
`autotest` against its own source tree via `here::here()`, so testing
them means running `autotest_package()` on the `autotest` package itself
from within its own test suite, which is slower than the other in-scope
work but requires no new fixture.

## Design Goals
- Bring `R/methods.R` from 0% to full (or near-full) coverage by testing
  `summary.autotest_package()` directly against constructed
  `autotest_package`-classed fixtures, covering: the normal path (mixed
  error/warning/message/diagnostic rows), the "no documented examples"
  branch, both the installed-package and local-source-package version
  lookup paths (`pkg_is_source()` true/false), and the optional
  `githash` attribute being present vs. absent.
- Substantially raise `R/input-name.R` coverage by adding a
  `make_test_name()` fixture (a synthetic function taking an unquoted
  name/formula parameter) to `tests/local-pkg.R`, wired into
  `make_pkg()`, and exercised through the existing
  `autotest_package(package = package, test = TRUE)` flow in
  `test-local-pkg.R` — reaching `test_single_name.autotest_obj`'s real
  execution path (currently only the `.NULL`/dummy branch is covered).
- Substantially raise `R/input-logical.R` coverage the same way, via a
  `make_test_logical()` fixture covering a synthetic function with a
  logical parameter, reaching `test_negate_logical`,
  `test_int_for_logical`, and `test_char_for_logical` and their
  `.autotest_obj` dispatch branches.
- Raise `R/input-int.R` coverage by extending the *existing*
  `make_test_int()` fixture (or the assertions run against it) to
  exercise more of `test_single_int_range.autotest_obj`'s internal
  mutation branches, not just the single `x > 1e3` error path already
  covered — enough to meaningfully move the file's 23.5% baseline, not
  necessarily to full coverage given the function's size.
- Bring `R/test-data.R` (`test_these_data()`) to full or near-full
  coverage via direct unit tests against small hand-built `test_data`/
  `obj` data.frames, covering: no matching `test_name` (zero-length
  result), a single unambiguous match, narrowing by `fn_name` when
  multiple `test_name` rows match, narrowing by `parameter` when
  multiple `fn_name`-narrowed rows match, and the ambiguous-input
  `stop()` path when a single `test` flag still can't be determined.
- Substantially raise `R/testthat-expections.R` coverage by adding
  success-path tests for `expect_autotest_no_err()` and
  `expect_autotest_no_warn()` (currently only their failure paths are
  tested) and first-time tests for `expect_autotest_no_testdata()` and
  `expect_autotest_testdata()`, using a target with a clean, warning/
  error-free documented example so the success paths are actually
  reached, rather than only ever testing against `stats::cov` (which is
  deliberately chosen elsewhere for its warnings/errors).
- Every new test must run fully offline (no package installs from
  CRAN/GitHub, no network calls) and complete quickly enough not to
  materially slow the existing suite — consistent with the synthetic
  in-tempdir package pattern `tests/local-pkg.R` already uses.

## Proposed Approach
- **`R/methods.R`**: add `tests/testthat/test-methods.R`. Build a small
  `data.frame`/tibble by hand with class `autotest_package` and the
  columns `summary.autotest_package()` reads (`fn_name`, `type`,
  `content`), setting `package`/`packageName` attributes to point at an
  installed package (e.g. `autotest` itself) for the non-source path,
  and separately to a `tests/local-pkg.R`-built package directory for
  the source path (exercising `pkg_is_source()` branching by reusing
  the existing local-pkg fixture rather than inventing a new one).
  Assert on captured `message()` output (`testthat::expect_message` /
  capturing via `evaluate_promise` or similar) and on the printed
  summary `data.frame`'s shape, not on exact wording, to keep the test
  resilient to copy changes.
- **`tests/local-pkg.R`**: add `make_test_name(d)` and
  `make_test_logical(d)`, following the existing `make_test_int(d)`
  pattern exactly (write an `R/*.R` source file plus a matching
  `man/*.Rd` file with a documented example), and wire both into
  `make_pkg()` alongside the existing `make_test_int`/`make_test_rect`
  calls.
- **`tests/testthat/test-local-pkg.R`**: extend the existing `"pkg"`
  test's assertions (or add new `test_that()` blocks in the same file)
  to check that the new name/logical synthetic functions produce
  `type`/`test` results consistent with the existing int/rect
  assertions, rather than building a wholly separate fixture package.
- **`R/input-int.R`**: inspect `test_single_int_range.autotest_obj`'s
  branch structure directly (it is one large function) to identify
  which mutation cases are currently unreached, and extend
  `make_test_int()`'s synthetic function (or add a second synthetic
  int-parameter function alongside it) so those branches are reached
  through the same `autotest_package(..., test = TRUE)` flow — not by
  calling internal `test_single_int_range*` functions directly, to
  stay consistent with how every other test in the suite exercises this
  machinery.
- **`R/test-data.R`**: add `tests/testthat/test-test-data.R`, calling
  `test_these_data()` directly (unexported but same-package, so no
  `:::` needed) against small hand-built two/three-row `test_data`
  data.frames and matching `obj` lists/data.frames, one `test_that()`
  block per branch listed above.
- **`R/testthat-expections.R`**: extend
  `tests/testthat/test-testthat-expectation.R` (or add a second file)
  with: (a) a success-path case for `expect_autotest_no_err()`/
  `expect_autotest_no_warn()` — likely reusing the `tests/local-pkg.R`
  synthetic package (or a similarly clean function) rather than
  `stats::cov`, since it has no warning/error-triggering example; and
  (b) `expect_autotest_no_testdata()`/`expect_autotest_testdata()`
  tests that run `autotest_package()` against `autotest`'s own source
  via `here::here()` — accept the slower runtime here since these two
  functions are explicitly designed to run against the calling
  package's own tree and there's no cheaper way to exercise them
  faithfully.
- After implementation, re-run `covr::package_coverage()` and report the
  before/after per-file numbers for all six in-scope files plus the
  package-wide total, as the stage's completion evidence.

## Open Questions
- Testing `expect_autotest_no_testdata()`/`expect_autotest_testdata()`
  means running `autotest_package()` against `autotest`'s own source
  tree from within its own test suite — the slowest addition in this
  plan. If it materially slows the suite once implemented, the fallback
  is to scope those two tests down (e.g. `functions =` a small subset)
  rather than skip them, since they're the only untested exported
  functions in `testthat-expections.R`.
- No numeric package-wide coverage target was set (e.g. 85%) — this
  stage is scoped by file, not by a percentage floor, and does not
  change CI to enforce a coverage threshold. Whether to add a
  covr/codecov CI gate is left for a separate stage if wanted.
- The exact set of `test_single_int_range.autotest_obj` branches worth
  targeting isn't enumerated yet (the function is ~320 lines); the
  implementation stage (`/designlens.make-tasks`) should do that
  analysis and break it into concrete tasks rather than the plan
  pre-committing to a branch list.
