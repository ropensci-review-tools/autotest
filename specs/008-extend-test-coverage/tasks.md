---
created: 2026-09-01T09:45:00Z
agent: claude-sonnet-5
git_hash: a571fbbd3611b205e1f960535e4cb4d22c8e9b8f
---

# Tasks: extend-test-coverage

## T008-1: Test `summary.autotest_package()` directly
- [ ] T008-1: Add `tests/testthat/test-methods.R`. Hand-build a small
  `data.frame` with class `autotest_package` and columns `fn_name`,
  `type` (mix of `"error"`/`"warning"`/`"message"`/`"diagnostic"`
  values across at least two `fn_name`s), and `content` (including at
  least one row containing the literal string `"no documented
  example"` to exercise the "no documented examples" branch). Add one
  `test_that()` block per scenario:
  - normal mixed-type summary against an *installed* package: set
    `attr(x, "package")` to `"autotest"` and `attr(x, "packageName")`
    to `"autotest"` (exercises the `pkg_is_source()` FALSE /
    `utils::packageVersion()` branch);
  - the same against a *local source* package: build one via
    `source("../local-pkg.R"); d <- make_pkg()` (or equivalent) and set
    `attr(x, "package")` to that directory path (exercises the
    `pkg_is_source()` TRUE / `read.dcf()` branch);
  - a fixture with one `"no documented example"` row (exercises the
    `no_ex`/"functions which have no documented examples" branch);
  - a fixture with and without a `githash` attribute set (exercises
    the trailing `!is.null(attr(object, "githash"))` branch).
  Capture `message()` output with `testthat::expect_message()` (or
  wrap the call and inspect via `testthat::capture_messages()`) and
  assert on the *shape* of what `summary()` returns/prints (row count,
  column names of the printed `res` data.frame) rather than exact
  wording, so the test stays resilient to copy changes.

## T008-2: Add a `make_test_name()` fixture to the local test package
- [ ] T008-2: In `tests/local-pkg.R`, add `make_test_name(d)` following
  the exact structure of the existing `make_test_int(d)` (lines 31-72):
  write a source file under `d/R/` defining a small roxygen-documented,
  exported function taking one unquoted name/formula-typed parameter
  (e.g. a `col` argument used as `col_name <- deparse(substitute(col))`
  or similar, mirroring how `typetracer`/`autotest` classify a
  name/formula parameter), with a working documented `@examples` call;
  and write the matching `man/*.Rd` file under `d/man/` by hand (same
  pattern as `test_int.Rd`). Wire `make_test_name(d)` into `make_pkg()`
  (around line 134) alongside the existing `make_test_int(d)` /
  `make_test_rect(d)` calls.

## T008-3: Add a `make_test_logical()` fixture to the local test package
- [ ] T008-3: In `tests/local-pkg.R`, add `make_test_logical(d)`
  following the same pattern, defining a small exported function
  taking one logical-typed parameter with a documented example. Design
  the function body so that: negating the default value still runs
  without error (exercises `test_negate_logical.autotest_obj`'s normal
  path); substituting integer values `0L`/`1L`/`2L` for the logical
  parameter is *not* uniformly rejected (exercises the `all(chk)`
  FALSE branch of `test_int_for_logical.autotest_obj`, i.e. `res$type
  <- "diagnostic"` is reached); and substituting a character value
  (`"a"`) is *not* rejected either (exercises the equivalent branch of
  `test_char_for_logical.autotest_obj`, i.e. `msgs` stays `NULL` so the
  original `res` — type `"diagnostic"` — is returned rather than set to
  `NULL`). A function like `f <- function(flag = TRUE) if (isTRUE(flag))
  1 else 0` satisfies all three (R's `if()` coerces non-logical scalars
  without erroring). Wire `make_test_logical(d)` into `make_pkg()`
  alongside the other `make_test_*(d)` calls.

## T008-4: Extend `test-local-pkg.R` assertions for the new fixtures
- [ ] T008-4: In `tests/testthat/test-local-pkg.R`, extend the existing
  `"pkg"` `test_that()` block's assertions (or add adjacent
  `test_that()` blocks in the same file) to check that the
  `xt <- autotest_package(package = package, test = TRUE)` result
  includes rows for the new name- and logical-parameter functions with
  `type` values consistent with what T008-2/T008-3 were designed to
  produce (e.g. `"diagnostic"` rows from the new logical-parameter
  substitution tests, non-`"no_test"` rows for the name-parameter
  test). Update the existing `expect_identical(as.integer(types[names
  == "diagnostic"]), 18L)` literal-count assertion to the new correct
  count once the new fixtures are wired in — recompute it by running
  the suite locally rather than guessing the new total.

## T008-5: Raise `R/input-int.R` coverage via `make_test_int()`
- [ ] T008-5: Read `test_single_int_range.autotest_obj` in
  `R/input-int.R` (lines ~39-360) end to end and list which of its
  internal mutation branches (e.g. min/max boundary probes, negative
  values, zero, `NA`, non-integer doubles passed as int, overflow
  checks) are and are not currently reached by the existing
  `make_test_int()` fixture (`test_int <- function(x = 1) { if (x >
  1e3) stop("upper limit"); x^2 }`). Extend `make_test_int(d)` in
  `tests/local-pkg.R` — either by broadening `test_int`'s body to
  respond differently to more of the probed values, or by adding a
  second synthetic int-parameter function alongside it with a
  different response profile (e.g. one that errors on negative input,
  one that has no upper bound) — so more of those branches are reached
  through the same `autotest_package(package = package, test = TRUE)`
  flow used in `test-local-pkg.R`. Do not call
  `test_single_int_range*` directly; route everything through
  `autotest_package()` as the rest of the suite does. Update
  `test-local-pkg.R`'s assertions to reflect the new result rows, same
  as T008-4.

## T008-6: Unit-test `test_these_data()` directly
- [ ] T008-6: Add `tests/testthat/test-test-data.R`. Call
  `test_these_data(test_data, obj)` directly by name (it is unexported
  but in-package, so no `:::` is needed inside `testthat` files run via
  `devtools::test()`/`testthat::test_local()`). Write one `test_that()`
  block per branch in `R/test-data.R`:
  - `test_data` has no row matching `obj$test_name` → result has
    `length() == 0`;
  - `test_data` has exactly one row matching `obj$test_name` → that
    row's `test` value is returned;
  - `test_data` has multiple rows matching `obj$test_name` but only
    some also match `obj$fn_name` → result narrows to the `fn_name`-
    matching subset (construct `test_data`/`obj` so the narrowed
    subset has a single unique `test` value);
  - after `fn_name`-narrowing, multiple rows remain but only some also
    match `obj$parameter` → result narrows further to the `parameter`-
    matching subset;
  - construct a case where, even after both narrowings, more than one
    distinct `test` value remains → assert `test_these_data()` throws
    via `expect_error()`, matching the `stop("Cannot determine single
    'test' flag from 'test_data' for...")` message.

## T008-7: Add success-path tests for `expect_autotest_no_err()`/`expect_autotest_no_warn()`
- [ ] T008-7: In `tests/testthat/test-testthat-expectation.R` (or a new
  adjacent file), add a `test_that()` block that runs
  `autotest_package(package = package, test = TRUE)` against a target
  with no warning/error-triggering documented examples — reuse the
  `tests/local-pkg.R` synthetic package built for T008-1/T008-4/T008-5
  (or a purpose-built minimal one if those fixtures' examples happen to
  trigger warnings) — and asserts `expect_success(expect_autotest_no_err(x))`
  and `expect_success(expect_autotest_no_warn(x))`, complementing the
  existing failure-path assertions against `stats::cov`.

## T008-8: Test `expect_autotest_no_testdata()` and `expect_autotest_testdata()`
- [ ] T008-8: In `tests/testthat/test-testthat-expectation.R` (or a new
  adjacent file), add tests exercising `expect_autotest_no_testdata()`
  and `expect_autotest_testdata()`, both of which internally call
  `autotest_package(here::here(), test = TRUE, ...)` — i.e. they run
  against `autotest`'s own source tree from within its own test suite.
  Cover: `expect_success(expect_autotest_no_testdata())` (or
  `expect_failure()`, whichever the current state of `autotest`'s own
  source actually produces — verify empirically rather than assuming);
  and `expect_autotest_testdata(test_data)` using
  `autotest_types(notest = <some test name>)` as `test_data`, checking
  both the success and (if reachable) the missing-`note`-column failure
  path already partially exercised in the existing `"expect_autotest"`
  test block. If this test measurably slows the suite, scope it down
  with `functions = <small subset>` rather than dropping it, per the
  plan's Open Questions.

## T008-9: Re-measure coverage and report results
- [ ] T008-9: After T008-1 through T008-8 are implemented, run
  `covr::package_coverage()` again and report before/after per-file
  coverage for all six in-scope files (`R/methods.R`,
  `R/input-name.R`, `R/input-int.R`, `R/input-logical.R`,
  `R/test-data.R`, `R/testthat-expections.R`) plus the package-wide
  total, as this stage's completion evidence. Run the full test suite
  (`devtools::test()` or `testthat::test_local()`) once more to confirm
  nothing regressed.
