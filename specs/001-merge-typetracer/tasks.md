---
created: 2026-07-29T11:15:16Z
agent: claude-sonnet-5
git_hash: 273bf8b5c03cab47d5d2b45a5d34c92db97c1b7b
---

# Tasks: merge-typetracer

## T001-1: Confirm typetracer's trace-provenance API is sufficient
- [x] T001-1: In the sibling `typetracer` source at
  `/data/mega/code/repos/pre-processing-r/typetracer`, read
  `R/trace-package.R`, `R/load-and-clear-traces.R`, and `R/tracer-define.R`
  in full (not just grep hits). Confirm that `trace_source`
  (`"examples"`/`"tests"`) and `call_env` reliably distinguish
  example-derived calls from test-suite-derived calls, including for
  indirect/nested calls (e.g. a package helper function called from inside
  a `testthat::test_that()`/`expect_error()` block, or a function called by
  another traced function during a test). Resolve open questions 1 and 3
  from `plan.md`: whether `call_env` (not just `trace_source`) is needed
  anywhere, and whether `Sys.setenv("TYPETRACER_LEAVE_TRACES", "true")`
  around `do.call(typetracer::trace_package, args)` still behaves correctly
  once `types` is passed explicitly. If a gap is found in typetracer's
  behaviour, patch it in the sibling repo and note the change; otherwise
  record confirmation that no typetracer-side change is needed. Document
  the resolution as a short note added to `plan.md`'s Open Questions
  section (mark each resolved).

## T001-2: Pass explicit `types = c("examples", "tests")` through to `trace_package()`
- [x] T001-2: In `R/typetrace-package.R`, update `autotest_trace_package()`
  to explicitly pass `types = c("examples", "tests")` to
  `typetracer::trace_package()` (rather than relying on the default), per
  the confirmation from T001-1. Verify the `Sys.setenv`/`Sys.unsetenv`
  handling of `"TYPETRACER_LEAVE_TRACES"` around the call still leaves
  trace files on disk for `autotest_package()` to read via
  `list.files(get_typetrace_dir(), pattern = "^typetrace\\_.*\\.Rds$")`.

## T001-3: Restrict mutation/fuzz testing to example-sourced traces
- [x] T001-3: In `R/autotest-functions.R`, update `autotest_package()`'s
  loop over `trace_files` (or `autotest_single_trace()` itself) so that
  `autotest_rectangular()`, `autotest_vector()`, `autotest_single()`, and
  `autotest_return()` are only invoked for trace files/trace_data whose
  `trace_source == "examples"` (read via `readRDS(trace_files[i])$trace_source`
  or the equivalent field on the loaded trace object — confirm exact field
  name/shape from T001-1's findings). Trace files with
  `trace_source == "tests"` must still be included in the `traces` object
  passed to `get_unique_fn_pars()`/`fn_pars` so they continue to enrich
  parameter type/class inference in `get_param_info()`, but must be skipped
  for the mutation-testing calls. Verify with a manual test against
  `autotest_package(package = "stats", functions = "cov")` that no error
  report is generated for `cov`'s known test-suite-triggered error case.

## T001-4: Remove the obsolete yaml/example-parsing pipeline
- [x] T001-4: Delete `autotest_yaml()` and `autotest_single_yaml()` from
  `R/autotest-functions.R` if any remnants remain (git diff shows they were
  already removed on this branch — confirm and remove any leftover
  references). Grep `R/*.R` for any other still-defined function that is
  only reachable from these two (e.g. anything in
  `R/examples-to-yaml.R`/`R/parse-yaml.R`/`R/scrape-examples.R` no longer
  called by the trace-based path) and remove it if genuinely orphaned —
  but keep `examples_to_yaml()`/`parse_yaml()` if still exported/used
  elsewhere (they remain exported per `NAMESPACE`). Run
  `roxygen2::roxygenise()` to regenerate `NAMESPACE` and `man/*.Rd`,
  confirming `autotest_yaml` no longer appears in `NAMESPACE`.

## T001-5: Rewrite or remove obsolete yaml-pipeline tests
- [x] T001-5: `tests/testthat/test-yaml.R` calls `autotest_yaml()` and
  `autotest_single_yaml()`, both deleted — currently fails with
  `could not find function "autotest_yaml"`. Remove this file if its
  assertions are now meaningless (they test yaml-input validation that no
  longer applies), or rewrite it to test any yaml-related functionality
  that remains relevant (e.g. `examples_to_yaml()`/`parse_yaml()` directly,
  if still part of the public API). Review
  `tests/testthat/test-statspkg.R` for the same `autotest_yaml()` calls
  (lines using `x0 <-`/`x_t <-`/`x_f <-`/`x_f_file <-`/`x_t_file <-`
  `autotest_yaml(...)`) and rewrite them to exercise
  `autotest_package()`/`autotest_trace_package()` against the `stats`
  package instead, preserving the original intent of each assertion
  (test/no-test modes, filename vs in-memory yaml equivalence — adapt to
  trace-based equivalents or drop the assertion if it no longer has a
  trace-based analogue).

## T001-6: Reimplement `test_fns_wo_example()` against trace data
- [x] T001-6: In `R/untested-fns-and-params.R`, replace the yaml-text-based
  `test_fns_wo_example.character()` (and its use of `fns_without_examples()`
  from `R/namespace-processing.R`) with a version that determines functions
  without example coverage by comparing the full set of package functions
  (via `include_functions()`/`m_get_pkg_functions()` from
  `R/typetrace-package.R`) against the set of unique `fn_name` values
  present in example-sourced traces (`trace_source == "examples"`). Keep
  the same S3 dispatch shape (`test_fns_wo_example.NULL`/`.character`) and
  `report_object()` output shape (`type = "warning"`,
  `content = "This function has no documented example"`) so downstream
  consumers (`expect_autotest_notes`, etc.) are unaffected. Delete the now
  orphaned yaml-text helper `add_internal_fns_to_namespace()` if nothing
  else calls it after this change (confirm via grep).

## T001-7: Reimplement `test_untested_params()` against trace data
- [x] T001-7: In `R/untested-fns-and-params.R`, replace the yaml-text-based
  `untested_params()` (called from `test_untested_params.list()`) with a
  version that, for each traced function, compares `names(formals(fn))`
  against the set of parameter names actually present (named or
  positional) across that function's example-sourced traces
  (`trace_source == "examples"`, from `fn_pars`/`traces`), flagging any
  formal parameter (excluding `...`) never exercised. Preserve the
  `test_untested_params.NULL`/`.list` S3 dispatch shape and the
  `report_object()` output shape (`type = "warning"`,
  `test_name = "par_is_demonstrated"`,
  `content = "Examples do not demonstrate usage of this parameter"`) so
  `autotest_package()`'s output schema is unchanged.

## T001-8: Wire the reinstated checks back into `autotest_package()`
- [x] T001-8: In `R/autotest-functions.R`, uncomment/replace the two
  currently-commented-out lines in `autotest_package()`
  (`# res <- test_untested_params(exs, res)` and
  `# res <- test_fns_wo_example(package, res, names(exs))`) with calls to
  the trace-based reimplementations from T001-6/T001-7, passing whatever
  trace-derived arguments those functions now require (e.g. `traces`,
  `fn_pars`, `package` name) instead of the old `exs`/`names(exs)`
  yaml-derived arguments.

## T001-9: Get the full test suite green
- [x] T001-9: Run `devtools::load_all()` then
  `testthat::test_dir("tests/testthat")` (or `devtools::test()`) repeatedly,
  fixing failures until the suite passes. Specifically verify
  `tests/testthat/test-testthat-expectation.R`'s `expect_autotest` test
  passes: `autotest_package(package = "stats", functions = "cov")` should
  produce `expect_success(expect_autotest_no_err(x))` (no spurious error
  from test-suite-sourced traces, per T001-3) and
  `expect_failure(expect_autotest_no_warn(x))` (a warning about `cov`'s
  undemonstrated parameter usage should be present, per T001-6/T001-7).
  Also confirm `tests/testthat/test-local-pkg.R` and
  `tests/testthat/test_autotest.R` (or their current-branch equivalents)
  still pass unchanged.

## T001-10: Update `DESCRIPTION` and `codemeta.json`
- [x] T001-10: In `DESCRIPTION`, remove the `Remotes: mpadge/typetracer`
  line (the `trace_source`/`call_env` features relied on are already in
  typetracer's CRAN release, per `plan.md`). Confirm the `typetracer`
  entry under `Imports:` remains. Regenerate `codemeta.json` (e.g. via
  `codemetar::write_codemeta()` or the project's existing
  regeneration process, matching the current NAMESPACE/DESCRIPTION state
  from T001-4).

## T001-11: Final verification pass
- [x] T001-11: With T001-1 through T001-10 complete, run
  `devtools::check()` (or the project's standard `R CMD check` /
  pre-commit invocation) on the `typetracer` branch and confirm: no
  `ERROR`/`WARNING` related to the changes in this stage, the full test
  suite passes, and `git diff main...typetracer --stat` no longer shows
  any reference to the deleted yaml-pipeline functions. Do not merge or
  push — stop here and report readiness for the (separate, explicitly
  user-confirmed) merge into `main`.
