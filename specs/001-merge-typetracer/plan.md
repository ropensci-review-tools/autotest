---
created: 2026-07-29T11:15:16Z
agent: claude-sonnet-5
git_hash: bf8409515660cbfeaf6be143472bf7988745fe43
---

# Plan: merge-typetracer

## Overview
Merge the `typetracer` branch into `main`: wire up typetracer's
`trace_source`/`call_env` output to distinguish example-derived traces (safe
to use for mutation/fuzz testing) from test-suite-derived traces (used only
for parameter type inference), remove the obsolete yaml/example-parsing
pipeline and its now-broken tests, restore functional parity for
untested-function/untested-parameter warnings against the new trace-based
data, and get the full test suite passing so the branch can merge into
`main` per [issue #76](https://github.com/ropensci-review-tools/autotest/issues/76).

## Context

- `autotest` (this repo) was originally built around a "miles of code"
  pipeline that scrapes each function's documented `@examples`, converts them
  to a structured YAML template (`examples_to_yaml`/`parse_yaml`), and drives
  mutation-style testing (`autotest_yaml` → `autotest_single_yaml`) from that
  YAML. See `specs/000-design-history/design-decisions.md` for the full
  history of that pipeline (Phases 1–3).
- Issue #76 proposed replacing that parsing pipeline with `typetracer`
  (github.com/mpadge/typetracer, also maintained by mpadge), which injects
  instrumentation into a package's functions and records the actual
  argument values/types/classes seen when the package's examples and/or test
  suite are executed, removing the need to parse example source text at all.
  This repo's `000-design-history` records this as "Phase 4" of the
  project's evolution.
- Work on the `typetracer` branch already replaced the core pipeline:
  `R/typetrace-package.R` (new) calls `typetracer::trace_package()`, and
  `R/autotest-functions.R` was rewritten so `autotest_package()` now calls a
  new `autotest_single_trace()` per trace file instead of the deleted
  `autotest_yaml()`/`autotest_single_yaml()`. `R/function-param-types.R` was
  rewritten to derive parameter type/class/storage-mode from trace data
  (`get_param_info()`) instead of by inspecting parsed YAML
  (`get_param_types`, `single_or_vec`, `double_or_int` — all deleted).
- The `typetracer` branch diff against `main` is currently 18 commits /
  ~10 files (`DESCRIPTION`, `NAMESPACE`, `R/autotest-functions.R`,
  `R/function-param-types.R`, `R/typetrace-package.R`, plus generated docs).
  `DESCRIPTION` adds `typetracer` as an `Imports` dependency with
  `Remotes: mpadge/typetracer` (not yet on CRAN).
- Issue #76's second comment (mpadge) identifies the blocking problem: many
  more calls get traced under the new approach, including calls from within
  a package's own test suite that are deliberately written to trigger
  errors (e.g. `expect_error(...)`). Typetracer had no way to tell such
  calls apart from documentation examples, so autotest was reporting
  spurious errors. The comment's proposed fix: modify `typetracer` to expose
  the calling environment, at least for `testthat`, so trace provenance can
  be identified.
- Investigation during this planning session confirmed the user's
  recollection: `typetracer` (sibling package source, installed version
  0.2.3.5, matching local source `0.2.3.005`) has since been extended to
  solve exactly this. `trace_package()` accepts a `types` argument
  (`"examples"`, `"tests"`, or both — default both), tags each batch of
  traces via `add_trace_source(traces, "examples"|"tests")`, and
  `load_traces()` returns a `trace_source` column plus a `call_env` column
  (derived from `call_envs`, which walks the call stack including nested
  environments like testthat expectations). **None of this is currently
  used anywhere in `autotest`'s R code** (`grep` for `trace_source`/`call_env`
  in `R/*.R` returns nothing) — the branch calls `typetracer::trace_package()`
  with default `functions`/`exclude` only, and never filters or inspects
  trace provenance downstream in `get_unique_fn_pars()`, `get_param_info()`,
  or `autotest_single_trace()`. This is the concrete, currently-missing
  piece needed to resolve the issue.
- Running the existing test suite on the `typetracer` branch surfaces two
  concrete regressions versus `main`, both consistent with the above:
  - `tests/testthat/test-yaml.R` and `tests/testthat/test-statspkg.R` call
    `autotest_yaml()`/`autotest_single_yaml()`, which no longer exist
    (`could not find function "autotest_yaml"`) — these tests were never
    updated for the pipeline rewrite.
  - `tests/testthat/test-testthat-expectation.R` (`expect_autotest` test)
    fails: it expects `autotest_package(package = "stats", functions = "cov")`
    to report both an error (`expect_autotest_no_err` should succeed) and a
    warning about undemonstrated parameter usage (`expect_autotest_no_warn`
    should fail, i.e. a warning is expected to be present) — but on this
    branch neither condition currently holds as expected. This traces
    directly to `R/autotest-functions.R`'s `autotest_package()` currently
    having `test_untested_params()`/`test_fns_wo_example()` calls commented
    out, so the warning about `cov`'s undocumented-in-examples parameter
    usage is no longer generated.

## Design Goals

1. **Correct trace provenance handling.** `autotest_package()` must use
   typetracer's `trace_source` (and `call_env` where needed for finer
   testthat-specific filtering) so that:
   - Traces from `"tests"` are used only to enrich parameter type/class
     information (feeding `get_unique_fn_pars()`/`get_param_info()`).
   - Actual mutation/fuzz autotesting (`autotest_rectangular`,
     `autotest_vector`, `autotest_single`, `autotest_return`) is driven only
     by traces with `trace_source == "examples"`, eliminating the spurious
     errors described in issue #76.
2. **No dead/obsolete code paths.** The yaml/example-text-parsing pipeline
   (`autotest_yaml`, `autotest_single_yaml`, and any now-orphaned helper
   functions only reachable from them) is fully removed, not left half
   deleted. Its removal is confirmed intentional (see prior clarification),
   so no backward-compatible `autotest_yaml()` shim is needed.
3. **No functional regression versus `main`.** `test_fns_wo_example()` /
   `test_untested_params()` (or trace-based equivalents that produce the
   same class of warnings: functions with no example coverage, parameters
   never exercised by any traced example call) are reinstated so
   `autotest_package()` output has parity with `main`'s behaviour for
   these checks.
4. **Green test suite.** All currently-relevant tests pass on the merged
   branch: obsolete yaml-pipeline tests (`test-yaml.R`, relevant parts of
   `test-statspkg.R`) are rewritten to exercise the trace-based path or
   removed if they test behaviour that no longer applies;
   `test-testthat-expectation.R` passes once provenance filtering and
   untested-parameter parity are restored.
5. **Mergeable dependency story.** The `trace_source`/`call_env` features
   being relied on are already in `typetracer`'s CRAN release, so
   `DESCRIPTION`'s `Remotes: mpadge/typetracer` line can simply be dropped.

## Proposed Approach

1. **Confirm typetracer's provenance API is sufficient as-is.** Read
   `trace-package.R`/`load-and-clear-traces.R`/`tracer-define.R` in the
   sibling `typetracer` source in full (only skimmed so far) to verify
   `trace_source` and `call_env` reliably distinguish example vs. test
   calls for the cases autotest cares about, including nested/indirect
   calls (e.g. a helper called from within a testthat block). If a gap is
   found, patch `typetracer` in parallel (the user has confirmed this
   sibling package can be modified as part of this work), then bump
   autotest's dependency pin.
2. **Thread `trace_source` through `R/typetrace-package.R` and
   `R/autotest-functions.R`.** `autotest_trace_package()` keeps calling
   `typetracer::trace_package()` requesting both `"examples"` and `"tests"`
   types (needed for parameter-type enrichment from test-derived traces).
   `get_unique_fn_pars()` continues to use all traces for type/class
   aggregation. `autotest_package()`'s loop over trace files is changed to
   only build `autotest_obj`/run mutation tests
   (`autotest_rectangular`/`vector`/`single`/`return`) for trace files whose
   `trace_source == "examples"`; test-sourced trace files still contribute
   to `fn_pars` but are skipped for the mutation-testing loop.
3. **Remove the yaml pipeline fully.** Delete `autotest_yaml`,
   `autotest_single_yaml`, and confirm no other exported/internal function
   still depends on them (`get_param_types`, `single_or_vec`,
   `double_or_int` already gone). Regenerate `NAMESPACE`/`man/*.Rd` via
   roxygen2. Remove or rewrite `tests/testthat/test-yaml.R` and the
   yaml-specific parts of `tests/testthat/test-statspkg.R` to instead
   exercise `autotest_package()`/`autotest_trace_package()` directly.
4. **Reinstate untested-function/untested-parameter checks on trace data.**
   Reimplement `test_fns_wo_example()` (functions in the package namespace
   with zero associated example-sourced traces) and `test_untested_params()`
   (formals of a function never seen as a named/positional parameter across
   that function's example-sourced traces) against `fn_pars`/`traces`
   instead of parsed yaml text, replacing the commented-out calls in
   `autotest_package()`. `R/untested-fns-and-params.R`'s yaml-text-parsing
   internals (`untested_params()`, `add_internal_fns_to_namespace()`) are
   replaced accordingly; the `test_fns_wo_example.character`/`.NULL` and
   `test_untested_params.NULL`/`.list` S3 dispatch shape can likely be kept.
5. **Run and fix the full test suite** (`devtools::test()` /
   `testthat::test_dir("tests/testthat")`), iterating until green, paying
   particular attention to `test-testthat-expectation.R`'s `stats::cov`
   scenario as the concrete regression test for goals 1 and 3.
6. **Update `DESCRIPTION`.** The `trace_source`/`call_env` features are
   already in the CRAN release of `typetracer`, so drop the
   `Remotes: mpadge/typetracer` line (no longer needed) and regenerate
   `codemeta.json`.
7. Only after the above is green, prepare the branch for merge into `main`
   (this stage covers getting it mergeable; the actual merge/PR is a
   separate, later action requiring explicit user confirmation).

## Open Questions

- ~~Does `call_env` (not just `trace_source`) need to be consulted
  anywhere...~~ **RESOLVED (T001-1):** `call_env` is not needed.
  `trace_source` ("examples"/"tests") alone is sufficient to exclude all
  testthat-sourced traces from mutation testing, which fully addresses the
  spurious-error problem from issue #76 regardless of whether a given
  test-sourced call sits inside `expect_error()` or a plain assertion. Note
  the important nuance that typetracer's individual per-call RDS trace
  files (read via `readRDS(trace_files[i])`) retain `$trace_source`, but
  the *combined* data.frame returned by `trace_package()` itself
  (`add_pkg_trace_sources()` in typetracer's `trace-package.R`) strips
  `trace_source` and replaces it with `source_file_name` instead — so
  filtering must happen against the individual trace files, not the
  combined `traces` object used for `fn_pars`.
- Should `test_fns_wo_example`/`test_untested_params` warnings be based
  only on `trace_source == "examples"` traces (parity with old
  example-only yaml pipeline), or should a function/parameter touched only
  by the test suite (never by an example) still count as "tested" for this
  warning's purposes? Current lean: keep parity with `main`, i.e. base
  these checks on example-sourced traces only, consistent with goal 1's
  example/test split.
- ~~`R/typetrace-package.R`'s `autotest_trace_package()` currently
  sets/unsets `Sys.setenv("TYPETRACER_LEAVE_TRACES", "true")`...~~
  **RESOLVED (T001-1):** `types = c("examples", "tests")` is already
  typetracer's default, so `autotest_trace_package()` is already
  requesting both trace types today — passing it explicitly is a
  self-documenting no-op, not a behavioural change. The
  `TYPETRACER_LEAVE_TRACES` env-var handling in `uninject_pkg_trace_fns()`
  is independent of `types` and unaffected either way. Separately
  confirmed: for the package-name-only path (no local `pkg_dir`), "tests"
  tracing already silently no-ops (typetracer requires source `pkg_dir`
  for test tracing, since installed packages generally lack a
  `tests/` dir) — this is typetracer's documented, expected behaviour, not
  a bug to fix here.
