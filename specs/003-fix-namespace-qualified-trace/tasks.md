---
created: 2026-07-30T10:24:03Z
agent: claude-sonnet-5
git_hash: 2a14b62dc6e107c926b8cb1ab7acea4eaa08bcc9
---

# Tasks: fix-namespace-qualified-trace

## T003-1: Fix typetracer_header() to handle namespace-qualified call forms
- [ ] T003-1: In the sibling `typetracer` package's `R/tracer-define.R`, in `typetracer_header()`, after `typetracer_env$fn_call <- match.call (expand.dots = TRUE)` and `typetracer_env$fn_name <- typetracer_env$fn_call [[1]]`, add logic to detect whether `typetracer_env$fn_name` is a call to `` `::` `` or `` `:::` `` (i.e. `is.call(typetracer_env$fn_name)` and `identical(typetracer_env$fn_name[[1]], quote(\`::\`))` or `quote(\`:::\`)`). If so, replace `typetracer_env$fn_name` with the third element of that call (the bare function-name symbol) before it's used in `typetracer_env$fn <- match.fun (typetracer_env$fn_name)`. Leave the plain-symbol call path (`fn(...)`) completely unchanged.

## T003-2: Check process_back_trace() for the same unqualified-symbol assumption
- [ ] T003-2: In the same file, review `process_back_trace()` (used later in `typetracer_header()` via `typetracer_env$process_back_trace(trace_dat, typetracer_env$fn_name)`) to confirm it still works correctly now that `fn_name` from T003-1 is always a bare symbol/string rather than potentially a `::`/`:::` call — this function matches `fn_name` against parsed call text (`SYMBOL`/`SYMBOL_FUNCTION_CALL` tokens) via `any (fns == fn_name)`, which should already expect a bare name; confirm no further change is needed here, or make one if it is.

## T003-3: Add regression test for namespace-qualified traced calls
- [ ] T003-3: In the sibling `typetracer` package's `tests/testthat/` directory, add a test (in `test-trace-fns.R` or a new file) that defines a simple function, injects the tracer via `inject_tracer()`, then calls it using an explicit `` `::` ``-qualified form (e.g. construct and evaluate a call equivalent to `pkgname::fn(...)`, or — if easier to set up without a real installed test package — directly construct and `eval()` a call object of the form `` call("::", as.name("base"), ...) `` style equivalent that reaches the same `fn_call[[1]]` shape) and asserts that tracing succeeds (`load_traces()` returns a result, no error thrown), where before the fix in T003-1 it would have thrown `match.fun`'s "not a function, character or symbol" error. Also add/keep a companion assertion that the existing bare-symbol call form still traces correctly (no regression).

## T003-4: Run typetracer's full test suite
- [ ] T003-4: Run `devtools::test()` (or equivalent) in the sibling `typetracer` package and confirm the new test from T003-3 passes and no existing test regresses.

## T003-5: Reinstall the fixed typetracer package into the session library
- [ ] T003-5: Reinstall `typetracer` from the fixed local sibling source (e.g. `devtools::install()` run from that package's directory, or `R CMD INSTALL`) so the fix takes effect in this session — the currently-installed copy (version 0.2.3.5) is a plain installed copy, not dev-linked to the sibling source, so edits to the source alone do not take effect without reinstalling.

## T003-6: Re-verify autotest_package() against pkgstats matches testthat::test_local()
- [ ] T003-6: Re-run `autotest_package(package = "../pkgstats")` from within the `autotest` repo (mirroring `script.R`) and confirm it no longer reports the spurious `archive_trawl`/`pkgstats_fns_from_archive`-related failures identified during planning (the `expect_s3_class`/`expect_equal`/`expect_identical` failures in `test-archive-trawl.R` caused by `pkgstats::pkgstats_fn_names()` traced calls returning `NULL`). Also re-run `testthat::test_local("../pkgstats")` alongside and confirm both now agree (no discrepancy between traced and untraced runs).

## T003-7: Commit the typetracer fix in its own repository
- [ ] T003-7: Once T003-1 through T003-6 pass, commit the fix (source change + new regression test) in the sibling `typetracer` repository with a clear commit message describing the namespace-qualified-call bug and fix. This is a real, intentional fix per `script.R`'s explicit request — not a deferred/uncommitted change.

## T003-8: Check whether autotest's own DESCRIPTION needs any change
- [ ] T003-8: Check autotest's `DESCRIPTION` `Remotes: mpadge/typetracer` entry (no `@ref` pin) — confirm it tracks the default branch of the sibling repo such that no change is needed here now that the fix is committed there, per the plan's open question. If it turns out a pin or version bump is needed, make that change; otherwise explicitly confirm no change is required and note why.
