---
created: 2026-07-30T11:56:25Z
agent: claude-sonnet-5
git_hash: 9a49259c7fb06168643defcf09d54063e89929e7
---

# Tasks: fix-unexported-fn-resolution

## T005-1: Fix inject_pkg_trace_fns() to fall back to getFromNamespace()
- [x] T005-1: In the sibling `typetracer` package's `R/trace-package.R`, in `inject_pkg_trace_fns()`, change `f <- get (fnm, envir = pkg_env)` to first try `get(fnm, envir = pkg_env)` and, if that fails (e.g. via `tryCatch`), fall back to `utils::getFromNamespace(fnm, package)`. This mirrors the two-tier lookup `autotest`'s own `get_pkg_functions()` (`R/namespace-processing.R`) already uses to enumerate function names, including unexported ones (e.g. S3 methods registered via `NAMESPACE`'s `S3method()` but not `export()`ed).

## T005-2: Apply the identical fix to uninject_pkg_trace_fns()
- [x] T005-2: In the same file, apply the identical two-tier lookup fix to `uninject_pkg_trace_fns()`'s equivalent line, `f <- get (f, envir = pkg_env)`. This function has the same single-tier limitation and would fail the same way during clean-up once T005-1 allows injection of unexported functions to succeed.

## T005-3: Add regression test coverage for unexported function resolution
- [x] T005-3: In the sibling `typetracer` package's `tests/testthat/` directory, add a test that exercises `inject_pkg_trace_fns()`/`uninject_pkg_trace_fns()` (or the full `trace_package()` path, whichever more directly and realistically exercises the fix) against a function that is registered but not exported — e.g. an S3 method declared via `S3method()` in a real installed package's `NAMESPACE` but not directly `export()`ed (following stage 003's precedent of using a real external target package like `rematch` rather than fabricating one, if a suitable unexported S3 method exists there or in another already-available test dependency; otherwise construct a minimal analogous case). Confirm the test fails without the fix (i.e. reproduces the "object not found" error) and passes with it.

## T005-4: Run typetracer's full test suite
- [x] T005-4: Run `devtools::test()` (or equivalent) in the sibling `typetracer` package and confirm the new test from T005-3 passes and no existing test regresses.

## T005-5: Reinstall the fixed typetracer package into the session library
- [x] T005-5: Reinstall `typetracer` from the fixed local sibling source (e.g. `devtools::install()` run from that package's directory) so the fix takes effect in this session, since the installed copy is a plain install, not dev-linked to the sibling source.

## T005-6: Re-verify autotest_package() against rjd3qr/JDCruncheR end-to-end
- [x] T005-6: Re-run `autotest_package(package = "/data/forks/rjd3qr")` (mirroring `script.R`) and confirm it completes successfully without error, returning a non-empty result. This is an end-to-end check, not just confirmation that the originally-reported `compute_score.QR_matrix` error is gone — per the verification discipline established in stage 003, a fix at the first-encountered error site is not sufficient evidence the whole run now succeeds.

## T005-7: Escalate if verification surfaces anything beyond the planned fix
- [x] T005-7: If T005-6 surfaces any further error or discrepancy beyond the one this stage set out to fix, stop and confirm scope with the user before making any additional changes, rather than assuming it is automatically in scope for this stage (per the plan's Design Goal 6 and Open Questions).

## T005-8: Confirm no changes are committed in the typetracer repository
- [x] T005-8: After all fixes and verification are complete, confirm via `git -C /data/mega/code/repos/pre-processing-r/typetracer status --short` that all changes made there (source fix, new test, any snapshot updates) remain as uncommitted working-tree modifications. Do not run `git add` or `git commit` in that repository under any circumstances for this stage, regardless of `autotest`'s own `.designlens.json` `auto_commit` setting, which applies only to `autotest`'s own retrospective/specs commits.
