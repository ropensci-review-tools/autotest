---
created: 2026-07-30T11:53:53Z
agent: claude-sonnet-5
git_hash: f4520f1df837bdef44ce6cb43fe0ea806d459aed
---

# Plan: fix-unexported-fn-resolution

## Overview
`script.R` runs `autotest_package(package = "/data/forks/rjd3qr")` (package `JDCruncheR`) and errors with `Error in get(fnm, envir = pkg_env): object 'compute_score.QR_matrix' not found`. Root-caused to a mismatch between how `autotest` enumerates a package's functions (deliberately including unexported internal functions, e.g. S3 methods registered via `NAMESPACE`'s `S3method()` but not directly exported) and how the sibling `typetracer` package's `inject_pkg_trace_fns()`/`uninject_pkg_trace_fns()` resolve function names (only via the attached `package:X` search-path environment, which does not contain unexported objects). Fix in `typetracer`, per instruction — but do not commit there.

## Context
`autotest_trace_package()` (`R/typetrace-package.R`) calls `include_functions()` → `m_get_pkg_functions()` → `get_pkg_functions()` (`R/namespace-processing.R`) to build the list of function names to trace when the user doesn't restrict `functions`/`exclude`. `get_pkg_functions()` enumerates candidate names from documented Rd-file *aliases* (which include unexported S3 methods documented under a shared topic, e.g. `compute_score.QR_matrix` is `\alias`ed alongside the exported generic `compute_score` in `JDCruncheR`'s own `man/compute_score.Rd`), then classifies each name using a two-tier lookup: `get(i, envir = <attached package env>)` first, falling back to `utils::getFromNamespace(i, package)` if that fails. This means `get_pkg_functions()` correctly identifies `compute_score.QR_matrix` as a real function (via the `getFromNamespace()` fallback) even though it isn't exported, and includes it in the returned function list — this appears to be deliberate, so that internal/unexported functions (a very common case for S3 methods, which are frequently registered but not exported) also get traced and tested, not just the package's public API.

That function list is passed down through `typetracer::trace_package(..., functions = functions)` to `inject_pkg_trace_fns()`, which does *not* have the same two-tier lookup — it only does `get(fnm, envir = as.environment(paste0("package:", package)))`, with no `getFromNamespace()` fallback. Confirmed by direct reproduction: with `preload_package()`'s actual loading mode for source packages (`devtools::load_all(package, export_all = FALSE)`, confirmed by re-reading `R/utils.R`), `compute_score.QR_matrix` is not visible in the attached `package:JDCruncheR` environment (`exists(..., inherits = FALSE)` is `FALSE`, `get()` there throws), but `utils::getFromNamespace("compute_score.QR_matrix", "JDCruncheR")` succeeds and returns the function. `uninject_pkg_trace_fns()` has the identical single-tier lookup and would hit the same problem once tracing succeeds (during clean-up).

This continues the precedent established in stage 003: genuine `typetracer` bugs affecting `autotest`'s operation are fixed at the source in the sibling repo, not worked around in `autotest`. Per explicit instruction for this stage, the fix is made in `typetracer` but **not committed** there — the change is left as an uncommitted working-tree modification in that repo, mirroring how stage 003's fix was ultimately left (before it was separately committed outside this session).

## Design Goals
1. `inject_pkg_trace_fns()` in `typetracer`'s `R/trace-package.R` must be able to resolve and inject a tracer into any function name it's given, whether or not that function is exported — mirroring the two-tier lookup (`get()` from the attached environment, falling back to `utils::getFromNamespace()`) that `autotest`'s own `get_pkg_functions()` already uses to enumerate such names in the first place.
2. `uninject_pkg_trace_fns()` needs the identical fix, since it has the same single-tier lookup and would fail the same way during clean-up once injection of unexported functions succeeds.
3. `inject_tracer()`/`uninject_tracer()` themselves need no change: they mutate the closure's internals in place (confirmed in stage 003), so resolving a function via `getFromNamespace()` instead of the attached environment has no bearing on whether the tracer injection/removal itself works correctly.
4. Verify end-to-end, not just at the single call site that currently errors: after the fix, re-run `autotest_package(package = "/data/forks/rjd3qr")` (mirroring `script.R`) in full and confirm it completes successfully, consistent with the verification discipline established in stage 003 (a single fix that resolves the first-encountered error is not sufficient evidence the whole run now succeeds).
5. Make the fix in the sibling `typetracer` repository (`/data/mega/code/repos/pre-processing-r/typetracer`), but leave it **entirely uncommitted** there — no `git add`/`git commit` in that repo under any circumstances for this stage, regardless of `autotest`'s own `auto_commit` setting (which only ever applies to `autotest`'s own retrospective/specs commits, never to the sibling repo).
6. No changes to `autotest`'s own code are anticipated — `get_pkg_functions()`'s existing behaviour (including unexported functions via the `getFromNamespace()` fallback) is correct and deliberate; the mismatch is entirely on `typetracer`'s side. If verification (Goal 4) surfaces something that does require an `autotest`-side change, treat that as a new finding to confirm with the user before proceeding, not an assumed part of this plan.

## Proposed Approach
1. In `typetracer`'s `R/trace-package.R`, change `inject_pkg_trace_fns()`'s function-resolution line (`f <- get (fnm, envir = pkg_env)`) to fall back to `utils::getFromNamespace(fnm, package)` when the plain `get()` fails (e.g. via `tryCatch`), matching the two-tier pattern already used in `autotest`'s `get_pkg_functions()`.
2. Apply the identical fix to `uninject_pkg_trace_fns()`'s equivalent line.
3. Run `typetracer`'s full test suite (`devtools::test()`) to confirm no regressions, and add a regression test exercising an unexported function name (e.g. an S3 method registered via `S3method()` but not exported, similar to how stage 003 added a test using `rematch::re_match` as a real external target rather than a fabricated one).
4. Reinstall `typetracer` from the fixed local source into this session's library so the fix takes effect (as in stage 003 — the installed copy is a plain install, not dev-linked).
5. Re-run `autotest_package(package = "/data/forks/rjd3qr")` end-to-end and confirm it completes successfully.
6. Leave all `typetracer` changes (source, test, snapshot if any) as uncommitted working-tree modifications in that repo — do not stage or commit anything there.
7. If step 5 surfaces any further issue, pause and confirm scope with the user before continuing, per the same escalation discipline used in stage 003.

## Open Questions
- None currently — the root cause, fix location, and verification path are all confirmed. If end-to-end verification (step 5) surfaces something new, that will be raised explicitly rather than assumed in-scope.
