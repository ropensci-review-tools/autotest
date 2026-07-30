---
created: 2026-07-30T10:21:38Z
agent: claude-sonnet-5
git_hash: 2a14b62dc6e107c926b8cb1ab7acea4eaa08bcc9
---

# Plan: fix-namespace-qualified-trace

## Overview
`script.R` reports that `autotest_package("../pkgstats")` claims test failures that `testthat::test_local("../pkgstats")` shows do not exist. Root-caused to a bug in the sibling `typetracer` package's tracer-injection code: it mishandles functions called with an explicit namespace prefix (`pkg::fn(...)`), which pkgstats uses deliberately inside `parallel::mclapply()` blocks. Fix the bug in `typetracer`, verify against `pkgstats`, and confirm `autotest_package()` and `testthat::test_local()` agree.

## Context
`autotest` depends on the sibling `typetracer` package (`Remotes: mpadge/typetracer` in `DESCRIPTION`) to trace function calls made in a package's examples and test suite (stage 001, `specs/001-merge-typetracer/`). Stage 001 already established the precedent of fixing genuine `typetracer` bugs directly in that sibling repository rather than working around them in `autotest` — it did so for a package-reinstall performance bug and an integer-probing safety bug, both left as explicit, deliberate fixes at the source. This stage follows the same precedent for a correctness bug (not performance/safety) surfaced by validating against a different real package, `pkgstats` (sibling directory `../pkgstats` relative to `autotest`), rather than `stats` as used in stage 001's validation.

Root cause, confirmed by direct reproduction in this session:
- `typetracer_header()` (`R/tracer-define.R` in the sibling `typetracer` package source), the code spliced into the head of every traced function's body, does:
  ```r
  typetracer_env$fn_call <- match.call (expand.dots = TRUE)
  typetracer_env$fn_name <- typetracer_env$fn_call [[1]]
  ...
  typetracer_env$fn <- match.fun (typetracer_env$fn_name)
  ```
- When the traced function is called as a bare symbol (`fn(...)`), `fn_call[[1]]` is a symbol and `match.fun()` works fine.
- When called with an explicit namespace prefix (`pkg::fn(...)`), `fn_call[[1]]` is instead a *call* to the `` `::` `` operator (i.e. `` `::`(pkg, fn) ``), not a bare symbol or string. `match.fun()` does not accept that form and throws: `` '<environment>$fn_name' is not a function, character or symbol ``.
- `pkgstats::pkgstats_fn_names()` is called this way — with the explicit `pkgstats::` prefix — inside `parallel::mclapply()` blocks in `R/cran-data-fn-names.R` and `R/cran-data-update.R` (a common defensive pattern for parallel workers, to guarantee namespace resolution inside forked children regardless of what's attached there). Confirmed by direct reproduction: calling the traced function as `pkgstats::pkgstats_fn_names(path)` throws the `match.fun` error immediately (even outside `mclapply`, in a single direct call); calling it unqualified (`pkgstats_fn_names(path)`, as `tests/testthat/test-fn-names.R` does) works fine.
- pkgstats' own code wraps each `pkgstats::pkgstats_fn_names(i)` call in `tryCatch (..., error = function (e) NULL)`, so under `autotest_package()` (which injects tracers into every function before running examples/tests) this error is silently swallowed and converted to `NULL`, corrupting `pkgstats_fns_from_archive()`'s results and failing several `expect_*()` assertions in `tests/testthat/test-archive-trawl.R` — assertions that never run under plain `testthat::test_local()`, since no tracer is injected there.

## Design Goals
1. Fix `typetracer_header()` in `typetracer`'s `R/tracer-define.R` so it correctly resolves the traced function regardless of whether it was called as `fn(...)`, `pkg::fn(...)`, or `pkg:::fn(...)` — i.e. normalize `fn_call[[1]]` to a bare function-name symbol before passing to `match.fun()`, rather than assuming it is always already a symbol.
2. Do not change tracing behavior/output for the already-working bare-symbol call form — this is a targeted fix for the namespace-qualified case, not a rewrite of the header logic.
3. Add regression coverage in `typetracer`'s own test suite (`tests/testthat/test-trace-fns.R` or similar) exercising a function traced and then called via explicit `pkg::fn(...)` syntax, so this doesn't silently regress.
4. Verify the fix resolves the original symptom end-to-end: reinstall the fixed `typetracer` from source (the currently-installed copy at `~/R/x86_64-pc-linux-gnu-library/4.6/typetracer`, version 0.2.3.5, is a plain installed copy, not dev-linked to the sibling source — reinstalling is required for the fix to take effect in this session), then re-run `autotest_package("../pkgstats")` and confirm it no longer reports the spurious `archive_trawl`/`pkgstats_fns_from_archive`-related failures, matching `testthat::test_local("../pkgstats")`.
5. Leave `autotest` itself unchanged — this bug and its fix are entirely within `typetracer`'s tracer-injection mechanism; `autotest` is only the vehicle that surfaced it via `script.R`.

## Proposed Approach
1. In the sibling `typetracer` package's `R/tracer-define.R`, modify `typetracer_header()`'s function-name resolution: after `typetracer_env$fn_call <- match.call (expand.dots = TRUE)`, detect whether `typetracer_env$fn_call[[1]]` is a call to `` `::` `` or `` `:::` `` (i.e. `is.call(...)` and its own `[[1]]` is one of those operators); if so, extract the function-name symbol from the third element of that call instead of using the raw `fn_call[[1]]` directly. Keep the existing behavior untouched for the plain-symbol case.
2. Add a test in `typetracer`'s own suite that defines a function inside a throwaway package-like namespace (or more simply, mimics the call pattern with an explicit `` `::` ``/`` `:::` `` call on an injected function) and confirms tracing succeeds where it previously threw `match.fun`'s error.
3. Run `typetracer`'s full test suite (`devtools::test()`) to confirm no regressions.
4. Reinstall `typetracer` from the fixed local sibling source into this session's library (e.g. `devtools::install()` run from that package's directory, or equivalent), since the currently-installed copy is a static install, not a dev/symlinked source.
5. Re-run `autotest_package(package = "../pkgstats")` and confirm the `test-archive-trawl.R`-derived spurious failures are gone.
6. Re-run `testthat::test_local("../pkgstats")` alongside, to confirm both now agree (both pass, or both report the same genuine findings — not one silently swallowing failures the other reveals).
7. Commit the `typetracer` fix in its own repository (a real source-code bug fix, not a deferred/uncommitted change this time, since `script.R` explicitly asks for the anomaly to be fixed, not just diagnosed).

## Open Questions
- Does `autotest`'s own `DESCRIPTION` (`Remotes: mpadge/typetracer`) need any change (e.g. pinning a commit) once the sibling repo fix is committed, or does it already track the default branch such that no change is needed here? To be confirmed once the sibling-repo commit exists — likely no change needed since `Remotes: mpadge/typetracer` with no `@ref` tracks the default branch, but worth a quick check before considering this stage fully closed.
- Should the same `` `::`/`:::` `` normalization also be applied anywhere else in `typetracer` that inspects `match.call()`/`sys.call()` results assuming a bare symbol (e.g. `process_back_trace()`'s `fn_name` matching against parsed call text in `R/tracer-define.R`)? Worth a quick check during implementation in case the same assumption recurs, but not expected to be a large addition.
