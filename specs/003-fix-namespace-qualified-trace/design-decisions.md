---
created: 2026-07-30T12:12:00Z
agent: claude-sonnet-5
git_hash: e23c47590345d1509db01a803148fcd932a944c4
---

# Design Decisions: fix-namespace-qualified-trace

## Summary
Root-caused and fixed the reason `autotest_package()` reported test failures against `pkgstats` that `testthat::test_local()` did not: typetracer's tracer-injection header made four related, incorrect assumptions about the shape of a traced function's call site, each triggered by patterns `pkgstats` genuinely uses (namespace-qualified self-calls inside `parallel::mclapply()`, and running an instrumented closure via `callr::r_bg()`).

## New Design Decisions

### Decision 1: Normalize namespace-qualified call heads before match.fun()
**Chosen:** `typetracer_header()` now detects when `match.call()[[1]]` is a call to `` `::` ``/`` `:::` `` (i.e. the traced function was invoked as `pkg::fn(...)`) and extracts the bare function-name symbol before passing it to `match.fun()`.
**Rationale:** `pkgstats` deliberately calls its own functions with an explicit namespace prefix inside `parallel::mclapply()` workers (a common defensive pattern); `match.fun()` cannot resolve a `::`-call directly, and `pkgstats`'s own `tryCatch(..., error = function(e) NULL)` around these calls silently converted the resulting error into `NULL`, corrupting downstream results.
**Tradeoffs:** None material — the bare-symbol call path is untouched.

### Decision 2: Fall back to tempdir() when typetracedir is unset
**Chosen:** `typetracer_header()` falls back to `tempdir()` when the `typetracedir` option is unset, matching `get_typetrace_dir()`'s existing behavior elsewhere in the package.
**Rationale:** `typetracedir` is set in `.onLoad()`, so is unset in a context where `typetracer` itself was never loaded — exactly what happens when an already-instrumented closure is serialized and executed inside a fresh `callr::r_bg()` subprocess (used by `pkgstats` to isolate `pkgstats()` runs).
**Tradeoffs:** Trace data produced inside such a subprocess is written to that subprocess's own (distinct) tempdir and is not recoverable by the parent process; acceptable here since the goal is only that the instrumented function still executes and returns a correct result, not that every execution's trace is collected.

### Decision 3: Guard against a function value as call head
**Chosen:** Where a call head resolves to a function value directly (which `callr`'s internal invocation mechanism does, equivalent to `do.call(<closure>, args)`) rather than a name, a fixed placeholder (`"<unknown>"`) is used for anywhere a character name is required; `match.fun()` already accepts a function value unchanged, so is unaffected.
**Rationale:** There is no reliable way to recover the original call-site name of a function passed as a raw value.
**Tradeoffs:** Traces produced this way record `fn_name` as `"<unknown>"` rather than the real name.

### Decision 4: Apply the same normalization inside process_back_trace()
**Chosen:** `process_back_trace()`'s own frame-walking loop (over the full call stack from `rlang::trace_back()`) had the identical bug independently — extracting a call's head via `as.name()` without handling `::`/`:::`-qualified calls or function values — and was fixed the same way.
**Rationale:** Any frame in the back-trace, not just the entry call, can be namespace-qualified; this was only discovered by re-verifying against `pkgstats` after Decisions 1–3 still left one failure unresolved.
**Tradeoffs:** None material.

## Integration with Prior Work
Continues stage 001's typetracer integration and its established precedent (Decision 4 there) of fixing genuine `typetracer` bugs at the source rather than working around them in `autotest`. Unlike stage 001's deferred install-caching fix, `script.R` explicitly requested this anomaly be fixed, not just diagnosed — however, per explicit instruction partway through this stage, the `typetracer`-side changes remain uncommitted in that repository for now (see Process Notes).

## Issues Resolved
- `autotest_package("../pkgstats")` vs `testthat::test_local("../pkgstats")` discrepancy (`script.R`): resolved — both now agree, verified directly.

## Deferred Items
- The four `typetracer` fixes and their new regression tests are verified working (full `typetracer` test suite passes; `autotest_package()`/`test_local()` parity confirmed against `pkgstats`) but left uncommitted in the sibling `typetracer` repository, per explicit instruction. Committing and releasing them there is a follow-up outside this repo.
- `autotest`'s own `DESCRIPTION` (`Remotes: mpadge/typetracer`, no `@ref` pin) needs no change regardless of when/whether the sibling fix is eventually committed and pushed, since it tracks the default branch.

## Process Notes
- Scope expanded significantly during implementation: the plan anticipated one root cause (namespace-qualified calls); verification against the real target package surfaced three further related defects, each only visible by re-running the actual end-to-end reproduction after each fix rather than trusting static analysis of the change.
- Two mid-implementation decision points were escalated for explicit confirmation before continuing (the second and third newly-discovered defects), consistent with the practice established in stage 001.
