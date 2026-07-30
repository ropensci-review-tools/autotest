---
created: 2026-07-30T12:32:00Z
agent: claude-sonnet-5
git_hash: 7e0eaa3149491cd46607d83363014d1a508107e6
---

# Design Decisions: fix-unexported-fn-resolution

## Summary
Fixed `autotest_package()` erroring against `rjd3qr`/`JDCruncheR` with "object 'compute_score.QR_matrix' not found". Root cause was in `typetracer`: `inject_pkg_trace_fns()`/`uninject_pkg_trace_fns()` only resolved function names via the attached search-path environment, missing unexported functions that `autotest`'s own enumeration deliberately includes. End-to-end re-verification then surfaced and fixed a second, unrelated bug in `autotest`'s own code.

## New Design Decisions

### Decision 1: Resolve package functions via asNamespace(), not the attached environment
**Chosen:** `typetracer`'s `inject_pkg_trace_fns()`/`uninject_pkg_trace_fns()` now use `pkg_env <- asNamespace(package)` in place of `as.environment(paste0("package:", package))`, since the namespace environment contains both exported and unexported functions.
**Rationale:** `autotest`'s own `get_pkg_functions()` deliberately enumerates unexported functions too (e.g. S3 methods registered via `NAMESPACE`'s `S3method()` but not `export()`ed), via a `get()`-then-`getFromNamespace()` fallback; `typetracer`'s single-tier, attached-environment-only lookup couldn't find those same names once handed to it.
**Tradeoffs:** None material — simpler than the originally-planned `tryCatch`/`getFromNamespace()` fallback, and equally correct.

### Decision 2: Preserve the inject/uninject cache-naming invariant
**Chosen:** `uninject_pkg_trace_fns()`'s loop variable stays named `f`, matching `inject_pkg_trace_fns()`'s.
**Rationale:** `inject_tracer()`/`uninject_tracer()` cache each function's pre-injection body under a file name derived from `deparse(substitute(f))` at their own call site — an existing, fragile mechanism that silently fails to uninject (no error, just a no-op) if the call-site variable name differs between the two calls. A mid-implementation rename briefly broke this; caught by the new regression test and reverted.
**Tradeoffs:** None — this preserves pre-existing (if fragile) behavior rather than changing it.

### Decision 3: Fix a second, unrelated bug found during end-to-end verification
**Chosen:** `autotest_single_trace()`'s `int_val` data frame now builds `fn` via `rep(trace_data$fn_name, nrow(param_info))` rather than a bare scalar.
**Rationale:** A function whose parameters have no formal defaults, all omitted from a traced call (e.g. a documented example calling it with zero arguments and relying on internal `missing()` checks), traces every parameter as `par_eval = NULL`; `get_param_info()` filters all of these out, leaving a zero-row `param_info` for a function that nonetheless has one `fn_name`. The subsequent `data.frame(fn = trace_data$fn_name, ...)` then errored on the length mismatch (1 vs. 0).
**Tradeoffs:** None material.

## Integration with Prior Work
Continues stage 003's precedent of fixing genuine `typetracer` bugs at the source. Also continues stage 003's verification discipline explicitly: end-to-end re-verification against the real target package, not just the first fix passing, is what surfaced Decision 3.

## Issues Resolved
- `autotest_package()` erroring against `rjd3qr`/`JDCruncheR` (`script.R`): resolved, verified end-to-end (both `autotest`'s and `typetracer`'s full test suites pass; `autotest_package()` completes successfully).

## Deferred Items
- The `typetracer`-side fix and its regression test are complete and verified but were committed in that repository by a separate process outside this session (confirmed, same as stage 003) rather than by this agent, per instruction not to commit there directly.

## Process Notes
- The originally-planned fix (a `tryCatch`/`getFromNamespace()` two-tier fallback, mirroring `autotest`'s own pattern) was simplified to a direct `asNamespace()` replacement after the user proposed it mid-implementation.
- Writing the regression test itself surfaced a genuine, separate bug (Decision 2) introduced by an incidental variable rename — caught only by actually running an inject-then-uninject cycle, not by reasoning about the change alone.
- End-to-end verification (re-running `autotest_package()` against the real target package, per the stage 003 discipline) surfaced a second, unrelated `autotest`-side bug (Decision 3), escalated and confirmed with the user before fixing, rather than assumed in scope.
