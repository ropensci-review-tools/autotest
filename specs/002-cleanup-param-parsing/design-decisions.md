---
created: 2026-07-30T09:25:00Z
agent: claude-sonnet-5
git_hash: b616f5f44383760c64d9a5c42609f7be62ef5b76
---

# Design Decisions: cleanup-param-parsing

## Summary
Completed the removal of orphaned yaml-era code left behind by the typetracer migration (stage 001): the dead `get_params()` helper cluster, the entire `R/scrape-examples.R` example-scraping pipeline, and their cascading dependents, were deleted; the remaining Rd-lookup file was renamed to reflect its narrowed purpose; and a real regression from an earlier cleanup pass in this same session was caught and fixed.

## New Design Decisions

### Decision 1: Whether code is actually reachable, not authorship intent, decides what's dead
**Chosen:** Every symbol slated for removal was first confirmed via `grep` to have zero callers anywhere in `R/` or `tests/` outside the cluster being removed, tracing transitively back to a live entry point (`autotest_package()`, `autotest_trace_package()`) rather than relying on file-level or comment-level assumptions about purpose.
**Rationale:** A prior pass in this same session had deleted a whole file on the assumption its contents were uniformly yaml-only, which silently removed a genuinely live function (see Decision 3).
**Tradeoffs:** Slower than bulk deletion by file, but necessary given the demonstrated failure mode.

### Decision 2: R/function-params.R renamed to R/rd-lookup.R
**Chosen:** Once the dead `get_params()` cluster was stripped, the file contained only Rd-value/Rd-param lookup helpers (`m_rd_db()`, `get_Rd_value()`, `get_Rd_param()`), so it was renamed accordingly.
**Rationale:** File name should reflect current contents, not its pre-typetracer origin.
**Tradeoffs:** None material.

### Decision 3: Verification requires actual execution, not static analysis alone
**Chosen:** `devtools::load_all()`, the full test suite, and a direct `autotest_package()` smoke test are now required after any dead-code removal in this area, in addition to `grep`-based call-graph checks.
**Rationale:** Deleting `R/examples-to-yaml.R` earlier in this session had also deleted `preload_package()`, which turned out to be called live from `autotest_package()` itself — a real break in the core pipeline that static `grep` review alone did not catch before the file was deleted, and that was only caught by attempting to run `autotest_package()` directly. `preload_package()` was restored (into `R/utils.R`).
**Tradeoffs:** None material — the cost of running the test suite is negligible next to the cost of a silent regression.

## Integration with Prior Work
Directly continues stage 001's typetracer migration and the yaml-pipeline removal it began. The yaml pipeline is now fully gone: no yaml-authoring entry points, no yaml-generation code, and no yaml-era example-text-scraping pipeline remain anywhere in the package.

## Issues Resolved
- Dead `get_params()` cluster in `R/function-params.R`: removed.
- Entire dead example-scraping pipeline in `R/scrape-examples.R` (24 of 25 functions, all unreachable since typetracer replaced text-based example scraping): removed; the one live function, `get_package_name()`, relocated to `R/namespace-processing.R`.
- Cascading dead code exposed in `R/namespace-processing.R` (`is_pkg_same()`, `topic_to_fns()`, `m_topic_to_fns()`): removed.
- Regression: `preload_package()` deleted along with `R/examples-to-yaml.R` in an earlier pass this session, breaking `autotest_package()`: caught and fixed.

## Deferred Items
None — both open questions from `plan.md` were resolved and folded into this stage's scope before implementation.

## Process Notes
- The scope of this stage expanded materially mid-planning: what began as a single-function cleanup grew to include a full second dead pipeline (`R/scrape-examples.R`) once a systematic audit was requested, illustrating that yaml-era orphans were more extensive than initially assumed.
- `get_fn_exs()` (in the removed `R/scrape-examples.R`) called `get_fn_aliases()`, a function that no longer existed anywhere in the codebase — independent, non-`grep` confirmation that the chain was already broken, not merely unreached.
