---
created: 2026-07-30T08:43:44Z
agent: claude-sonnet-5
git_hash: 1e2ee52769b3934ae024bd26ad13bb55a384e32b
---

# Plan: cleanup-param-parsing

## Overview
Clean up `get_params()` in `R/function-params.R` and all associated helper functions, because they are no longer needed after removal of YAML template stuff — expanded, per user direction, to also (a) rename the resulting file to reflect its narrowed purpose, and (b) audit and remove the equally-orphaned yaml-era example-scraping pipeline in `R/scrape-examples.R` and its cascading dead helpers in `R/namespace-processing.R`.

## Context
Stage 001 (`specs/001-merge-typetracer/`) completed the pivot from the yaml/example-text-parsing test pipeline to typetracer-based tracing, and deliberately removed the yaml-authoring/parsing entry points (`autotest_yaml()`, `autotest_single_yaml()`, `parse_yaml_template()`). A follow-up conversation after that stage found and removed further orphaned yaml-only code that stage 001 had missed:
- `R/parse-yaml.R` (`at_yaml_template()`) — deleted; confirmed zero consumers of the template it wrote.
- `R/examples-to-yaml.R` and `R/param-classes-vs-descrs.R` (`examples_to_yaml()`, `param_classes_in_desc()`, etc.) — deleted, then `R/param-classes-vs-descrs.R` was restored by the user for a closer look, but the restored functions (`param_classes_in_desc()`, `is_fn_a_constructor()`, `param_desc_is_other_fn()`) were confirmed to still only be reachable through the deleted yaml-only entry point, with no live callers anywhere in the current trace-based pipeline.

During that same investigation, `get_params()` in `R/function-params.R` was flagged as another such orphan, which is what this stage originally set out to address.

**Regression found and already fixed during this stage's investigation (not part of the remaining plan, noted for the record):** deleting `R/examples-to-yaml.R` had also deleted `preload_package()`, which turned out to be a genuinely live function called from `R/autotest-functions.R` (inside `autotest_package()` itself) and `R/typetrace-package.R` — i.e. the core pipeline was actually broken, not just carrying dead code. This was caught by trying to run `autotest_package()` directly (it errored with "could not find function preload_package") rather than by the static `grep` call-graph checks alone. `preload_package()` has been restored to `R/utils.R` (a self-contained function with no dependency on any other deleted yaml code), and `autotest_package(package = "stats", functions = "var")` has been re-verified to run successfully. Two other dangling references from that same deletion were found to *not* need restoring, because their only remaining callers are themselves dead code being removed in this stage: `exclude_functions()` (called only from the "get-examples" test in `test-statspkg.R`, which is being deleted below) and `get_fn_aliases()` (called only from `get_fn_exs()` in `R/scrape-examples.R`, also being deleted below).

This history is a direct argument for Design Goal 5 below: re-verify with actual execution, not just `grep`, before treating anything as safely dead.

## Design Goals
1. Remove `get_params()` and its exclusive helper cluster from `R/function-params.R`, since none of it is reachable from the live trace-based pipeline. Confirmed via `grep` that each of the following has zero callers anywhere in `R/` or `tests/` outside this cluster itself:
   - `get_params()`, `parse_backtick_lines()`, `rm_nth_backtick_pair()`, `fill_param_vals()`, `get_non_formula_val()`, `clean_final_pars_list()`, `get_param_descs_source()`
2. Preserve and rename the remainder of `R/function-params.R`, which is genuinely live:
   - `m_rd_db()` (memoised `tools::Rd_db()` wrapper, added in stage 001 to fix a multi-GB/multi-minute performance blowup)
   - `get_Rd_value()` — called from `R/test-return-object.R`
   - `get_Rd_param()` — called from `R/untested-fns-and-params.R`, `R/input-int.R`, `R/input-char.R`
   Since the file will then contain only Rd-value/Rd-param lookup helpers, rename `R/function-params.R` to `R/rd-lookup.R` (OQ#1 resolved: yes, rename).
3. Remove the entire dead yaml-era example-scraping pipeline in `R/scrape-examples.R` (OQ#2 resolved: yes, in scope). Confirmed via `grep` that every function in this file except `get_package_name()` has zero callers anywhere outside the file itself: `get_all_examples()`, `get_fn_exs()`, `get_example_lines()`, `get_example_lines_installed()`, `get_example_lines_source()`, `load_all_if_needed()`, `remove_comments()`, `preprocess_example_lines()`, `clean_example_lines()`, `find_fn_call_points()`, `process_fn_calls()`, `split_ex_by_fn_calls()`, `rm_seed_calls()`, `dispatched_fns()`, `join_at_operators()`, `rm_examples_if()`, `merge_piped_lines()`, `merge_fn_defs()`, `single_clause()`, `multi_line_quotes()`, `rm_dontrun_lines()`, `rm_not_parseable()`, `rm_plot_lines()`, `rm_enclosing_brackets()`, `transform_single_quotes()`.
   - `get_all_examples()`'s only caller anywhere was the deleted `examples_to_yaml()` (confirmed via `git show` on the commit that deleted `R/examples-to-yaml.R`) plus the "get-examples" test in `tests/testthat/test-statspkg.R`.
   - `get_fn_exs()` additionally calls a function, `get_fn_aliases()`, that no longer exists anywhere in the codebase (it lived in the now-deleted `R/examples-to-yaml.R`) — meaning this code path is not just unreached but actually broken if it were ever called. Strong independent confirmation this whole chain is dead.
   - `get_package_name()` is the one live exception: it's called from `R/namespace-processing.R` (inside `get_pkg_functions()`, itself reachable live via `m_get_pkg_functions()` from `R/typetrace-package.R`) and from `R/example-objects.R`. It must be preserved, moved into `R/namespace-processing.R` alongside the other package-introspection helpers it's used with.
4. Remove the cascading dead code this exposes in `R/namespace-processing.R`: `is_pkg_same()`, `topic_to_fns()`, and `m_topic_to_fns()`. Their only callers are, respectively, the dead `get_all_examples()`/`topic_to_fns()` chain and each other — once `R/scrape-examples.R`'s dead chain is removed, these become fully unreachable too. (`get_pkg_functions()`, `m_get_pkg_functions()`, `fns_from_other_pkgs()`, `fns_without_examples()`, `fns_to_topics()`, `m_fns_to_topics()` all remain — confirmed live via `R/typetrace-package.R`, `R/untested-fns-and-params.R`, `R/function-params.R`/`R/rd-lookup.R`, and `R/test-return-object.R`.)
5. Remove the now-dead "get-examples" test block in `tests/testthat/test-statspkg.R` (tests `exclude_functions()` and `get_all_examples()`, both gone).
6. Confirm no NAMESPACE/man changes are needed anywhere in this stage: none of the removed functions carry roxygen export tags.
7. Verify by actual execution, not just `grep`, at every step — per the regression found above, re-run `devtools::load_all()` and the full test suite (not just the affected test files) after each deletion, and specifically re-run `autotest_package()` against a real package (e.g. `stats::var`) as an end-to-end smoke test before considering this stage done.

## Proposed Approach
1. In `R/function-params.R`: delete the dead cluster (`get_params()`, `parse_backtick_lines()`, `rm_nth_backtick_pair()`, `get_param_descs_source()`, `fill_param_vals()`, `get_non_formula_val()`, `clean_final_pars_list()`), keep `m_rd_db()`/`get_Rd_value()`/`get_Rd_param()`, then rename the file to `R/rd-lookup.R` (`git mv`).
2. In `R/namespace-processing.R`: add `get_package_name()` (moved from `R/scrape-examples.R`), remove `is_pkg_same()`, `topic_to_fns()`, `m_topic_to_fns()`.
3. Delete `R/scrape-examples.R` entirely (all remaining content is dead once `get_package_name()` is moved out).
4. Update any callers of `get_package_name()` (`R/example-objects.R`, and the newly-relocated `get_pkg_functions()`/`preload_package()` in `R/namespace-processing.R`/`R/utils.R`) — should be a no-op since it's the same function, just check no file-local (unexported, same-file-only) helper naming collisions arise from the move.
5. Delete the "get-examples" `test_that()` block in `tests/testthat/test-statspkg.R`.
6. Re-run the full `grep`-based call-graph check across all touched symbols to make sure nothing was missed.
7. `devtools::load_all()` to confirm the package loads without error.
8. Run the full test suite (`devtools::test()`), not just the touched test files.
9. Smoke-test `autotest_package(package = "stats", functions = "var", test = FALSE)` end-to-end as a final real-execution check, mirroring the check that caught the `preload_package()` regression.

## Open Questions
None remaining — both prior open questions were resolved by the user (rename `R/function-params.R`; bring the `R/scrape-examples.R` audit into scope) and are now incorporated into the Design Goals and Proposed Approach above.
