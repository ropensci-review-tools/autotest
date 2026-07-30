---
created: 2026-07-30T09:02:36Z
agent: claude-sonnet-5
git_hash: 1e2ee52769b3934ae024bd26ad13bb55a384e32b
---

# Tasks: cleanup-param-parsing

## T002-1: Strip the dead helper cluster out of R/function-params.R
- [x] T002-1: In `R/function-params.R`, delete `get_params()`, `parse_backtick_lines()`, `rm_nth_backtick_pair()`, `get_param_descs_source()`, `fill_param_vals()`, `get_non_formula_val()`, and `clean_final_pars_list()` (and their header comments). Retain `m_rd_db()`, `get_Rd_value()`, and `get_Rd_param()` untouched — these remain live, called respectively from within this file, `R/test-return-object.R`, and `R/untested-fns-and-params.R`/`R/input-int.R`/`R/input-char.R`.

## T002-2: Rename R/function-params.R to R/rd-lookup.R
- [x] T002-2: Once T002-1 leaves the file containing only `m_rd_db()`, `get_Rd_value()`, and `get_Rd_param()`, run `git mv R/function-params.R R/rd-lookup.R` to reflect its narrowed purpose (Rd-value/Rd-param lookup only).

## T002-3: Move get_package_name() out of R/scrape-examples.R into R/namespace-processing.R
- [x] T002-3: `get_package_name()` is the one function in `R/scrape-examples.R` that is still live (called from `R/namespace-processing.R`'s `get_pkg_functions()` and from `R/example-objects.R`). Before deleting `R/scrape-examples.R` (T002-5), relocate `get_package_name()`'s definition into `R/namespace-processing.R`, placed near `get_pkg_functions()` which uses it. Do not change its implementation.

## T002-4: Remove the now-dead is_pkg_same()/topic_to_fns()/m_topic_to_fns() from R/namespace-processing.R
- [x] T002-4: In `R/namespace-processing.R`, delete `is_pkg_same()`, `topic_to_fns()`, and `m_topic_to_fns()`. These become fully unreachable once `R/scrape-examples.R` (their only external caller, via the dead `get_all_examples()`/`find_fn_call_points()` chain) is deleted in T002-5. Confirm via `grep -rn` across `R/` and `tests/` that no other reference to these three names remains before deleting. Do not touch `get_pkg_functions()`, `m_get_pkg_functions()`, `fns_from_other_pkgs()`, `fns_without_examples()`, `fns_to_topics()`, or `m_fns_to_topics()` — all confirmed live.

## T002-5: Delete R/scrape-examples.R
- [x] T002-5: After T002-3 has relocated `get_package_name()` out, delete `R/scrape-examples.R` in full. It contains only dead yaml-era example-scraping code: `get_all_examples()`, `get_fn_exs()`, `get_example_lines()`, `get_example_lines_installed()`, `get_example_lines_source()`, `load_all_if_needed()`, `remove_comments()`, `preprocess_example_lines()`, `clean_example_lines()`, `find_fn_call_points()`, `process_fn_calls()`, `split_ex_by_fn_calls()`, `rm_seed_calls()`, `dispatched_fns()`, `join_at_operators()`, `rm_examples_if()`, `merge_piped_lines()`, `merge_fn_defs()`, `single_clause()`, `multi_line_quotes()`, `rm_dontrun_lines()`, `rm_not_parseable()`, `rm_plot_lines()`, `rm_enclosing_brackets()`, `transform_single_quotes()` — none has a live caller, and `get_fn_exs()` calls a function (`get_fn_aliases()`) that no longer exists anywhere in the codebase, confirming the chain is not just unreached but broken.

## T002-6: Remove the dead "get-examples" test block from tests/testthat/test-statspkg.R
- [x] T002-6: Delete the `test_that ("get-examples", { ... })` block in `tests/testthat/test-statspkg.R` (currently calls `exclude_functions()` and `get_all_examples()`, both removed by T002-5 and the prior stage's cleanup). Leave the other `test_that()` blocks in this file (`"autotest var"`, `"autotest rnorm"`) untouched.

## T002-7: Re-run the full grep call-graph check across all touched symbols
- [x] T002-7: After T002-1 through T002-6, re-run `grep -rn` for every removed symbol name (`get_params`, `parse_backtick_lines`, `rm_nth_backtick_pair`, `get_param_descs_source`, `fill_param_vals`, `get_non_formula_val`, `clean_final_pars_list`, `get_all_examples`, `get_fn_exs`, `get_example_lines`, `get_example_lines_installed`, `get_example_lines_source`, `load_all_if_needed`, `remove_comments`, `preprocess_example_lines`, `clean_example_lines`, `find_fn_call_points`, `process_fn_calls`, `split_ex_by_fn_calls`, `rm_seed_calls`, `dispatched_fns`, `join_at_operators`, `rm_examples_if`, `merge_piped_lines`, `merge_fn_defs`, `single_clause`, `multi_line_quotes`, `rm_dontrun_lines`, `rm_not_parseable`, `rm_plot_lines`, `rm_enclosing_brackets`, `transform_single_quotes`, `is_pkg_same`, `topic_to_fns`, `m_topic_to_fns`) across `R/` and `tests/` to confirm zero remaining references anywhere in the codebase.

## T002-8: Verify the package loads cleanly
- [x] T002-8: Run `devtools::load_all()` (or equivalent) and confirm it completes with no errors or new warnings after all deletions and the file rename/move.

## T002-9: Run the full test suite
- [x] T002-9: Run the complete test suite (`devtools::test()` or equivalent), not just `test-statspkg.R`, and confirm no failures or new errors relative to the pre-stage baseline.

## T002-10: Smoke-test autotest_package() end-to-end
- [x] T002-10: Run `autotest_package(package = "stats", functions = "var", test = FALSE)` directly (mirroring the check that caught the `preload_package()` regression earlier this stage) and confirm it completes successfully and returns a non-empty result, as a real-execution check beyond static `grep` analysis.
