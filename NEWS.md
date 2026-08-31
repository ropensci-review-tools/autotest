# autotest 0.1.0

*2026-01-27 – 2026-08-31*

### New features

* Replaced the entire YAML/example-parsing pipeline that underpinned autotesting since 0.0.x with one built on
  [`typetracer`](https://github.com/mpadge/typetracer): `autotest_package()` now traces actual calls
  made in a package's examples, tests, and vignettes, rather than statically parsing and re-executing example code.
* Reimplemented the untested-function/untested-parameter checks on top of the new trace-based data.
* Added a `progress` parameter to `autotest_package()`, replacing `quiet`, for finer control over console output.
* Suppressed plot/PDF file artifacts and progress-bar noise generated while `autotest_package()` runs.

### Bug fixes and minor improvements

* Fixed formals lookup and package-reload gaps uncovered while merging the typetracer pipeline.
* Fixed resolution of unexported functions during autotesting.
* Fixed namespace-qualified (`pkg::fn`) call tracing.
* Fixed parameter parsing/cleanup left over from the old yaml-based `get_params`/`scrape-examples.R` code.
* Removed now-orphaned internals from the old pipeline (`yaml_handlers()`, `R/parse-yaml.R`, `R/example-objects.R`,
  `R/text-parsing-fns.R`, and other unused yaml-parsing code).
* Switched to `checkmate` for input assertion checks.
* Renamed the `package` parameter to `pkg` for consistency.
* Bumped roxygen2 to 8.1 and added `exportS3Method` tags.
* Updated the repo status badge to "active".

# autotest 0.0.2

*2021-02-23 – 2026-01-27*

### New features

* Added a first cut of `typetracer`-based tracing as an alternative example-execution backend, laying the
  groundwork for the 0.1.0 rewrite.
* Added `check_param_is_arbitrary()` to skip mutation-testing of parameters whose values are arbitrary characters.
* Added `documented_int_range()` and related handling so documented integer ranges (upper/lower, positive/negative)
  are properly tested.
* Added a hex sticker/logo for the package.
* Added a `CODE_OF_CONDUCT.md` and allcontributors setup.

### Bug fixes

* Fixed `autotest_single_yaml()` returning a 0-row data frame instead of `NULL` when no items in a report were
  testable.
* Fixed parsing of bracket sequences spanning multi-line quoted expressions.
* Fixed parsing of in-line code comments and quoted sequences containing escape characters (`remove_comments()`,
  `quote_sequences()`).
* Fixed handling of packages installed to non-standard library paths.
* Fixed `get_Rd_value()` to use `Rd2txt()` instead of manual parsing, and fixed related Rd-extraction bugs
  (`get_pkg_functions()`, `get_Rd_param()`, `rm_dontrun_lines()`).
* Fixed function-alias grepping so functions called via aliases are correctly identified.
* Fixed `single_or_vec()` to correctly evaluate parameter expressions and only treat atomic classes as vectors.
* Fixed the `double_noise` test for vectors containing `NA`s and adjusted its sensitivity threshold.
* Fixed `test_int_as_dbl` comparisons to be less sensitive to spurious differences.
* Fixed `split_content_at_commas()`, `rm_commas_in_qts()`, and `extract_primary_call_content()` for a range of
  edge cases (quoted strings, matrix results from `apply()`, expression delimiters).
* Fixed package-name/dependency extraction from `DESCRIPTION` (missing `Imports`, multi-line comma-separated
  fields, package name not on the first line).
* Fixed test-flag propagation across the various `input-*` test functions so `test = FALSE` is honoured
  consistently.

### Minor improvements

* Moved the project from the `ropenscilabs` to the `ropensci-review-tools` GitHub organisation and recommended
  r-universe installation.
* Migrated CI to GitHub Actions v2/v4 workflows and added a code-coverage workflow.
* Switched default status messages about assumed parameters from "assume" to "state" wording for clarity.
* Various internal refactors to reduce cyclomatic complexity and improve consistency of variable naming.

# autotest 0.0.1

*2020-07-03 – 2021-02-23*

Initial development of the package's core autotesting framework.

### New features

* Added the original YAML-based pipeline for scraping function-call examples from a package's `Rd` files and
  source, and converting them into a structured YAML test specification (`examples_to_yaml()` and friends).
* Added `autotest_rectangular()`, `autotest_vector()`, and `autotest_single()` to systematically mutate rectangular
  (data.frame/matrix), vector, and single-value (integer, double, character, logical, name/formula) inputs and
  check that functions respond sensibly (informative errors/warnings rather than silent failure).
* Added `autotest_package()` to run autotesting across every function in a package, including packages provided
  as local/source directories as well as installed packages.
* Added a structured `report_object`/tibble return value, with columns progressively built out to include
  `test_name`, `parameter_type`, `operation`, and `content`, plus a `summary()` method for the returned object.
* Added tests for whether function return values match their documented class and description
  (`test_return_has_class`, `test_return_desc`).
* Added tests for whether parameter documentation matches the classes of objects actually passed in examples
  (`param_docs_match_input`).
* Added detection of untested functions and parameters (`test_untested_params`), including identification of
  parameters that cannot be meaningfully tested and those left at their default values.
* Added an `autotest_obj` S3 class and moved dispatch of the rectangular/vector/single/return tests onto S3
  methods, replacing what had been long if/else chains.
* Added an `autotest_types` parameter/function to control which categories of test are run.
* Added a `test_data` parameter allowing user-supplied replacement values to be threaded through the rectangular,
  vector, and single-value tests instead of only auto-generated ones.
* Added `testthat` integration via `expect_autotest_no_err()`, `expect_autotest_no_warn()`,
  `expect_autotest_notes()`, and `expect_autotest_no_testdata()`.
* Added git-hash tracking of source packages in the report output.
* Added a package vignette describing autotest's design, plus a second vignette on controlling which tests run.

### Bug fixes and minor improvements

* Numerous fixes to the example-code parser: matching of nested/mismatched brackets, multi-line and piped
  expressions, in-line comments, quoted strings, `\%`-escaped operators, and `dontrun`/`donttest` blocks.
* Numerous fixes to matching parameter and return-object classes against their documented descriptions.
* Switched CI to run on GitHub Actions, added an `R CMD check` workflow across multiple OSes, and set up pkgdown.
