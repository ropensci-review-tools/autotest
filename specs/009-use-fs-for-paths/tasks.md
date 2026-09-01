---
created: 2026-09-01T13:15:00Z
agent: claude-sonnet-5
git_hash: 95a45fdb24ea3c0ceebc8956f3d738b87a4ebd0a
---

# Tasks: use-fs-for-paths

## T009-1: Move `fs` from Suggests to Imports
- [ ] T009-1: In `DESCRIPTION`, remove `fs` from the `Suggests:` block and add
  it (alphabetically) to the `Imports:` block, since it becomes a hard
  runtime dependency once `R/` calls `fs::` directly.

## T009-2: Convert R/zzz.R to fs
- [ ] T009-2: In `R/zzz.R` (12 sites), replace: `list.files()` → `fs::dir_ls(..., fail = FALSE)`
  in the package-root search loop; `normalizePath()` → `fs::path_abs()` in
  that same loop and in `pkg_lib_path()`'s `root = TRUE` branch;
  `file.path()` → `fs::path()` throughout (`pkg_is_source()`'s `need_these`,
  the `".."` walks, `get_git_hash()`'s `.git` check); `file.exists()` →
  `fs::file_exists()` and `dir.exists()` → `fs::dir_exists()` in
  `pkg_is_source()` and `get_git_hash()`. Replace both manual
  `strsplit(x, .Platform$file.sep)[[1]]` + `tail(..., 1)` patterns (in
  `pkg_lib_path()`, once for `package` and once inside the `vapply` over
  `searchpaths()`) with `fs::path_file(x)`.

## T009-3: Convert R/namespace-processing.R to fs
- [ ] T009-3: In `R/namespace-processing.R` (13 sites), replace all
  `file.path()` → `fs::path()`, `basename()` → `fs::path_file()`. Convert
  the three repeated `list.files(file.path(package, "man"), ...)` sites
  consistently to `fs::dir_ls(fs::path(package, "man"), ..., fail = FALSE)`
  so a package without a `man/` directory still yields `character(0)`
  instead of erroring.

## T009-4: Convert R/rd-lookup.R to fs
- [ ] T009-4: In `R/rd-lookup.R` (6 sites), replace `file.path()` →
  `fs::path()` and `basename()` → `fs::path_file()` at each call site
  (the `.Rd` path construction and the two `basename(package) == package`
  checks).

## T009-5: Convert R/utils.R to fs
- [ ] T009-5: In `R/utils.R` (4 sites), replace `basename()` →
  `fs::path_file()` (both occurrences), `file.path()` → `fs::path()`, and
  `normalizePath()` → `fs::path_abs()` in the `lib.loc = normalizePath(file.path(package, ".."))`
  call.

## T009-6: Convert R/autotest-functions.R to fs
- [ ] T009-6: In `R/autotest-functions.R` (3 sites), replace `list.files()`
  → `fs::dir_ls(..., fail = FALSE)` (the `trace_files` listing),
  `file.path()` → `fs::path()`, and `basename()` → `fs::path_file()`.

## T009-7: Convert R/methods.R to fs
- [ ] T009-7: In `R/methods.R`, replace the single `file.path(pkg_name, "DESCRIPTION")`
  call with `fs::path(pkg_name, "DESCRIPTION")`.

## T009-8: Convert R/test-rect-fns.R to fs
- [ ] T009-8: In `R/test-rect-fns.R`, replace both
  `chk <- file.remove (ftmp) # nolint` sites with
  `chk <- fs::file_delete (ftmp) # nolint`, keeping the existing
  lint-suppression comment since the return value is still discarded.

## T009-9: Convert tests/testthat/test-statspkg.R to fs
- [ ] T009-9: In `tests/testthat/test-statspkg.R`, replace
  `file.path (getwd (), "Rplots.pdf")` → `fs::path (getwd (), "Rplots.pdf")`,
  both `file.exists (rplots)` checks → `fs::file_exists (rplots)`, and
  `file.remove (rplots)` → `fs::file_delete (rplots)`.

## T009-10: Verify no regressions
- [ ] T009-10: Run the full test suite (`devtools::test()`) and
  `R CMD check` locally. Specifically confirm: package-root detection still
  works when `package = "."` (relative path) and when called from a nested
  subdirectory; `namespace-processing.R`/`rd-lookup.R` still handle a
  package lacking a `man/` directory without erroring; no `fs_path`-class
  values leak into user-facing output (e.g. printed data frames) in a way
  that changes their appearance. Fix any call site whose behavior changed
  under the new `fs::` calls.
