---
created: 2026-09-01T13:09:47Z
agent: claude-sonnet-5
git_hash: 95a45fdb24ea3c0ceebc8956f3d738b87a4ebd0a
---

# Plan: use-fs-for-paths

## Overview
Use 'fs' package for all path manipulations

## Context
`fs` is currently listed only in `Suggests`, unused by any `R/` source file.
All path handling is done with base R (`file.path`, `basename`, `dirname`,
`normalizePath`, `file.exists`, `dir.exists`, `list.files`, `file.remove`),
plus two spots that manually split paths with
`strsplit(x, .Platform$file.sep)`. Base path calls are spread across 7 files
in `R/` (heaviest in `namespace-processing.R` and `zzz.R`, ~41 call sites
total) plus 4 call sites in `tests/testthat/test-statspkg.R`.

The codebase's existing convention (established by `checkmate::`, `cli::`,
`data.table::`, etc.) is to call other packages with explicit `pkg::fn()`
rather than `@importFrom` + NAMESPACE imports — `fs::` calls will follow the
same pattern.

## Design Goals
- Every path-construction, path-inspection, and path-listing call in `R/`
  and `tests/testthat/` uses `fs::` instead of the base R equivalent.
- `fs` moves from `Suggests` to `Imports` in `DESCRIPTION`, since it becomes
  a hard runtime dependency rather than an optional/dev one.
- No behavioral regressions: package-root detection (`zzz.R`), namespace
  parsing (`namespace-processing.R`), and Rd lookup (`rd-lookup.R`) must
  continue to handle packages with a missing `man/` directory, and
  relative/absolute/trailing-slash path inputs, exactly as before.
- `tempfile()`/`tempdir()` calls stay as base R — `fs` has no replacement
  for temp-file/dir *creation* (only for manipulating paths once they
  exist), so these are out of scope.

## Proposed Approach
Function-level mapping, applied at each of the ~45 call sites across `R/`
and `tests/testthat/test-statspkg.R`:

| Base R | fs replacement | Notes |
|---|---|---|
| `file.path(...)` | `fs::path(...)` | direct swap |
| `basename(x)` | `fs::path_file(x)` | direct swap |
| `dirname(x)` | `fs::path_dir(x)` | direct swap |
| `normalizePath(x)` | `fs::path_abs(x)` | `path_abs()` doesn't require the path to exist, matching `normalizePath`'s tolerant behavior used in `zzz.R`'s root-search loop, which walks upward through possibly-nonexistent intermediate guesses |
| `file.exists(x)` | `fs::file_exists(x)` | direct swap |
| `dir.exists(x)` | `fs::dir_exists(x)` | direct swap |
| `file.remove(x)` | `fs::file_delete(x)` | `file_delete()` errors on a missing file rather than returning `FALSE`; acceptable here since every call site removes a `tempfile()` it just created itself |
| `list.files(dir, ...)` | `fs::dir_ls(dir, ..., fail = FALSE)` | `fail = FALSE` reproduces `list.files()`'s current behavior of returning `character(0)` (with a warning) rather than erroring when `dir` doesn't exist — needed because `man/` is checked unconditionally and may not exist |
| `strsplit(x, .Platform$file.sep)[[1]]` + `tail(..., 1)` (in `zzz.R::pkg_lib_path` and the `get_git_hash` neighborhood) | `fs::path_file(x)` | this manual splitting is exactly what `path_file()` does; replace both occurrences in `zzz.R` |

`fs_path` return values (an S3 class layered over character) are **not**
coerced back to plain `character` at call sites — they behave as character
in comparisons, `paste`/`sprintf`, and data frame columns, so existing
`==`, `%in%`, and string-building code keeps working unchanged. If any
downstream code turns out to depend on `class(x) == "character"` exactly,
that will surface as a test failure and be fixed at that specific site
rather than defensively coercing everywhere up front.

Per-file work:
- `R/zzz.R` (12 sites): package-root search loop, `pkg_is_source()`,
  `pkg_lib_path()` (including both manual path-splits), `get_git_hash()`.
- `R/namespace-processing.R` (13 sites): heaviest file; repeated
  `man_dir <- list.files(file.path(package, "man"), ...)` pattern appears
  3 times — convert each consistently.
- `R/rd-lookup.R` (6 sites), `R/utils.R` (4 sites), `R/autotest-functions.R`
  (3 sites), `R/methods.R` (1 site): direct swaps per the table above.
- `R/test-rect-fns.R` (2 sites): `file.remove(ftmp) # nolint` → 
  `fs::file_delete(ftmp) # nolint`, keep the existing lint-suppression
  comment since the return value is still discarded.
- `tests/testthat/test-statspkg.R` (4 sites): `file.path`, `file.exists`
  (×2), `file.remove` around the `Rplots.pdf` leak check.
- `DESCRIPTION`: move `fs` from `Suggests` to `Imports`.

After conversion, run the full test suite plus `R CMD check` locally to
confirm no regressions, particularly around package-root detection with
relative paths (`.`) and packages lacking a `man/` directory.

## Open Questions
None outstanding — scope, missing-directory handling (`fail = FALSE`), and
`fs_path` propagation (no coercion) were all resolved with the user before
writing this plan.
