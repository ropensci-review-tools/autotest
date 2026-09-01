---
created: 2026-09-01T13:25:04Z
agent: claude-sonnet-5
git_hash: 4180d2b22d96c1cc0b3a503bba57774b33145ac0
---

# Design Decisions: use-fs-for-paths

## Summary
Converted every path-construction, path-inspection, and path-listing call
across `R/` (7 files, ~41 sites) and `tests/testthat/test-statspkg.R` (4
sites) from base R to `fs`, promoting `fs` from an unused `Suggests` entry
to a hard `Imports` dependency, and fixed two latent behavioral bugs
surfaced by the conversion itself.

## New Design Decisions

### Full function-level mapping from base R to `fs`
**Chosen:** `file.path→fs::path`, `basename→fs::path_file`,
`dirname→fs::path_dir`, `normalizePath→fs::path_abs`,
`file.exists/dir.exists→fs::file_exists/fs::dir_exists`,
`file.remove→fs::file_delete`, `list.files→fs::dir_ls(..., fail = FALSE)`;
two manual `strsplit(x, .Platform$file.sep)[[1]]` + `tail(..., 1)` patterns
in `zzz.R` collapsed to `fs::path_file()`. `tempfile()`/`tempdir()` stay
base R — `fs` has no creation equivalent, only manipulation of paths that
already exist.
**Rationale:** One-to-one mapping keeps the diff mechanical and reviewable;
`fail = FALSE` reproduces `list.files()`'s existing tolerance of a missing
directory (several call sites list a package's `man/` directory, which may
legitimately not exist), needed because several call sites unconditionally
check a package's `man/` directory.
**Tradeoffs:** `fs_path` return values are not coerced back to plain
`character` — left to propagate, since they behave as character in the
comparisons/data frames used here.
**Proposed by:** joint

### Basename-comparison call sites needed explicit `fs::path_file()` wrapping
**Chosen:** Every site that compared `list.files()`'s bare-basename output
against literal filenames (e.g. `"DESCRIPTION" %in% list.files(package)` in
`zzz.R::dot_to_package()`) was rewritten as
`files %in% fs::path_file(fs::dir_ls(package, fail = FALSE))`, not a direct
`list.files→dir_ls` swap.
**Rationale:** `fs::dir_ls()` always returns full paths (there is no
`full.names = FALSE` equivalent), unlike `list.files()`, which returns
basenames by default. A naive swap would have silently broken package-root
detection for every relative/absolute path, since the membership test would
never match.
**Tradeoffs:** None — caught before landing, via targeted testing of
`dot_to_package(".")` from both the package root and a nested subdirectory.
**Proposed by:** agent

### Trace-file glob switched from an anchored `regexp` to `glob`
**Chosen:** `autotest-functions.R`'s trace-file lookup
(`list.files(dir, pattern = "^typetrace\\_.*\\.Rds$", full.names = TRUE)`)
became `fs::dir_ls(dir, glob = "*/typetrace_*.Rds", fail = FALSE)` rather
than the equivalent-looking `regexp = "^typetrace_.*\\.Rds$"`.
**Rationale:** `fs::dir_ls(regexp = ...)` matches against the *full path*,
not the basename, so the `^` anchor from the original `list.files()`
pattern never matched anything once a directory prefix was present — a
silent empty-result bug, caught by re-running `test-statspkg.R` (which
exercises `autotest_package()` end-to-end) and seeing it still pass with 16
assertions rather than trusting the conversion by inspection alone.
**Tradeoffs:** None once corrected.
**Proposed by:** agent

## Integration with Prior Work
Pure internal refactor with no interaction with the `typetracer` tracing
pipeline (stages 001–006) or the coverage work of stage 008. Continues the
project's practice (stages 001, 003, 007/008) of verifying behavioral
equivalence empirically rather than by code inspection alone, here via the
existing `test-statspkg.R` suite plus targeted ad hoc checks (relative-path
root detection, nested-directory detection, missing/empty `man/`
directories) and a full local `R CMD check`.

## Issues Resolved
- `fs` moved from unused `Suggests` to `Imports`; all base R path calls in
  `R/` and `tests/testthat/test-statspkg.R` replaced with `fs::` equivalents.
- Two conversion bugs (basename-vs-full-path mismatch; `regexp` vs `glob`
  anchoring) found and fixed before landing, not left latent.

## Deferred Items
None.

## Process Notes
- Both bugs above were only caught because each converted file was verified
  by re-running relevant tests rather than treating the mechanical mapping
  as self-evidently correct — `fs`'s API differs from base R in ways that
  are easy to miss (full-path-by-default listing, full-path regex matching).
