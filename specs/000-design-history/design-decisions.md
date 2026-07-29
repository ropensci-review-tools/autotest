---
created: 2026-07-29T11:15:16Z
agent: claude-sonnet-5
git-hash: bf8409515660cbfeaf6be143472bf7988745fe43
---

# Design History

`autotest` is an R package (rOpenSci review-tools org) that automatically
generates and runs tests for a target package's exported and internal
functions, driven by the parameter descriptions and examples in that
package's documentation. Development spans 2020-07-03 to present (~1100
commits), overwhelmingly authored by mpadge, with smaller contributions from
helske, Maëlle Salmon, Simon Parker, Jouni Helske, and Mauro Lepore.

## Project Evolution

**Phase 1 — YAML-driven parameter model (Jul–Sep 2020).** The project began
as a way to parse a package's documented parameter descriptions into a
structured YAML representation (`parse_yaml`, `yaml_template`). This YAML
schema — recording each parameter's name, class, and documented
constraints — became the backbone that all later testing logic reads from.

**Phase 2 — Core autotest engine and S3 class hierarchy (Sep 2020–early
2021, the highest-volume period, ~193 commits in Dec 2020 alone).** The
single `autotest` entry point was built out (`R/autotest.R`), followed by a
significant refactor introducing an `autotest_obj` S3 class that replaced an
earlier ad hoc `rect_test` class, plus a full family of S3 methods for
`test_return_*`, `test_single_*`, and `autotest_vector`/`autotest_rectangular`
generics (commits `e9c4f86`, `b0b64fd`, `42af230`, `b07aaa6`, `90c98ec`).
This established the pattern still in use: input-type-specific test
generators (`input-int.R`, `input-char.R`, `input-double.R`,
`input-logical.R`, `input-name.R`) each paired with corresponding
`test-*.R` files, dispatched through S3 methods rather than large
conditionals. Example-scraping (`scrape-examples.R`,
`examples-to-yaml.R` — the two most-edited files in the repo's history)
and function-parameter/class matching (`function-params.R`,
`param-classes-vs-descrs.R`) were hardened heavily during this phase, which
also accounts for the largest share of bug-fix commits.

**Phase 3 — Maturation and low-frequency maintenance (2021–2025).** Commit
volume dropped sharply after early 2021 and the project settled into
occasional bug fixes and incremental refinements (e.g. `split_content_at_commas`
edge cases, `param_classes_from_ex`, class-restriction handling for
`autotest_rectangular`), version-bumped to 0.1.0, with long gaps between
releases (single-digit commit months through 2022–2025).

**Phase 4 — `typetracer` integration and package-level testing (2026,
issue #76, current `typetracer` branch).** A new `autotest_package` function
was introduced, built on `typetracer` (a separate runtime type-tracing
mechanism) rather than purely static documentation parsing — `R/typetrace-package.R`,
`function-param-types.R` restructured "to get types from traces". This is
the most recent architectural extension, moving part of the parameter-class
inference from documentation text toward runtime-observed types, with
`include_functions`/`exclude` parameters added to scope which functions are
traced.

**Phase 5 — Tooling/style modernization (2026, most recent commits).**
A repo-wide "spaceout" pass reformatted nearly every R file for consistent
spacing, CI/pre-commit config was refreshed, a `push-to-elsewhere` workflow
was added, and roxygen2 was updated to v8 with `exportS3Method` tags — all
non-functional maintenance rather than design changes.

## Key Decisions

- **YAML as the intermediate representation for documented parameter
  constraints.** Rather than testing directly against parsed Rd files,
  documentation is first normalized into YAML (`parse_yaml`,
  `examples-to-yaml.R`). This decouples the (fragile, R-version-dependent)
  documentation-parsing step from the test-generation logic, and explains
  why `examples-to-yaml.R` and `scrape-examples.R` are by far the
  most-frequently-touched and most-frequently-fixed files — they absorb
  the bulk of edge-case handling for real-world package documentation.

- **S3 class/generic dispatch over conditional branching.** The `autotest_obj`
  class (replacing the earlier `rect_test` class, commit `e9c4f86`) and its
  associated S3 methods for each input type (int, double, char, logical,
  name, vector, rectangular) were adopted early (commit `b07aaa6`, "finish
  all S3 methods for #20") as the extensibility mechanism for adding new
  parameter/test types, and this pattern has been preserved through all
  later feature work.

- **Separate `autotest_rectangular`/`autotest_vector` code paths.** Rectangular
  (data-frame-like) and vector inputs were split into distinct test
  generators and S3 method families early on, reflecting that rectangular
  inputs need structural checks (column classes, dimensions) that plain
  vectors don't.

- **Introduction of runtime type-tracing (`typetracer`) for package-level
  testing (2026, #76).** `autotest_package` extends the original
  per-function/documentation-driven model with a runtime approach: tracing
  actual argument types as functions are exercised, rather than relying
  solely on parsed documentation text. This is an explicit widening of the
  original design's scope from "test what's documented" toward "test what's
  observed," while keeping the existing YAML/S3 machinery downstream.

- **CI/quality tooling investment over new features in later years.**
  The project's low-commit-volume years (2022–2025) were spent almost
  entirely on maintenance (R-CMD-check, test-coverage workflows,
  pre-commit config) rather than feature growth, consistent with a
  "concept"/stable-maintenance repostatus rather than active feature
  development, until the 2026 typetracer work reopened active development.
