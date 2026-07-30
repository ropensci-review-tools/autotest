---
created: 2026-07-30T14:22:01Z
agent: claude-sonnet-5
git_hash: 0c702721cdd64161a5bdefc50570018c59290ee2
---

# Plan: suppress-test-artifacts

## Overview
Suppress plot/PDF artifacts and progress-bar ANSI noise generated when autotest_package() executes example/mutation code, so that 'make knitr' (root and vignettes) produces clean README/vignette renders with no leftover PNG/Rplots.pdf files or terminal escape codes

## Context
Triggered by running `make knitr`, which re-renders `README.Rmd` and leaves 9 untracked
`README-stats-var-*.png` files in the repo root plus literal ANSI escape noise
(`...14% | ETA: 12s[KTesting functions...`) baked into the rendered `README.md`. Neither
artifact existed in the last committed `README.md` — both are new consequences of the
`typetracer` branch's recent work: commit `7e7a24c` ("catch bug in ... when no data") means
`stats::cor`/`cov`/`var`'s example code now actually executes further and triggers real base
graphics calls it previously errored out of before reaching, and stage
[[004-fix-progress-reporting]] added the `cli::cli_progress_bar()`-based `progress` parameter
(now default `"bar"`) that stage's own design-decisions.md flagged as *not* emitting message
conditions "in non-interactive contexts (confirmed by direct testing)" — but that testing
did not cover `rmarkdown::render`/knitr, where cli's tty-detection can still see a real
terminal at the file-descriptor level even though knitr has redirected R-level output via
`sink()`/`textConnection`, so the dynamic progress bar still renders and gets captured as
literal text.

Traced two independent places where arbitrary package/example code actually executes as a
side effect of `autotest_package()`, either of which can trigger stray plotting output
(and, for real-world packages, the `Rplots.pdf`-in-cwd residue the user separately flagged
from experience testing other packages):
- `R/example-objects.R::example_objects()` (~line 137): `source(tmp, ...)` runs a package's
  scraped `.Rd` example code verbatim to derive parameter types. Runs unconditionally,
  regardless of `test = TRUE/FALSE` — this is why plots/messages already appear in the
  `test = FALSE` README chunk today.
- `R/capture-fn-output.R::log_all_msgs()` (~lines 17-23): `do.call`/`eval` actually invokes
  mutated parameter combinations when `test = TRUE`. Called from 9 other files via
  `catch_all_msgs()`, i.e. this is the single choke point for all real test execution.

Also confirmed the same `fig.path = "README-"` knitr option (clearly copy-pasted, and
nonsensical as a name outside the README) is duplicated in both `vignettes/autotest.Rmd`
and `vignettes/autotest-control.Rmd`, both of which also call `autotest_package(test = TRUE)`
in several chunks and are therefore exposed to the identical bug. `vignettes/autotest.md`
and `vignettes/autotest-control.md` (currently untracked) are pure local-preview byproducts
of `vignettes/makefile`'s `knitr` target — `git ls-files vignettes/` confirms they have
never been tracked, and that makefile's `clean` target only removes `*.html`/`*.png`, not
`*.md`.

## Design Goals
- `make knitr`, run from a clean checkout in both the repo root and `vignettes/`, leaves
  zero untracked PNG/PDF/markdown artifacts behind afterward, and produces a re-render
  identical (modulo timestamps) to what's already committed.
- No literal ANSI/terminal-escape noise (e.g. the `[K` clear-line sequences currently
  visible) appears in any knitted `README.md` or vignette markdown output.
- The fix is general, not README-specific: any user running `autotest_package()` on an
  arbitrary real-world package — interactively, in CI, or under `R CMD check` — should
  never have an incidental `Rplots.pdf` or similar device file dropped in their working
  directory as an unrequested side effect of that package's own example code plotting.
- Applied consistently to both current knit-artifact-producing locations (root `README.Rmd`
  and the two `vignettes/*.Rmd` files) rather than patching the README alone and leaving
  the vignettes with a known-latent copy of the same bug.

## Proposed Approach

**A. Null-device wrapping at both real-code-execution choke points.** Introduce one small
shared helper (e.g. `with_null_device(expr)`, likely in `R/capture-fn-output.R` or a new
`R/utils.R`) that opens a discarding graphics device (`grDevices::pdf(nullfile())`) around
an expression and guarantees `on.exit(grDevices::dev.off(), add = TRUE)`. Apply it at both:
  - the `source(tmp, ...)` call in `example_objects()` (runs regardless of `test=`), and
  - the `eval(call(...))`/`do.call(...)` call in `log_all_msgs()` (runs when `test = TRUE`).
  R's graphics device stack is LIFO, so opening/closing a device here correctly nests under
  and restores whatever device was already active — including knitr's own chunk-recording
  device — with no special-casing needed for the knitr context versus any other caller.
  This is the root-cause fix for both the README PNG generation *and* the general
  `Rplots.pdf` residue the user has hit testing other packages.

**B. Knitr-aware progress-bar suppression.** In `autotest_package()`
(`R/autotest-functions.R`), when `progress == "bar"`, additionally check
`isTRUE(getOption("knitr.in.progress"))` and fall back to `"none"` in that case. This
targets the actual cause (fd-level tty detection surviving knitr's R-level output
redirection) directly, rather than trying to harden cli's own tty heuristics.

**C. Vignette parity cleanup.** Since (A) suppresses plot generation at the source, the
`fig.path = "README-"` option in both `vignettes/autotest.Rmd` and
`vignettes/autotest-control.Rmd` becomes dead, misleadingly-named configuration; remove it
from both and let knitr use its own per-file default.

**D. Build hygiene backstops.** Update root `makefile`'s `clean` target and
`vignettes/makefile`'s `clean` target to also remove any stray `Rplots.pdf` and rendered
`*.md` (vignettes only — root `README.md` is intentionally tracked) as a defensive net, and
add a `.gitignore` entry for `vignettes/*.md` since those have never been meant to be
tracked.

**E. One-time repo cleanup.** Delete the 9 currently-untracked `README-stats-var-*.png`
files once (A)/(B) are verified, then re-run `make knitr` to confirm the regenerated
`README.md` is clean and matches expectations (no new PNGs, no escape-code noise), and
commit the resulting clean `README.md`.

Out of scope: `data-raw/autotest.png` / `data-raw/autotest.svg` (untracked hex-sticker
build outputs from `data-raw/hex-eps.Rmd`, an unrelated manual script) are not touched by
this stage.

## Open Questions
- Whether `with_null_device` needs to be applied anywhere beyond the two identified sites —
  a full audit of any other place arbitrary package/example code is executed should happen
  during implementation, not just these two.
- (B)'s fix relies on `getOption("knitr.in.progress")` being TRUE during `make knitr` for
  this project's actual render invocation (`Rscript`-driven `rmarkdown::render`) — needs
  empirical confirmation during implementation rather than assumed from documentation alone.
