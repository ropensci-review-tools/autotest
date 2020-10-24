<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/autotest/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/autotest/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropenscilabs/autotest/branch/master/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/autotest)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

autotest
========

Automatic mutation testing of R packages. Mutation in the sense of
mutating inputs (parameters) to function calls, rather than mutation of
underlying code (see, for example,
[`mutant`](https://github.com/ropensci/mutant) for that). `autotest`
primarily works by scraping documented examples for all functions, and
mutating the parameters input to those functions.

**This package is very unstable and subject to ongoing development (Oct
2020)**

Installation
------------

Not yet on CRAN, so must be installed from remote repository host
systems using any one of the following options:

    # install.packages("remotes")
    remotes::install_git("https://git.sr.ht/~mpadge/autotest")
    remotes::install_bitbucket("mpadge/autotest")
    remotes::install_gitlab("mpadge/autotest")
    remotes::install_github("ropenscilabs/autotest")

The package can then be loaded the usual way:

    library (autotest)

Usage
-----

The simply way to use the package is

    x <- autotest_package ("<package>")

The argument to `autotest_package()` can either be the name of an
installed package, or a path to a local directory containing the source
for a package. The result is a `data.frame` of errors, warnings, and
other diagnostic messages issued during package `auotest`-ing.

What gets tested?
-----------------

The package is primarily intended to test packages and functions
intended to process *data*, with data suitable for input into functions
within a package specified in the `autotest.yaml` template. Other types
of packages, such as those which provide access to data, will not be
effectively tested by `autotest`. Standard tests include translation,
substitution, and manipulation of classes and attributes of most
standard one- and two-dimensional forms for representing data in R.

Prior work
----------

1.  The
    [`great-expectations`](https://github.com/great-expectations/great_expectations)
    framework for python, described in [this medium
    article](https://medium.com/@expectgreatdata/down-with-pipeline-debt-introducing-great-expectations-862ddc46782a).
2.  [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) for
    Haskell
3.  [`mutate`](https://github.com/mbj/mutant) for ruby.
