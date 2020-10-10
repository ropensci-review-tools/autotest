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

**This package is very unstable and subject to ongoing development (Oct 2020)**

Tests are controlled via a simple YAML schema specifying one or more
example workflows for each function of a package, with workflows able to
be automatically generated from documented examples, and `autotest`
tests able to be used to generate package tests.

The simplest workflows involve nominating data sets which may be
submitted to a function, while more complicated workflows may involve
multiple data sets, intermediate pre-processing stages, and other
transformations prior to submission to a nominated function. The
following illustrates a typical `yaml` schema, where items in
angle-brackets (`<`, `>`) indicate placeholders be completed for a
specific package and functions.

    package: <package_name>
    functions:
        - <name of function>:
            - preprocess:
                - "<R code required for pre-processing exlosed in quotation marks>"
                - "<second line of pre-processing code>"
                - "<more code>"
            - class:
                - <param_name>: [<class1>, <class2>]
            - parameters:
                - <param_name>: <value>
                - <another_param>: <value>
        - <name of same or different function>::
            - parameters:
                - <param_name>: <value>

The `class` line declares an input parameter, `<param_name>`, to be of
one or more defined classes. If not given, and if nominated input data
are, for example, of any standard rectangular format, then `autotest`
will test the response to the nominated function to input of *all*
standard rectangular formats. The `class` parameter can thus be used to
declare that the function is only expected to receive input data in one
or more pre-defined class formats, and so responses to potentially
different classes will not be tested. The above template can be
generated via the function `at_yaml_template()`.

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

What gets tested?
-----------------

The package is primarily intended to test packages and functions
intended to process *data*, with data suitable for input into functions
within a package specified in the `autotest.yaml` template. Other types
of packages, such as those which provide access to data, will not be
effectively tested by `autotest`. Standard tests include translation,
substitution, and manipulation of classes and attributes of most
standard one- and two-dimensional forms for representing data in R.

Example
-------

Current functionality applied to test two functions from the [`SmartEDA`
package](https://github.com/daya6489/SmartEDA) as specified in the
following `yaml`. This also illustrates the use of data transformation
steps, in this case implementing a couple of simple transformations on
columns of the external data set,
[`ISLR::Carseats`](https://cran.r-project.org/package=ISLR), including
introducing some missing data through replacing values with `NA`.

    yaml <- c ("package: SmartEDA",
    "functions:",
    "   - ExpData:",
    "       - preprocess:",
    "           - 'x <- ISLR::Carseats'",
    "           - 'x$Sales <- sqrt (x$Sales)'",
    "           - 'x$CompPrice <- as.integer (x$CompPrice)'",
    "           - 'x$Sales [ceiling (runif (10) * nrow (x))] <- NA'",
    "       - parameters:",
    "           - data: x",
    "   - ExpData:",
    "       - class:",
    "           - data: data.frame",
    "       - parameters:",
    "           - data: ISLR::College",
    "   - ExpStat:",
    "       - parameters:",
    "           - X: ISLR::Carseats$Sales",
    "           - Y: ISLR::Carseats$Urban",
    "           - valueOfGood: 'Yes'")

Note that the second function definition is for the same function as the
first (namely, `ExpData`), but specifies alternative input data, and
also includes the restriction that input data to the function should be
of class `data.frame` only. Submitting this `yaml` to the `autotest`
function gives the following results:

    autotest (yaml = yaml)

    #> ★ Loading the following libraries:
    #> ● ISLR
    #> ● SmartEDA
    #> ✔ ExpData
    #> ✔ ExpData
    #> ✔ ExpStat

|     | type       | fn\_name       | parameter   | operation                                   | content                                                                                                                         | yaml\_hash |
|:----|:-----------|:---------------|:------------|:--------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------|:-----------|
| 1   | diagnostic | ExpData        | data        | tabular structure with new class structure  | Function \[ExpData\] should error when class structure of `data.frame` input is removed.                                        | 1f3d15     |
| 2   | diagnostic | ExpData        | type        | ascertain integer range                     | Parameter \[type\] permits unrestricted integer inputs                                                                          | 1f3d15     |
| 3   | diagnostic | ExpData        | type        | length 2 vector for single-length parameter | parameter \[type\] is assumed to be a single value of integer type, yet admits vectors of length &gt; 1                         | 1f3d15     |
| 4   | diagnostic | ExpData        | NA          | check that description has return value     | Function \[ExpData\] does not specify a return value, yet returns a value of class \[data.frame\]                               | 1f3d15     |
| 5   | warning    | chisq.test(tb) | NA          | normal function call                        | Chi-squared approximation may be incorrect                                                                                      | 1f3d15     |
| 6   | diagnostic | ExpStat        | X           | convert vector input to list-columns        | Function \[ExpStat\] errors on list-columns when submitted as X Error message: unimplemented type ‘list’ in ‘orderVector1’      | 1f3d15     |
| 7   | diagnostic | ExpStat        | Y           | convert vector input to list-columns        | Function \[ExpStat\] errors on list-columns when submitted as Y Error message: unimplemented type ‘list’ in ‘orderVector1’      | 1f3d15     |
| 8   | diagnostic | ExpStat        | valueOfGood | lower case character parameter              | Parameter valueOfGood of function \[ExpStat\] is assumed to a single character, but is case dependent                           | 1f3d15     |
| 9   | diagnostic | ExpStat        | valueOfGood | upper case character parameter              | Parameter valueOfGood of function \[ExpStat\] is assumed to a single character, but is case dependent                           | 1f3d15     |
| 10  | diagnostic | ExpStat        | valueOfGood | random character string as parameter        | Parameter valueOfGood of function \[ExpStat\] is assumed to a single character, but does not match arguments to expected values | 1f3d15     |
| 11  | diagnostic | ExpStat        | valueOfGood | length 2 vector for single-length parameter | parameter \[valueOfGood\] is assumed to be a single value of character type, yet admits vectors of length &gt; 1                | 1f3d15     |

And neither the `ExpData` nor the `ExpStat` function pass `autotest`
conditions, with `ExpData` failing to specify the class of return value,
and `ExpData` failing to appropriately `unlist` or `unnest`
list-columns.

Prior work
----------

1.  The
    [`great-expectations`](https://github.com/great-expectations/great_expectations)
    framework for python, described in [this medium
    article](https://medium.com/@expectgreatdata/down-with-pipeline-debt-introducing-great-expectations-862ddc46782a).
2.  [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) for
    Haskell
