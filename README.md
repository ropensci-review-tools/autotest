# autotest <a href='https://docs.ropensci.org/autotest'><img src='man/figures/autotest.png' align="right" height=210 width=182></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/autotest/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/autotest/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/autotest/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci-review-tools/autotest)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Automatic mutation testing of R packages. Mutation in the sense of
mutating inputs (parameters) to function calls. `autotest` primarily
works by scraping documented examples for all functions, and mutating
the parameters input to those functions.

## Installation

The easiest way to install this package is via the associated
[`r-universe`](https://ropensci-review-tools.r-universe.dev/ui#builds).
As shown there, simply enable the universe with

``` r
options(repos = c(
    ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev",
    CRAN = "https://cloud.r-project.org"))
```

And then install the usual way with,

``` r
install.packages("autotest")
```

Alternatively, the package can be installed by running one of the
following lines:

``` r
# install.packages("remotes")
remotes::install_git("https://git.sr.ht/~mpadge/autotest")
remotes::install_bitbucket("mpadge/autotest")
remotes::install_gitlab("mpadge/autotest")
remotes::install_github("ropensci-review-tools/autotest")
```

The package can then be loaded the usual way:

``` r
library (autotest)
```

## Usage

The simply way to use the package is

``` r
x <- autotest_package ("<package>")
```

The main argument to the [`autotest_package()`
function](https://docs.ropensci.org/autotest/reference/autotest_package.html)
can either be the name of an installed package, or a path to a local
directory containing the source for a package. The result is a
`data.frame` of errors, warnings, and other diagnostic messages issued
during package `auotest`-ing. The function has an additional parameter,
`functions`, to restrict tests to specified functions only.

By default,
[`autotest_package()`](https://docs.ropensci.org/autotest/reference/autotest_package.html)
returns a list of all tests applied to a package without actually
running them. To implement those tests, set the parameter `test` to
`TRUE`. Results are only returned for tests in which functions do not
behave as expected, whether through triggering errors, warnings, or
other behaviour as described below. The ideal behaviour of
`autotest_package()` is to return nothing (or strictly, `NULL`),
indicating that all tests passed successfully. See the [main package
vignette](https://docs.ropensci.org/autotest/articles/autotest.html) for
an introductory tour of the package.

## What is tested?

The package includes a function which lists all tests currently
implemented.

``` r
autotest_types ()
#> # A tibble: 27 × 8
#>    type  test_name  fn_name parameter parameter_type operation   content   test 
#>    <chr> <chr>      <chr>   <chr>     <chr>          <chr>       <chr>     <lgl>
#>  1 dummy rect_as_o… <NA>    <NA>      rectangular    Convert on… "check f… TRUE 
#>  2 dummy rect_comp… <NA>    <NA>      rectangular    Convert on… "expect … TRUE 
#>  3 dummy rect_comp… <NA>    <NA>      rectangular    Convert on… "expect … TRUE 
#>  4 dummy rect_comp… <NA>    <NA>      rectangular    Convert on… "expect … TRUE 
#>  5 dummy extend_re… <NA>    <NA>      rectangular    Extend exi… "(Should… TRUE 
#>  6 dummy replace_r… <NA>    <NA>      rectangular    Replace cl… "(Should… TRUE 
#>  7 dummy vector_to… <NA>    <NA>      vector         Convert ve… "(Should… TRUE 
#>  8 dummy vector_cu… <NA>    <NA>      vector         Custom cla… "(Should… TRUE 
#>  9 dummy double_is… <NA>    <NA>      numeric        Check whet… "int par… TRUE 
#> 10 dummy trivial_n… <NA>    <NA>      numeric        Add trivia… "(Should… TRUE 
#> # … with 17 more rows
```

That functions returns a [`tibble`](https://tibble.tidyverse.org)
describing 27 unique tests. The default behaviour of
[`autotest_package()`](https://docs.ropensci.org/autotest/reference/autotest_package.html)
with `test = FALSE` uses these test types to identify which tests will
be applied to each parameter and function. The table returned from
[`autotest_types()`](https://docs.ropensci.org/autotest/reference/autotest_types.html)
can be used to selectively switch tests off by setting values in the
`test` column to `FALSE`, as demonstrated below.

## How Does It Work?

The package works by scraping documented examples from all `.Rd` help
files, and using those to identify the types of all parameters to all
functions. Usage therefore first requires that the usage of all
parameters be demonstrated in example code.

As described above, tests can also be selectively applied to particular
functions through the parameters `functions`, used to nominate functions
to include in tests, or `exclude`, used to nominate functions to exclude
from tests. The following code illustrates.

``` r
x <- autotest_package (package = "stats", functions = "var", test = FALSE)
#> 
#> ── autotesting stats ──
#> 
#> ✔ [1 / 6]: var
#> ✔ [2 / 6]: cor
#> ✔ [3 / 6]: cor
#> ✔ [4 / 6]: cov
#> ✔ [5 / 6]: cov
#> ✔ [6 / 6]: cor
print (x)
#> # A tibble: 185 × 9
#>    type    test_name  fn_name parameter parameter_type operation  content  test 
#>    <chr>   <chr>      <chr>   <chr>     <chr>          <chr>      <chr>    <lgl>
#>  1 warning par_is_de… var     use       <NA>           Check tha… Example… TRUE 
#>  2 warning par_is_de… cov     y         <NA>           Check tha… Example… TRUE 
#>  3 dummy   trivial_n… var     x         numeric        Add trivi… (Should… TRUE 
#>  4 dummy   vector_cu… var     x         vector         Custom cl… (Should… TRUE 
#>  5 dummy   vector_to… var     x         vector         Convert v… (Should… TRUE 
#>  6 dummy   negate_lo… var     na.rm     single logical Negate de… (Functi… TRUE 
#>  7 dummy   subst_int… var     na.rm     single logical Substitut… (Functi… TRUE 
#>  8 dummy   subst_cha… var     na.rm     single logical Substitut… should … TRUE 
#>  9 dummy   single_pa… var     na.rm     single logical Length 2 … Should … TRUE 
#> 10 dummy   return_su… var     (return … (return objec… Check tha… <NA>     TRUE 
#> # … with 175 more rows, and 1 more variable: yaml_hash <chr>
```

Testing the `var` function also tests `cor` and `cov`, because these are
all documented within a single `.Rd` help file. Typing `?var` shows that
the help topic is `cor`, and that the examples include the three
functions, `var`, `cor`, and `cov`. That result details the 185 tests
which would be applied to the `var` function from the `stats` package.
These 185 tests yield the following results when actually applied:

``` r
y <- autotest_package (package = "stats", functions = "var", test = TRUE)
#> ── autotesting stats ──
#> 
#> ✔ [1 / 6]: var
#> ✔ [2 / 6]: cor
#> ✔ [3 / 6]: cor
#> ✔ [4 / 6]: cov
#> ✔ [5 / 6]: cov
#> ✔ [6 / 6]: cor
print (y)
#> # A tibble: 23 × 9
#>    type       test_name fn_name parameter parameter_type operation content test 
#>    <chr>      <chr>     <chr>   <chr>     <chr>          <chr>     <chr>   <lgl>
#>  1 warning    par_is_d… var     use       <NA>           Check th… "Examp… TRUE 
#>  2 warning    par_is_d… cov     y         <NA>           Check th… "Examp… TRUE 
#>  3 diagnostic vector_t… var     x         vector         Convert … "Funct… TRUE 
#>  4 diagnostic vector_t… var     x         vector         Convert … "Funct… TRUE 
#>  5 diagnostic vector_t… var     y         vector         Convert … "Funct… TRUE 
#>  6 diagnostic single_c… cor     use       single charac… upper-ca… "is ca… TRUE 
#>  7 diagnostic single_c… cor     method    single charac… upper-ca… "is ca… TRUE 
#>  8 diagnostic vector_c… cor     x         vector         Custom c… "Funct… TRUE 
#>  9 diagnostic single_c… cor     method    single charac… upper-ca… "is ca… TRUE 
#> 10 diagnostic single_c… cor     use       single charac… upper-ca… "is ca… TRUE 
#> # … with 13 more rows, and 1 more variable: yaml_hash <chr>
```

And only 23 of the original 185 tests produced unexpected behaviour.
There were in fact only 4 kinds of tests which produced these 23
results:

``` r
unique (y$operation)
#> [1] "Check that parameter usage is demonstrated"
#> [2] "Convert vector input to list-columns"      
#> [3] "upper-case character parameter"            
#> [4] "Custom class definitions for vector input"
```

One of these involves conversion of a vector to a list-column
representation (via `I(as.list(<vec>))`). Relatively few packages accept
this kind of input, even though doing so is relatively straightforward.
The following lines demonstrate how these tests can be switched off when
`autotest`-ing a package. The `autotest_types()` function, used above to
extract information on all types of tests, also accepts a single
argument listing the `test_name` entries of any tests which are to be
switched off.

``` r
types <- autotest_types (notest = "vector_to_list_col")
y <- autotest_package (package = "stats", functions = "var",
                       test = TRUE, test_data = types)
#> ── autotesting stats ──
#> 
#> ✔ [1 / 6]: var
#> ✔ [2 / 6]: cor
#> ✔ [3 / 6]: cor
#> ✔ [4 / 6]: cov
#> ✔ [5 / 6]: cov
#> ✔ [6 / 6]: cor
print (y)
#> # A tibble: 28 × 9
#>    type       test_name fn_name parameter parameter_type operation content test 
#>    <chr>      <chr>     <chr>   <chr>     <chr>          <chr>     <chr>   <lgl>
#>  1 warning    par_is_d… var     use       <NA>           Check th… Exampl… TRUE 
#>  2 warning    par_is_d… cov     y         <NA>           Check th… Exampl… TRUE 
#>  3 diagnostic single_c… cor     use       single charac… upper-ca… is cas… TRUE 
#>  4 diagnostic single_c… cor     method    single charac… upper-ca… is cas… TRUE 
#>  5 diagnostic vector_c… cor     x         vector         Custom c… Functi… TRUE 
#>  6 diagnostic single_c… cor     method    single charac… upper-ca… is cas… TRUE 
#>  7 diagnostic single_c… cor     use       single charac… upper-ca… is cas… TRUE 
#>  8 diagnostic single_c… cor     use       single charac… upper-ca… is cas… TRUE 
#>  9 diagnostic single_c… cor     method    single charac… upper-ca… is cas… TRUE 
#> 10 diagnostic vector_c… cov     x         vector         Custom c… Functi… TRUE 
#> # … with 18 more rows, and 1 more variable: yaml_hash <chr>
```

Those tests are still returned from `autotest_package()`, but with
`test = FALSE` to indicate they were not run, and a `type` of “no_test”
rather than the previous “diagnostic”.

## Can `autotest` automatically create tests in my `tests` directory?

Not yet, but that should be possible soon. In the meantime, there are
[`testthat`](https://testthat.r-lib.org) expectations, listed in the
[main package
functions](https://docs.ropensci.org/autotest/reference/index.html),
which enable `autotest` to be used in a package’s test suite.

## Prior work

1.  The
    [`great-expectations`](https://github.com/great-expectations/great_expectations)
    framework for python, described in [this medium
    article](https://medium.com/@expectgreatdata/down-with-pipeline-debt-introducing-great-expectations-862ddc46782a).
2.  [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) for
    Haskell
3.  [`mutate`](https://github.com/mbj/mutant) for ruby
4.  [`mutant`](https://github.com/ropensci/mutant) for mutation of R
    code itself

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
