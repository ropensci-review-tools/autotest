
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/autotest/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/autotest/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/autotest/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci-review-tools/autotest)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

# autotest

Automatic mutation testing of R packages. Mutation in the sense of
mutating inputs (parameters) to function calls, rather than mutation of
underlying code (see, for example,
[`mutant`](https://github.com/ropensci/mutant) for that). `autotest`
primarily works by scraping documented examples for all functions, and
mutating the parameters input to those functions.

**This package is very unstable and subject to ongoing development (Jun,
2021)**

## Installation

Not yet on CRAN, so must be installed from remote repository host
systems using any one of the following options:

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

The argument to `autotest_package()` can either be the name of an
installed package, or a path to a local directory containing the source
for a package. The result is a `data.frame` of errors, warnings, and
other diagnostic messages issued during package `auotest`-ing. See the
[main package
vignette](https://ropensci-review-tools.github.io/autotest/articles/autotest.html)
for an introductory tour of the package.

## What is tested?

The package includes a function which lists all tests currently
implemented.

``` r
autotest_types ()
#> # A tibble: 27 x 8
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
describing 27 unique tests. All `autotest` functions return these same
kinds of objects. The table returned from
[`autotest_types()`](https://ropensci-review-tools.github.io/autotest/reference/autotest_types.html)
can be used to selectively switch tests off by setting values in the
`test` column to `FALSE`, as demonstrated below.

Descriptions of each test can be readily extracted from the results of
that function:

``` r
a <- autotest_types ()
print (a [, c ("parameter_type", "operation", "content")], n = Inf)
#> # A tibble: 27 x 3
#>    parameter_type     operation                    content                      
#>    <chr>              <chr>                        <chr>                        
#>  1 rectangular        Convert one rectangular cla… "check for error/warning mes…
#>  2 rectangular        Convert one rectangular cla… "expect dimensions are same "
#>  3 rectangular        Convert one rectangular cla… "expect column names are ret…
#>  4 rectangular        Convert one rectangular cla… "expect all columns retain i…
#>  5 rectangular        Extend existent class with … "(Should yield same result)" 
#>  6 rectangular        Replace class with new class "(Should yield same result)" 
#>  7 vector             Convert vector input to lis… "(Should yield same result)" 
#>  8 vector             Custom class definitions fo… "(Should yield same result)" 
#>  9 numeric            Check whether double is onl… "int parameters should have …
#> 10 numeric            Add trivial noise to numeri… "(Should yield same result)" 
#> 11 single integer     Integer value converted to … "(Should yield same result)" 
#> 12 single logical     Substitute integer values f… "(Function call should still…
#> 13 single character   random character string as … "Should error"               
#> 14 single character   Change case                  "(Should yield same result)" 
#> 15 single integer     Ascertain permissible range  "Should either be unrestrict…
#> 16 single integer     Length 2 vector for length … "Should trigger message, war…
#> 17 single name or fo… Unquoted name/formula as qu… "Capture any warnings or err…
#> 18 single logical     Substitute character values… "should trigger warning or e…
#> 19 single logical     Negate default value of log… "(Function call should still…
#> 20 (return object)    Check that function success…  <NA>                        
#> 21 (return object)    Check that description has …  <NA>                        
#> 22 (return object)    Check whether description o…  <NA>                        
#> 23 (return object)    Compare class of return val…  <NA>                        
#> 24 <NA>               Check that parameter usage … "Examples do not demonstrate…
#> 25 <NA>               Identify functions without …  <NA>                        
#> 26 <NA>               Check that parameter is doc… "Examples do not document th…
#> 27 <NA>               Check that documentation ma…  <NA>
```

## How Does It Work?

The `autotest_package()` function returns by default a list of all tests
which would be conducted on a package, without actually implementing
those tests. The function has a parameter, `test`, with a default value
of `FALSE`. Setting `test = TRUE` then implements all tests, and only
returns results from tests which diverge from expected behaviour,
whether unexpected errors, warnings, or other behaviour. An ideal result
is that `autotest_package(., test = TRUE)` returns nothing (strictly,
`NULL`), indicating that all tests passed successfully.

Tests can also be selectively applied to particular functions through
the parameters `functions`, used to nominate functions to include in
tests, or `exclude`, used to nominate functions to exclude from tests.
The following code illustrates.

``` r
x <- autotest_package (package = "stats", functions = "var", test = FALSE)
#> 
#> ── autotesting stats ──
#> 
#> ✔ [1 / 4]: var
#> ✔ [2 / 4]: cor
#> ✔ [3 / 4]: cov
#> ✔ [4 / 4]: cov
print (x)
#> # A tibble: 150 x 9
#>    type  test_name  fn_name parameter  parameter_type operation  content   test 
#>    <chr> <chr>      <chr>   <chr>      <chr>          <chr>      <chr>     <lgl>
#>  1 dummy int_as_nu… var     x          integer vector Integer v… (Should … TRUE 
#>  2 dummy vector_cu… var     x          vector         Custom cl… (Should … TRUE 
#>  3 dummy vector_to… var     x          vector         Convert v… (Should … TRUE 
#>  4 dummy negate_lo… var     na.rm      single logical Negate de… (Functio… TRUE 
#>  5 dummy subst_int… var     na.rm      single logical Substitut… (Functio… TRUE 
#>  6 dummy subst_cha… var     na.rm      single logical Substitut… should t… TRUE 
#>  7 dummy single_pa… var     na.rm      single logical Length 2 … Should t… TRUE 
#>  8 dummy return_su… var     (return o… (return objec… Check tha… <NA>      TRUE 
#>  9 dummy return_va… var     (return o… (return objec… Check tha… <NA>      TRUE 
#> 10 dummy return_de… var     (return o… (return objec… Check whe… <NA>      TRUE 
#> # … with 140 more rows, and 1 more variable: yaml_hash <chr>
```

Testing the `var` function also tests `cor` and `cov`, because the
package works by scraping the documented examples from the associated
`.Rd` help file, and `?var` shows that the help topic is `cor`, and
includes the three functions, `var`, `cor`, and `cov`. That result
details the 150 tests which would be applied to the `var` function from
the `stats` package. These 150 tests yield the following results when
actually applied:

``` r
y <- autotest_package (package = "stats", functions = "var", test = TRUE)
#> ── autotesting stats ──
#> 
#> ✔ [1 / 4]: var
#> ✔ [2 / 4]: cor
#> ✔ [3 / 4]: cov
#> ✔ [4 / 4]: cov
print (y)
#> # A tibble: 15 x 9
#>    type   test_name  fn_name parameter parameter_type operation  content   test 
#>    <chr>  <chr>      <chr>   <chr>     <chr>          <chr>      <chr>     <lgl>
#>  1 diagn… vector_to… var     x         vector         Convert v… "Functio… TRUE 
#>  2 diagn… vector_to… var     x         vector         Convert v… "Functio… TRUE 
#>  3 diagn… vector_to… var     y         vector         Convert v… "Functio… TRUE 
#>  4 diagn… single_ch… cor     use       single charac… upper-cas… "is case… TRUE 
#>  5 diagn… single_ch… cor     method    single charac… upper-cas… "is case… TRUE 
#>  6 diagn… single_ch… cor     use       single charac… upper-cas… "is case… TRUE 
#>  7 diagn… single_ch… cor     method    single charac… upper-cas… "is case… TRUE 
#>  8 diagn… single_ch… cov     use       single charac… upper-cas… "is case… TRUE 
#>  9 diagn… single_ch… cov     method    single charac… upper-cas… "is case… TRUE 
#> 10 diagn… single_ch… cov     use       single charac… upper-cas… "is case… TRUE 
#> 11 diagn… single_ch… cov     method    single charac… upper-cas… "is case… TRUE 
#> 12 diagn… single_ch… cov     use       single charac… upper-cas… "is case… TRUE 
#> 13 diagn… single_ch… cov     method    single charac… upper-cas… "is case… TRUE 
#> 14 warni… par_is_de… var     use       <NA>           Check tha… "Example… TRUE 
#> 15 warni… par_is_de… cov     y         <NA>           Check tha… "Example… TRUE 
#> # … with 1 more variable: yaml_hash <chr>
```

And only 15 of the original 150 tests produced unexpected behaviour.
There were in fact only three kinds of tests which produced these 15
results:

``` r
unique (y$operation)
#> [1] "Convert vector input to list-columns"      
#> [2] "upper-case character parameter"            
#> [3] "Check that parameter usage is demonstrated"
```

The first involves conversion of a vector to a list-column
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
#> ✔ [1 / 4]: var
#> ✔ [2 / 4]: cor
#> ✔ [3 / 4]: cov
#> ✔ [4 / 4]: cov
print (y)
#> # A tibble: 16 x 9
#>    type   test_name  fn_name parameter parameter_type operation  content   test 
#>    <chr>  <chr>      <chr>   <chr>     <chr>          <chr>      <chr>     <lgl>
#>  1 no_te… vector_to… var     x         vector         Convert v… (Should … FALSE
#>  2 no_te… vector_to… var     y         vector         Convert v… (Should … FALSE
#>  3 no_te… vector_to… cor     x         vector         Convert v… (Should … FALSE
#>  4 no_te… vector_to… cor     y         vector         Convert v… (Should … FALSE
#>  5 diagn… single_ch… cor     use       single charac… upper-cas… is case … TRUE 
#>  6 diagn… single_ch… cor     method    single charac… upper-cas… is case … TRUE 
#>  7 diagn… single_ch… cor     use       single charac… upper-cas… is case … TRUE 
#>  8 diagn… single_ch… cor     method    single charac… upper-cas… is case … TRUE 
#>  9 diagn… single_ch… cov     use       single charac… upper-cas… is case … TRUE 
#> 10 diagn… single_ch… cov     method    single charac… upper-cas… is case … TRUE 
#> 11 diagn… single_ch… cov     use       single charac… upper-cas… is case … TRUE 
#> 12 diagn… single_ch… cov     method    single charac… upper-cas… is case … TRUE 
#> 13 diagn… single_ch… cov     use       single charac… upper-cas… is case … TRUE 
#> 14 diagn… single_ch… cov     method    single charac… upper-cas… is case … TRUE 
#> 15 warni… par_is_de… var     use       <NA>           Check tha… Examples… TRUE 
#> 16 warni… par_is_de… cov     y         <NA>           Check tha… Examples… TRUE 
#> # … with 1 more variable: yaml_hash <chr>
```

Those tests are still returned from `autotest_package()`, but with
`test = FALSE` to indicate they were not run, and a `type` of “no\_test”
rather than the previous “diagnostic”.

## Can `autotest` automatically create tests in my `tests` directory?

Not yet, but that should be possible soon. In the meantime, there are
[`testthat`](https://testthat.r-lib.org) expectations, listed in the
[main package
functions](https://ropensci-review-tools.github.io/autotest/reference/index.html),
which enable `autotest` to be used in a package’s test suite.

## Prior work

1.  The
    [`great-expectations`](https://github.com/great-expectations/great_expectations)
    framework for python, described in [this medium
    article](https://medium.com/@expectgreatdata/down-with-pipeline-debt-introducing-great-expectations-862ddc46782a).
2.  [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) for
    Haskell
3.  [`mutate`](https://github.com/mbj/mutant) for ruby.
