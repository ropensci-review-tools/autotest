<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/autotest/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/autotest/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropenscilabs/autotest/branch/master/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/autotest)
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

**This package is very unstable and subject to ongoing development (Jan
2021)**

## Installation

Not yet on CRAN, so must be installed from remote repository host
systems using any one of the following options:

``` r
# install.packages("remotes")
remotes::install_git("https://git.sr.ht/~mpadge/autotest")
remotes::install_bitbucket("mpadge/autotest")
remotes::install_gitlab("mpadge/autotest")
remotes::install_github("ropenscilabs/autotest")
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
other diagnostic messages issued during package `auotest`-ing.

## What is tested?

The package includes a function which lists all tests currently
implemented.

``` r
autotest_types ()
#> # A tibble: 25 x 8
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
#>  9 dummy trivial_n… <NA>    <NA>      numeric        Add trivia… "(Should… TRUE 
#> 10 dummy int_as_nu… <NA>    <NA>      single integer Integer va… "(Should… TRUE 
#> # … with 15 more rows
```

That functions returns a [`tibble`](https://tibble.tidyverse.org)
describing 25 unique tests. All `autotest` functions return these same
kinds of objects. The table returned from
[`autotest_types()`](https://ropenscilabs.github.io/autotest/reference/autotest_types.html)
can be used to selectively switch tests off by setting values in the
`test` column to `FALSE`, as demonstrated below.

Descriptions of each test can be readily extracted from the results of
that function:

``` r
a <- autotest_types ()
print (a [, c ("parameter_type", "operation", "content")], n = Inf)
#> # A tibble: 25 x 3
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
#>  9 numeric            Add trivial noise to numeri… "(Should yield same result)" 
#> 10 single integer     Integer value converted to … "(Should yield same result)" 
#> 11 single logical     Substitute integer values f… "(Function call should still…
#> 12 single character   random character string as … "Should error"               
#> 13 single character   Change case                  "(Should yield same result)" 
#> 14 single integer     Ascertain permissible range  "Should either be unrestrict…
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
```

## How Does It Work?

The `autotest_package()` function returns the results of implementing
all tests on a given package, or on specified functions on that package.
These results only indicate any divergence from expected behaviour,
whether unexpected errors, warnings, or other behaviour. An ideal result
is that `autotest_package()` returns nothing (strictly, `NULL`),
indicating that all tests passed successfully. To see which tests were
performed, or to see which tests would be performed prior to actually
running them, the function has a `test` parameter with a default value
of `TRUE`. Setting this to `FALSE` returns a (generally much larger)
`data.frame` of all tests which would be conducted on the nominated
package.

Tests can also be selectively specified through the parameters
`functions`, used to nominate functions to include in tests, or
`exclude`, used to nominate functions to exclude from tests. The
following code illustrates.

``` r
x <- autotest_package (package = "stats", functions = "var", test = FALSE)
```

    #> 
    #> ── autotesting stats ──
    #> 
    #> ✔ [1 / 4]: var
    #> ✔ [2 / 4]: cor
    #> ✔ [3 / 4]: cov
    #> ✔ [4 / 4]: cov


``` r
print (x)
#> # A tibble: 122 x 9
#>    type  test_name fn_name parameter parameter_type operation content test 
#>    <chr> <chr>     <chr>   <chr>     <chr>          <chr>     <chr>   <lgl>
#>  1 dummy int_as_n… var     x         integer vector Integer … (Shoul… TRUE 
#>  2 dummy vector_c… var     x         vector         Custom c… (Shoul… TRUE 
#>  3 dummy vector_t… var     x         vector         Convert … (Shoul… TRUE 
#>  4 dummy negate_l… var     na.rm     single logical Negate d… (Funct… TRUE 
#>  5 dummy subst_in… var     na.rm     single logical Substitu… (Funct… TRUE 
#>  6 dummy subst_ch… var     na.rm     single logical Substitu… should… TRUE 
#>  7 dummy single_p… var     na.rm     single logical Length 2… Should… TRUE 
#>  8 dummy return_s… var     (return … (return objec… Check th… <NA>    TRUE 
#>  9 dummy return_v… var     (return … (return objec… Check th… <NA>    TRUE 
#> 10 dummy return_d… var     (return … (return objec… Check wh… <NA>    TRUE 
#> # … with 112 more rows, and 1 more variable: yaml_hash <chr>
```

Testing the `var` function also tests `cor` and `cov`, because the
package works by scraping the documented examples from the associated
`.Rd` help file, and `?var` shows that the help topic is `cor`, and
includes the three functions, `var`, `cor`, and `cov`. That result
details the 122 tests which would be applied to the `var` function from
the `stats` package. These 122 tests yield the following results when
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
#> # A tibble: 16 x 9
#>    type  test_name fn_name parameter parameter_type operation content test 
#>    <chr> <chr>     <chr>   <chr>     <chr>          <chr>     <chr>   <lgl>
#>  1 diag… vector_t… var     x         vector         Convert … "Funct… TRUE 
#>  2 diag… vector_t… var     y         vector         Convert … "Funct… TRUE 
#>  3 diag… vector_t… cor     x         vector         Convert … "Funct… TRUE 
#>  4 diag… vector_t… cor     y         vector         Convert … "Funct… TRUE 
#>  5 diag… single_c… cor     use       single charac… upper-ca… "is ca… TRUE 
#>  6 diag… single_c… cor     method    single charac… upper-ca… "is ca… TRUE 
#>  7 diag… single_c… cor     use       single charac… upper-ca… "is ca… TRUE 
#>  8 diag… single_c… cor     method    single charac… upper-ca… "is ca… TRUE 
#>  9 diag… single_c… cov     use       single charac… upper-ca… "is ca… TRUE 
#> 10 diag… single_c… cov     method    single charac… upper-ca… "is ca… TRUE 
#> 11 diag… single_c… cov     use       single charac… upper-ca… "is ca… TRUE 
#> 12 diag… single_c… cov     method    single charac… upper-ca… "is ca… TRUE 
#> 13 diag… single_c… cov     use       single charac… upper-ca… "is ca… TRUE 
#> 14 diag… single_c… cov     method    single charac… upper-ca… "is ca… TRUE 
#> 15 warn… par_is_d… var     use       <NA>           Check th… "Examp… TRUE 
#> 16 warn… par_is_d… cov     y         <NA>           Check th… "Examp… TRUE 
#> # … with 1 more variable: yaml_hash <chr>
```

And only 16 of the original 122 tests produced unexpected behaviour.
There were in fact only three kinds of tests which produced these 16
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
`autotest`-ing a package. The `autotest_types()` function accepts a
single argument listing the `test_name` entries of any tests which are
to be switched off.

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
#>    type  test_name fn_name parameter parameter_type operation content test 
#>    <chr> <chr>     <chr>   <chr>     <chr>          <chr>     <chr>   <lgl>
#>  1 no_t… vector_t… var     x         vector         Convert … (Shoul… FALSE
#>  2 no_t… vector_t… var     y         vector         Convert … (Shoul… FALSE
#>  3 no_t… vector_t… cor     x         vector         Convert … (Shoul… FALSE
#>  4 no_t… vector_t… cor     y         vector         Convert … (Shoul… FALSE
#>  5 diag… single_c… cor     use       single charac… upper-ca… is cas… TRUE 
#>  6 diag… single_c… cor     method    single charac… upper-ca… is cas… TRUE 
#>  7 diag… single_c… cor     use       single charac… upper-ca… is cas… TRUE 
#>  8 diag… single_c… cor     method    single charac… upper-ca… is cas… TRUE 
#>  9 diag… single_c… cov     use       single charac… upper-ca… is cas… TRUE 
#> 10 diag… single_c… cov     method    single charac… upper-ca… is cas… TRUE 
#> 11 diag… single_c… cov     use       single charac… upper-ca… is cas… TRUE 
#> 12 diag… single_c… cov     method    single charac… upper-ca… is cas… TRUE 
#> 13 diag… single_c… cov     use       single charac… upper-ca… is cas… TRUE 
#> 14 diag… single_c… cov     method    single charac… upper-ca… is cas… TRUE 
#> 15 warn… par_is_d… var     use       <NA>           Check th… Exampl… TRUE 
#> 16 warn… par_is_d… cov     y         <NA>           Check th… Exampl… TRUE 
#> # … with 1 more variable: yaml_hash <chr>
```

Those tests are still returned from `autotest_package()`, but with
`test = FALSE` to indicate they were not run, and a `type` of “no_test”
rather than the previous “diagnostic”.

## Can `autotest` automatically create tests in my `tests` directory?

Not yet, but that should be possible soon. Stay tuned …

## Prior work

1.  The
    [`great-expectations`](https://github.com/great-expectations/great_expectations)
    framework for python, described in [this medium
    article](https://medium.com/@expectgreatdata/down-with-pipeline-debt-introducing-great-expectations-862ddc46782a).
2.  [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) for
    Haskell
3.  [`mutate`](https://github.com/mbj/mutant) for ruby.
