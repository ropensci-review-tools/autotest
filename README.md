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

**This package is very unstable and subject to ongoing development (Feb,
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

<img src="README-stats-var-no-test-1.png" width="672" /><img src="README-stats-var-no-test-2.png" width="672" /><img src="README-stats-var-no-test-3.png" width="672" /><img src="README-stats-var-no-test-4.png" width="672" /><img src="README-stats-var-no-test-5.png" width="672" /><img src="README-stats-var-no-test-6.png" width="672" /><img src="README-stats-var-no-test-7.png" width="672" /><img src="README-stats-var-no-test-8.png" width="672" /><img src="README-stats-var-no-test-9.png" width="672" /><img src="README-stats-var-no-test-10.png" width="672" /><img src="README-stats-var-no-test-11.png" width="672" /><img src="README-stats-var-no-test-12.png" width="672" /><img src="README-stats-var-no-test-13.png" width="672" /><img src="README-stats-var-no-test-14.png" width="672" /><img src="README-stats-var-no-test-15.png" width="672" /><img src="README-stats-var-no-test-16.png" width="672" /><img src="README-stats-var-no-test-17.png" width="672" /><img src="README-stats-var-no-test-18.png" width="672" /><img src="README-stats-var-no-test-19.png" width="672" /><img src="README-stats-var-no-test-20.png" width="672" /><img src="README-stats-var-no-test-21.png" width="672" /><img src="README-stats-var-no-test-22.png" width="672" /><img src="README-stats-var-no-test-23.png" width="672" /><img src="README-stats-var-no-test-24.png" width="672" /><img src="README-stats-var-no-test-25.png" width="672" /><img src="README-stats-var-no-test-26.png" width="672" /><img src="README-stats-var-no-test-27.png" width="672" /><img src="README-stats-var-no-test-28.png" width="672" /><img src="README-stats-var-no-test-29.png" width="672" /><img src="README-stats-var-no-test-30.png" width="672" /><img src="README-stats-var-no-test-31.png" width="672" /><img src="README-stats-var-no-test-32.png" width="672" /><img src="README-stats-var-no-test-33.png" width="672" /><img src="README-stats-var-no-test-34.png" width="672" /><img src="README-stats-var-no-test-35.png" width="672" /><img src="README-stats-var-no-test-36.png" width="672" /><img src="README-stats-var-no-test-37.png" width="672" /><img src="README-stats-var-no-test-38.png" width="672" /><img src="README-stats-var-no-test-39.png" width="672" /><img src="README-stats-var-no-test-40.png" width="672" /><img src="README-stats-var-no-test-41.png" width="672" /><img src="README-stats-var-no-test-42.png" width="672" /><img src="README-stats-var-no-test-43.png" width="672" /><img src="README-stats-var-no-test-44.png" width="672" /><img src="README-stats-var-no-test-45.png" width="672" /><img src="README-stats-var-no-test-46.png" width="672" /><img src="README-stats-var-no-test-47.png" width="672" /><img src="README-stats-var-no-test-48.png" width="672" /><img src="README-stats-var-no-test-49.png" width="672" /><img src="README-stats-var-no-test-50.png" width="672" /><img src="README-stats-var-no-test-51.png" width="672" /><img src="README-stats-var-no-test-52.png" width="672" /><img src="README-stats-var-no-test-53.png" width="672" /><img src="README-stats-var-no-test-54.png" width="672" /><img src="README-stats-var-no-test-55.png" width="672" /><img src="README-stats-var-no-test-56.png" width="672" /><img src="README-stats-var-no-test-57.png" width="672" /><img src="README-stats-var-no-test-58.png" width="672" /><img src="README-stats-var-no-test-59.png" width="672" /><img src="README-stats-var-no-test-60.png" width="672" /><img src="README-stats-var-no-test-61.png" width="672" /><img src="README-stats-var-no-test-62.png" width="672" /><img src="README-stats-var-no-test-63.png" width="672" /><img src="README-stats-var-no-test-64.png" width="672" /><img src="README-stats-var-no-test-65.png" width="672" /><img src="README-stats-var-no-test-66.png" width="672" /><img src="README-stats-var-no-test-67.png" width="672" /><img src="README-stats-var-no-test-68.png" width="672" /><img src="README-stats-var-no-test-69.png" width="672" /><img src="README-stats-var-no-test-70.png" width="672" /><img src="README-stats-var-no-test-71.png" width="672" /><img src="README-stats-var-no-test-72.png" width="672" /><img src="README-stats-var-no-test-73.png" width="672" /><img src="README-stats-var-no-test-74.png" width="672" /><img src="README-stats-var-no-test-75.png" width="672" /><img src="README-stats-var-no-test-76.png" width="672" /><img src="README-stats-var-no-test-77.png" width="672" /><img src="README-stats-var-no-test-78.png" width="672" /><img src="README-stats-var-no-test-79.png" width="672" /><img src="README-stats-var-no-test-80.png" width="672" /><img src="README-stats-var-no-test-81.png" width="672" /><img src="README-stats-var-no-test-82.png" width="672" /><img src="README-stats-var-no-test-83.png" width="672" /><img src="README-stats-var-no-test-84.png" width="672" /><img src="README-stats-var-no-test-85.png" width="672" /><img src="README-stats-var-no-test-86.png" width="672" /><img src="README-stats-var-no-test-87.png" width="672" /><img src="README-stats-var-no-test-88.png" width="672" /><img src="README-stats-var-no-test-89.png" width="672" /><img src="README-stats-var-no-test-90.png" width="672" /><img src="README-stats-var-no-test-91.png" width="672" /><img src="README-stats-var-no-test-92.png" width="672" /><img src="README-stats-var-no-test-93.png" width="672" /><img src="README-stats-var-no-test-94.png" width="672" /><img src="README-stats-var-no-test-95.png" width="672" /><img src="README-stats-var-no-test-96.png" width="672" /><img src="README-stats-var-no-test-97.png" width="672" /><img src="README-stats-var-no-test-98.png" width="672" /><img src="README-stats-var-no-test-99.png" width="672" /><img src="README-stats-var-no-test-100.png" width="672" /><img src="README-stats-var-no-test-101.png" width="672" /><img src="README-stats-var-no-test-102.png" width="672" /><img src="README-stats-var-no-test-103.png" width="672" /><img src="README-stats-var-no-test-104.png" width="672" /><img src="README-stats-var-no-test-105.png" width="672" /><img src="README-stats-var-no-test-106.png" width="672" /><img src="README-stats-var-no-test-107.png" width="672" /><img src="README-stats-var-no-test-108.png" width="672" /><img src="README-stats-var-no-test-109.png" width="672" /><img src="README-stats-var-no-test-110.png" width="672" /><img src="README-stats-var-no-test-111.png" width="672" /><img src="README-stats-var-no-test-112.png" width="672" /><img src="README-stats-var-no-test-113.png" width="672" /><img src="README-stats-var-no-test-114.png" width="672" /><img src="README-stats-var-no-test-115.png" width="672" /><img src="README-stats-var-no-test-116.png" width="672" /><img src="README-stats-var-no-test-117.png" width="672" /><img src="README-stats-var-no-test-118.png" width="672" /><img src="README-stats-var-no-test-119.png" width="672" /><img src="README-stats-var-no-test-120.png" width="672" /><img src="README-stats-var-no-test-121.png" width="672" /><img src="README-stats-var-no-test-122.png" width="672" /><img src="README-stats-var-no-test-123.png" width="672" /><img src="README-stats-var-no-test-124.png" width="672" /><img src="README-stats-var-no-test-125.png" width="672" /><img src="README-stats-var-no-test-126.png" width="672" /><img src="README-stats-var-no-test-127.png" width="672" /><img src="README-stats-var-no-test-128.png" width="672" /><img src="README-stats-var-no-test-129.png" width="672" /><img src="README-stats-var-no-test-130.png" width="672" /><img src="README-stats-var-no-test-131.png" width="672" /><img src="README-stats-var-no-test-132.png" width="672" /><img src="README-stats-var-no-test-133.png" width="672" /><img src="README-stats-var-no-test-134.png" width="672" /><img src="README-stats-var-no-test-135.png" width="672" /><img src="README-stats-var-no-test-136.png" width="672" /><img src="README-stats-var-no-test-137.png" width="672" /><img src="README-stats-var-no-test-138.png" width="672" /><img src="README-stats-var-no-test-139.png" width="672" /><img src="README-stats-var-no-test-140.png" width="672" /><img src="README-stats-var-no-test-141.png" width="672" /><img src="README-stats-var-no-test-142.png" width="672" /><img src="README-stats-var-no-test-143.png" width="672" /><img src="README-stats-var-no-test-144.png" width="672" /><img src="README-stats-var-no-test-145.png" width="672" /><img src="README-stats-var-no-test-146.png" width="672" /><img src="README-stats-var-no-test-147.png" width="672" /><img src="README-stats-var-no-test-148.png" width="672" /><img src="README-stats-var-no-test-149.png" width="672" /><img src="README-stats-var-no-test-150.png" width="672" /><img src="README-stats-var-no-test-151.png" width="672" /><img src="README-stats-var-no-test-152.png" width="672" /><img src="README-stats-var-no-test-153.png" width="672" /><img src="README-stats-var-no-test-154.png" width="672" /><img src="README-stats-var-no-test-155.png" width="672" /><img src="README-stats-var-no-test-156.png" width="672" /><img src="README-stats-var-no-test-157.png" width="672" /><img src="README-stats-var-no-test-158.png" width="672" /><img src="README-stats-var-no-test-159.png" width="672" /><img src="README-stats-var-no-test-160.png" width="672" /><img src="README-stats-var-no-test-161.png" width="672" /><img src="README-stats-var-no-test-162.png" width="672" /><img src="README-stats-var-no-test-163.png" width="672" /><img src="README-stats-var-no-test-164.png" width="672" /><img src="README-stats-var-no-test-165.png" width="672" /><img src="README-stats-var-no-test-166.png" width="672" /><img src="README-stats-var-no-test-167.png" width="672" /><img src="README-stats-var-no-test-168.png" width="672" /><img src="README-stats-var-no-test-169.png" width="672" /><img src="README-stats-var-no-test-170.png" width="672" /><img src="README-stats-var-no-test-171.png" width="672" /><img src="README-stats-var-no-test-172.png" width="672" /><img src="README-stats-var-no-test-173.png" width="672" /><img src="README-stats-var-no-test-174.png" width="672" /><img src="README-stats-var-no-test-175.png" width="672" /><img src="README-stats-var-no-test-176.png" width="672" /><img src="README-stats-var-no-test-177.png" width="672" /><img src="README-stats-var-no-test-178.png" width="672" /><img src="README-stats-var-no-test-179.png" width="672" /><img src="README-stats-var-no-test-180.png" width="672" /><img src="README-stats-var-no-test-181.png" width="672" />

    #> 
    #> ── autotesting stats ──
    #> 
    #> ✔ [1 / 4]: var
    #> ✔ [2 / 4]: cor
    #> ✔ [3 / 4]: cov
    #> ✔ [4 / 4]: cov

<img src="README-stats-var-no-test-182.png" width="672" />

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
`test = FALSE` to indicate they were not run, and a `type` of “no\_test”
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
