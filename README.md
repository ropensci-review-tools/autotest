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

**This package is very unstable and subject to ongoing development (Nov
2020)**

Installation
------------

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

Usage
-----

The simply way to use the package is

``` r
x <- autotest_package ("<package>")
```

The argument to `autotest_package()` can either be the name of an
installed package, or a path to a local directory containing the source
for a package. The result is a `data.frame` of errors, warnings, and
other diagnostic messages issued during package `auotest`-ing.

How Does It Work? (Part 1)
--------------------------

The `autotest_package()` function returns the results of implementing
all tests on a given package. These results only indicate any divergence
from expected behaviour, whether unexpected errors, warnings, or other
behaviour. An ideal result is that `autotest_package()` returns nothing
(strictly, `NULL`), indicating that all tests passed successfully. To
see which tests were performed, or to see which tests would be performed
prior to actually running them, the function has a `test` parameter with
a default value of `TRUE`. Setting this to `FALSE` returns a (generally
much larger) `data.frame` of all tests which would be conducted on the
nominated package.

Tests can also be selectively specified through the parameters
`functions`, used to nominate functions to include in tests, or
`exclude`, used to nominate functions to exclude from tests. The
following code illustrates.

``` r
autotest_package (package = "stats", functions = "var", test = FALSE)
```

    #> 
    #> ── autotesting stats ──
    #> 
    #> ✔ [1 / 4]: var
    #> ✔ [2 / 4]: cor
    #> ✔ [3 / 4]: cov
    #> ✔ [4 / 4]: cov

    #> # A tibble: 94 x 7
    #>    type  fn_name parameter  parameter_type operation        content yaml_hash   
    #>    <chr> <chr>   <chr>      <chr>          <chr>            <chr>   <chr>       
    #>  1 dummy var     x          integer vector Integer vector … <NA>    bcf42b882bb…
    #>  2 dummy var     x          generic vector Custom class de… <NA>    bcf42b882bb…
    #>  3 dummy var     x          generic vector Convert vector … <NA>    bcf42b882bb…
    #>  4 dummy var     na.rm      single logical Negate default … <NA>    bcf42b882bb…
    #>  5 dummy var     na.rm      single logical Substitute inte… <NA>    bcf42b882bb…
    #>  6 dummy var     na.rm      single logical Substitute char… <NA>    bcf42b882bb…
    #>  7 dummy var     na.rm      single logical Length 2 vector… <NA>    bcf42b882bb…
    #>  8 dummy var     (return o… <NA>           Check that desc… <NA>    bcf42b882bb…
    #>  9 dummy var     (return o… <NA>           Check whether d… <NA>    bcf42b882bb…
    #> 10 dummy var     (return o… <NA>           Compare class o… <NA>    bcf42b882bb…
    #> # … with 84 more rows

That result details the 130 tests which would be applied to the `var`
function from the `stats` package. These 130 tests yield the following
results:

``` r
autotest_package (package = "stats", functions = "var", test = TRUE)
#> ── autotesting stats ──
#> 
#> ✔ [1 / 4]: var
#> ✔ [2 / 4]: cor
#> ✔ [3 / 4]: cov
#> ✔ [4 / 4]: cov
#> # A tibble: 9 x 7
#>   type   fn_name parameter parameter_type  operation   content       yaml_hash  
#>   <chr>  <chr>   <chr>     <chr>           <chr>       <chr>         <chr>      
#> 1 diagn… var     x         generic vector  Convert ve… "Function [v… bcf42b882b…
#> 2 diagn… var     y         generic vector  Convert ve… "Function [v… bcf42b882b…
#> 3 diagn… cor     x         generic vector  Convert ve… "Function [c… bcf42b882b…
#> 4 diagn… cor     y         generic vector  Convert ve… "Function [c… bcf42b882b…
#> 5 diagn… cor     use       single charact… upper-case… "is case dep… bcf42b882b…
#> 6 diagn… cor     use       single charact… upper-case… "is case dep… 4e21cddacf…
#> 7 diagn… cov     use       single charact… upper-case… "is case dep… 4e21cddacf…
#> 8 diagn… cov     use       single charact… upper-case… "is case dep… 8dc19144c4…
#> 9 diagn… cov     use       single charact… upper-case… "is case dep… 87d5da3d7a…
```

How Does It Work (Part 2)
-------------------------

The tests themselves are controlled by `yaml` files, internally
generated by calling `autotest_package()`. These `yaml` test
specifications can be explicitly extracted for a given package or
function(s) thereof with the `examples_to_yaml()` function, as
demonstrated in the following code.

``` r
yaml <- examples_to_yaml (package = "stats", functions = "var")
length (yaml)
#> [1] 4
print (yaml [[1]], width = 20)
#>  [1] "package: stats"                    
#>  [2] "functions:"                        
#>  [3] "    - var:"                        
#>  [4] "        - preprocess:"             
#>  [5] "            - 'Cl <- cor(longley)'"
#>  [6] "        - parameters:"             
#>  [7] "            - x: 1:10"             
#>  [8] "    - var:"                        
#>  [9] "        - preprocess:"             
#> [10] "            - 'Cl <- cor(longley)'"
#> [11] "        - parameters:"             
#> [12] "            - x: 1:5"              
#> [13] "            - y: 1:5"              
#> [14] "    - cor:"                        
#> [15] "        - preprocess:"             
#> [16] "            - 'Cl <- cor(longley)'"
#> [17] "        - parameters:"             
#> [18] "            - x: 1:10"             
#> [19] "            - y: 2:11"             
#> [20] "    - cor:"                        
#> [21] "        - preprocess:"             
#> [22] "            - 'Cl <- cor(longley)'"
#> [23] "        - parameters:"             
#> [24] "            - x: longley"          
#> attr(,"package")
#> [1] "stats"
```

That `yaml` contains the values of all parameters used in one function
call within the documented example code. Only explicitly specified
values are declared, with all other parameters assuming default values.
All parameters of a function are then mutated, whether explicitly
specified or default.

The “preprocess” lines of the `yaml` include any lines which assign
values to named objects, and which may be used in any subsequent line of
a given `yaml`. That `yaml` can also be used to generate lists of tests
(with `test = FALSE`), or results of applying those tests (with
`test = TRUE`), using the function, `autotest_yaml()`:

``` r
autotest_yaml (yaml = yaml, test = FALSE)
#> ✔ var
#> ✔ var
#> ✔ cor
#> ✔ cor
#> ✔ cor
#> ✔ cov
#> ✔ cov
#> ✔ cov
#> ✔ cov
#> # A tibble: 94 x 7
#>    type  fn_name parameter  parameter_type operation        content yaml_hash   
#>    <chr> <chr>   <chr>      <chr>          <chr>            <chr>   <chr>       
#>  1 dummy var     x          integer vector Integer vector … <NA>    bcf42b882bb…
#>  2 dummy var     x          generic vector Custom class de… <NA>    bcf42b882bb…
#>  3 dummy var     x          generic vector Convert vector … <NA>    bcf42b882bb…
#>  4 dummy var     na.rm      single logical Negate default … <NA>    bcf42b882bb…
#>  5 dummy var     na.rm      single logical Substitute inte… <NA>    bcf42b882bb…
#>  6 dummy var     na.rm      single logical Substitute char… <NA>    bcf42b882bb…
#>  7 dummy var     na.rm      single logical Length 2 vector… <NA>    bcf42b882bb…
#>  8 dummy var     (return o… <NA>           Check that desc… <NA>    bcf42b882bb…
#>  9 dummy var     (return o… <NA>           Check whether d… <NA>    bcf42b882bb…
#> 10 dummy var     (return o… <NA>           Compare class o… <NA>    bcf42b882bb…
#> # … with 84 more rows
autotest_yaml (yaml = yaml, test = TRUE)
#> ✔ var
#> ✔ var
#> ✔ cor
#> ✔ cor
#> ✔ cor
#> ✔ cov
#> ✔ cov
#> ✔ cov
#> ✔ cov
#> # A tibble: 9 x 7
#>   type   fn_name parameter parameter_type  operation   content       yaml_hash  
#>   <chr>  <chr>   <chr>     <chr>           <chr>       <chr>         <chr>      
#> 1 diagn… var     x         generic vector  Convert ve… "Function [v… bcf42b882b…
#> 2 diagn… var     y         generic vector  Convert ve… "Function [v… bcf42b882b…
#> 3 diagn… cor     x         generic vector  Convert ve… "Function [c… bcf42b882b…
#> 4 diagn… cor     y         generic vector  Convert ve… "Function [c… bcf42b882b…
#> 5 diagn… cor     use       single charact… upper-case… "is case dep… bcf42b882b…
#> 6 diagn… cor     use       single charact… upper-case… "is case dep… 4e21cddacf…
#> 7 diagn… cov     use       single charact… upper-case… "is case dep… 4e21cddacf…
#> 8 diagn… cov     use       single charact… upper-case… "is case dep… 8dc19144c4…
#> 9 diagn… cov     use       single charact… upper-case… "is case dep… 87d5da3d7a…
```

Prior work
----------

1.  The
    [`great-expectations`](https://github.com/great-expectations/great_expectations)
    framework for python, described in [this medium
    article](https://medium.com/@expectgreatdata/down-with-pipeline-debt-introducing-great-expectations-862ddc46782a).
2.  [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) for
    Haskell
3.  [`mutate`](https://github.com/mbj/mutant) for ruby.
