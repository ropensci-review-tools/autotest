<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/autotest/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/autotest/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/mpadge/autotest/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/autotest)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

# autotest

Automatic testing of R packages via a simple YAML schema specifying one
or more example workflows for each function of a package. The simplest
workflows involve nominating data sets which may be submitted to a
function, while more complicated workflows may involve multiple data
sets, intermediate pre-processing stages, and other transformations
prior to submission to a nominated function. The following illustrates a
typical `yaml` schema, where items in angle-brackets (`<`, `>`) must be
completed.

    package: <package_name>
    functions:
        - <name of first function>:
            - data:
                - name: <name of a data set, in pkg::name format when from another package>
                - preprocess:
                    - "<R code required for pre-processing exlosed in quotation marks>"
                    - "<second line of pre-processing code>"
                    - "<more code>"
            - data:
                - name: <name of another data set which may be sumibted to funciton>
                - preprocess:
                    - "<more lines of pre-processing code>"
        - <name of secon function>::
            - data: <single data set>
            - preprocess: "<single line of pre-processing code>"

This `yaml` code should be in the `test` directory of a package – not in
the `test/testthat` directory. The above template can be generated via
the function `at_yaml_template()`.
