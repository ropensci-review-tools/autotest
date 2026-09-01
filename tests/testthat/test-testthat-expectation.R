context ("testthat expectation")

# Note that local packages should just use the expectations
# `expect_autotest_no_testdata`, or `expect_autotest_testdata`. This test file
# demonstrates finer-level testthat expectations for which a pre-derived
# `autotest` object can be submitted.
#
# The expect_autotest_notes expectation is that tests which have been switched
# off should contain an additional 'note' column which explains why each one is
# not run.

test_that ("expect_autotest", {

    x <- autotest_package (
        package = "stats",
        functions = "cov",
        test = TRUE
    )

    # This should be expect_success, but cov's own documented example
    # explicitly demonstrates an error case via
    # `try(cov(swM, use = "all"))`; typetracer traces that literal call,
    # and autotest correctly flags it as an error when it re-calls `cov()`
    # directly (without the documentation's protective `try()`), so:
    expect_failure (expect_autotest_no_err (x))
    # This should be expect_success, but cov generates warnings about
    # parameter usage not being demonstrated in examples, so:
    expect_failure (expect_autotest_no_warn (x))
    expect_success (expect_autotest_notes (x))

    test_data <- autotest_types (notest = "vector_to_list_col")
    x <- autotest_package (
        package = "stats",
        functions = "cov",
        test = TRUE,
        test_data = test_data
    )

    # That should fail becuase there is no 'note' column
    # expect_failure (expect_autotest_notes (x))
    # ... but turned off because of #61

    x$note <- ""
    x [grep ("vector_to_list", x$test_name, fixed = TRUE), "note"] <-
        "these tests have been switched off because ..."

    # Adding a note column leads to success:
    expect_success (expect_autotest_notes (x))
})

test_that ("expect_autotest success paths", {

    # 'test_int'/'test_logical' alone (excluding 'test_rect', which flags a
    # documentation warning, and 'test_name', which is deliberately designed
    # to error) produce only "diagnostic" rows, so both expectations should
    # succeed against them.
    source ("../local-pkg.R")
    package <- make_pkg ()
    x <- autotest_package (
        package = package,
        functions = c ("test_int", "test_logical"),
        test = TRUE
    )
    expect_true (all (x$type == "diagnostic"))

    expect_success (expect_autotest_no_err (x))
    expect_success (expect_autotest_no_warn (x))
})

# 'expect_autotest_no_testdata()' and 'expect_autotest_testdata()' are
# deliberately not tested here. Both call
# 'autotest_package(here::here(), test = TRUE, ...)', and 'here::here()'
# resolves once per R session and cannot be redirected by 'setwd()'/
# 'withr::with_dir()' -- so calling either from within 'autotest's own test
# suite makes them trace (and thus re-execute) that same suite, including
# this file, from inside itself: unbounded self-recursion, not just slowness.
# 'autotest_trace_package()' always traces
# 'types = c("examples", "tests")' with no way to opt out, so there is
# currently no safe way to exercise these two functions from inside
# 'autotest's own tests/testthat/ suite.
