
context("testthat expectation")

# Note that local packages should just use the expectations
# `expect_autotest_no_testdata`, or `expect_autotest_testdata`. This test file
# demonstrates finer-level testthat expectations for which a pre-derived
# `autotest` object can be submitted.
#
# The expect_autotest_notes expectation is that tests which have been switched
# off should contain an additional 'note' column which explains why each one is
# not run.

test_that("expect_autotest", {

              x <- autotest_package (package = "stats",
                                     functions = "cov",
                                     test = TRUE)

              expect_success (expect_autotest_no_err (x))
              # This should be expect_success, but cov generates warnings about
              # parameter usage not being demonstrated in examples, so:
              expect_failure (expect_autotest_no_warn (x))
              expect_success (expect_autotest_notes (x))

              test_data <- autotest_types (notest = "vector_to_list_col")
              x <- autotest_package (package = "stats",
                                     functions = "cov",
                                     test = TRUE,
                                     test_data = test_data)
       
              # That should fail becuase there is no 'note' column
              #expect_failure (expect_autotest_notes (x))
              # ... but turned off because of #61

              x$note <- ""
              x [grep ("vector_to_list", x$test_name), "note"] <-
                  "these tests have been switched off because ..."

              # Adding a note column leads to success:
              expect_success (expect_autotest_notes (x))
})
