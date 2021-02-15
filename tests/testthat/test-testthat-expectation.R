
context("testthat expectation")

# The expect_autotest expectation is that tests which have been switched off
# should contain an additional 'note' column which explains why each one is not
# run.

test_that("expect_autotest", {

              test_data <- autotest_types (notest = "vector_to_list_col")
              x <- autotest_package (package = "stats",
                                     functions = "cov",
                                     test = TRUE,
                                     test_data = test_data)
       
              # That should fail becuase there is no 'note' column
              expect_failure (expect_autotest (x))

              x$note <- ""
              x [grep ("vector_to_list", x$test_name), "note"] <-
                  "these tests have been switched off because ..."

              expect_success (expect_autotest (x))
})
