context("stats-package")

test_that("stats", {

              package <- "stats"
              functions <- "var"
              exclude <- NULL

              exclude <- exclude_functions (package, functions, exclude)
              expect_is (exclude, "character")
              expect_true (length (exclude) > 100)

              is_source <- pkg_is_source (package)
              expect_false (is_source)
              exs <- get_all_examples (package, is_source, exclude)

              expect_is (exs, "list")
              expect_equal (length (exs), 1)
              expect_true (length (exs [[1]]) > 1)
})
