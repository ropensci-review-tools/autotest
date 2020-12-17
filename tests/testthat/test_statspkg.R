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

              fns <- functions
              topics <- m_fns_to_topics (fns, package)
              expect_is (topics, "data.frame")
              expect_equal (ncol (topics), 3)

              topic <- topics$topic
              expect_length (topic, 1)
              expect_is (topic, "character")
              fns <- find_fn_call_points (topic, package)
              expect_is (fns, "character")
              expect_true (length (fns) > 1)

              #exs <- get_all_examples (package, is_source, exclude)

              #expect_is (exs, "list")
              #expect_equal (length (exs), 1)
              #expect_true (length (exs [[1]]) > 1)
})
