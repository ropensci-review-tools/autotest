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

        fns_short <- vapply (fns, function (i) strsplit (i, "\\.") [[1]] [1],
                             character (1), USE.NAMES = FALSE)
        expect_is (fns_short, "character")
        expect_equal (length (fns_short), length (fns))

              exs <- get_all_examples (package, is_source, exclude)

              expect_is (exs, "list")
              expect_equal (length (exs), 1)
              expect_true (length (exs [[1]]) > 1)

        ex <- examples_to_yaml (package = package, functions = functions)
        expect_is (ex, "list")
        expect_true (length (ex) >= 1)

        #x <- autotest (yaml = ex, test = FALSE)
        #expect_is (x, "tbl_df")
        #expect_true (all (x$type == "dummy"))
        #expect_false (all (x$fn_name == functions))
        #expect_true (all (unique (x$fn_name) %in% c ("cov", "cor", "var")))
})
