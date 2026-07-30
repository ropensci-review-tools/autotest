context ("stats-package")

test_that ("get-examples", {

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
    expect_length (exs, 1)
    expect_true (length (exs [[1]]) > 1)
    expect_length (exs [[1]], 6)
    expect_length (exs [[1]] [[6]], 4)
})

test_that ("autotest var", {

    package <- "stats"
    functions <- "var"

    expect_message (
        x_f <- autotest_package (
            package = package, functions = functions,
            test = FALSE
        )
    )
    expect_message (
        x_t <- autotest_package (
            package = package, functions = functions,
            test = TRUE
        )
    )
    expect_true (nrow (x_f) > nrow (x_t))

    for (x in list (x_f, x_t)) {
        expect_is (x, "data.frame")
        expect_equal (ncol (x), 8)
        expect_identical (names (x), c (
            "type",
            "test_name",
            "fn_name",
            "parameter",
            "parameter_type",
            "operation",
            "content",
            "test"
        ))
    }
})

test_that ("autotest rnorm", {

    package <- "stats"
    functions <- "rnorm"

    expect_message (
        x0 <- autotest_package (
            package = package, functions = functions,
            test = FALSE
        )
    )
    expect_message (
        x <- autotest_package (
            package = package, functions = functions,
            test = TRUE
        )
    )
    expect_true (nrow (x0) > nrow (x))
})
