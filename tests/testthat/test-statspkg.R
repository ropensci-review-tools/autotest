context ("stats-package")

test_that ("autotest var", {

    package <- "stats"
    functions <- "var"

    expect_message (
        x_f <- autotest_package (
            package = package, functions = functions,
            test = FALSE, progress = "tests"
        )
    )
    expect_message (
        x_t <- autotest_package (
            package = package, functions = functions,
            test = TRUE, progress = "tests"
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
            test = FALSE, progress = "tests"
        )
    )
    expect_message (
        x <- autotest_package (
            package = package, functions = functions,
            test = TRUE, progress = "tests"
        )
    )
    expect_true (nrow (x0) > nrow (x))
})
