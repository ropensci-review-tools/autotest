context("stats-package")

test_that("get-examples", {

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

test_that ("examples_to_yaml", {

    package <- "stats"
    functions <- "var"
    exclude <- NULL

    exclude <- exclude_functions (package, functions, exclude)
    exs <- examples_to_yaml (package, exclude = exclude)
    expect_is (exs, "list")
    expect_length (exs, 4)
    expect_true (all (names (exs) == "var"))
})

test_that ("autotest var", {

    package <- "stats"
    functions <- "var"
    exclude <- NULL

    exclude <- exclude_functions (package, functions, exclude)
    exs <- examples_to_yaml (package, exclude = exclude)

    yaml <- exs [[1]]
    expect_silent (
        x0 <- autotest_yaml (yaml, quiet = TRUE)
        )
    expect_message (
        x_t <- autotest_yaml (yaml, test = TRUE)
        )
    #expect_identical (x0, x_t)

    expect_message (
        x_f <- autotest_yaml (yaml, test = FALSE)
        )
    #expect_true (nrow (x_f) > nrow (x_t))
    expect_true (is.null (x_t))

    expect_is (x_f, "data.frame")
    expect_equal (ncol (x_f), 9)
    expect_identical (names (x_f), c ("type",
                                      "test_name",
                                      "fn_name",
                                      "parameter",
                                      "parameter_type",
                                      "operation",
                                      "content",
                                      "test",
                                      "yaml_hash"))

    f <- tempfile (fileext = ".yaml")
    con <- file (f)
    writeLines (yaml, con = con)
    close (con)
    expect_true (file.exists (f))

    expect_message (
        x_f_file <- autotest_yaml (filename = f, test = FALSE)
        )
    expect_message (
        x_t_file <- autotest_yaml (filename = f, test = TRUE)
        )
    expect_identical (x_f, x_f_file)
    expect_identical (x_t, x_t_file)
})

test_that ("autotest rnorm", {

    package <- "stats"
    functions <- "rnorm"

    expect_message (
        x0 <- autotest_package (package = package, functions = functions,
                                test = FALSE)
    )
    expect_message (
        x <- autotest_package (package = package, functions = functions,
                               test = TRUE)
    )
    expect_true (nrow (x0) > nrow (x))
})
