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
    expect_length (exs, 1)
    expect_true (length (exs [[1]]) > 1)
    expect_length (exs [[1]], 6)
    expect_length (exs [[1]] [[6]], 4)

    exs <- examples_to_yaml (package, exclude = exclude)
    expect_is (exs, "list")
    expect_length (exs, 4)
    expect_true (all (names (exs) == "var"))


    yaml <- exs [[1]]
    expect_message (
        x <- autotest (yaml)
        )
    expect_is (x, "data.frame")
    expect_equal (ncol (x), 7)
    expect_identical (names (x), c ("type",
                                    "fn_name",
                                    "parameter",
                                    "parameter_type",
                                    "operation",
                                    "content",
                                    "yaml_hash"))

    f <- tempfile (fileext = ".yaml")
    con <- file (f)
    writeLines (yaml, con = con)
    close (con)
    expect_true (file.exists (f))

    expect_silent (
        x2 <- autotest (filename = f, quiet = TRUE)
        )
    expect_identical (x, x2)
})
