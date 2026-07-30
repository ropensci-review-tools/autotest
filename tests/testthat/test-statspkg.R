context ("stats-package")

test_that ("autotest var", {

    package <- "stats"
    functions <- "var"

    devs_before <- grDevices::dev.list ()
    rplots <- file.path (getwd (), "Rplots.pdf")
    if (file.exists (rplots)) {
        file.remove (rplots)
    }

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

    # 'var'/'cor'/'cov' example code triggers real plotting as a side effect;
    # confirm that never leaks out to an open device or an 'Rplots.pdf' file.
    expect_identical (grDevices::dev.list (), devs_before)
    expect_false (file.exists (rplots))

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

test_that ("autotest_package progress falls back under knitr", {

    withr::local_options (list (knitr.in.progress = TRUE))

    out <- utils::capture.output (
        x <- autotest_package (
            package = "stats", functions = "var",
            test = FALSE, progress = "bar"
        ),
        type = "message"
    )
    # 'progress = "bar"' should silently fall back to "none" under knitr, so no
    # cli progress-bar output (ticks or ANSI clear-line sequences) should appear
    expect_false (any (grepl ("Testing functions|ETA|\\[K", out)))
    expect_is (x, "data.frame")
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
    # 'x' may legitimately be NULL here: 'autotest_package()' returns NULL when
    # every test passes cleanly, so 'nrow()' (unlike 'NROW()') would otherwise
    # yield NULL, silently turning this comparison into a no-op 'logical(0)'.
    expect_true (nrow (x0) > NROW (x))
})
