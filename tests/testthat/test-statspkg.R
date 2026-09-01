context ("stats-package")

test_that ("autotest var", {

    # typetracer::reload_pkg() mis-parses Windows tempdir() paths as regular
    # expressions (grepl() called without fixed = TRUE), causing this to error
    # on Windows. Fixed in typetracer dev version; remove this skip once CRAN
    # typetracer > 0.2.5.
    skip_on_os ("windows")

    package <- "stats"
    functions <- "var"

    devs_before <- grDevices::dev.list ()
    rplots <- fs::path (getwd (), "Rplots.pdf")
    if (fs::file_exists (rplots)) {
        fs::file_delete (rplots)
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
    expect_gt (nrow (x_f), nrow (x_t))

    # 'var'/'cor'/'cov' example code triggers real plotting as a side effect;
    # confirm that never leaks out to an open device or an 'Rplots.pdf' file.
    expect_identical (grDevices::dev.list (), devs_before)
    expect_false (fs::file_exists (rplots))

    for (x in list (x_f, x_t)) {
        expect_is (x, "data.frame")
        expect_identical (ncol (x), 8L)
        expect_named (x, c (
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

    # typetracer::reload_pkg() mis-parses Windows tempdir() paths as regular
    # expressions (grepl() called without fixed = TRUE), causing this to error
    # on Windows. Fixed in typetracer dev version; remove this skip once CRAN
    # typetracer > 0.2.5.
    skip_on_os ("windows")

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

    # typetracer::reload_pkg() mis-parses Windows tempdir() paths as regular
    # expressions (grepl() called without fixed = TRUE), causing this to error
    # on Windows. Fixed in typetracer dev version; remove this skip once CRAN
    # typetracer > 0.2.5.
    skip_on_os ("windows")

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
    expect_gt (nrow (x0), NROW (x))
})
