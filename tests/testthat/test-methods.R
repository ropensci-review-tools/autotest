context ("methods")

test_that ("summary installed package", {

    x <- rbind (
        report_object (
            type = "error",
            fn_name = "fn1",
            content = "an error"
        ),
        report_object (
            type = "warning",
            fn_name = "fn1",
            content = "a warning"
        ),
        report_object (
            type = "message",
            fn_name = "fn2",
            content = "a message"
        ),
        report_object (
            type = "diagnostic",
            fn_name = "fn2",
            content = "a diagnostic"
        )
    )
    attr (x, "package") <- "autotest"
    attr (x, "packageName") <- "autotest"

    out <- NULL
    msgs <- testthat::capture_messages (
        out <- utils::capture.output (summary (x))
    )
    expect_true (any (grepl ("1 error", msgs)))
    expect_true (any (grepl ("1 warning", msgs)))
    expect_true (any (grepl ("1 message", msgs)))
    expect_true (any (grepl ("other diagnostics", msgs)))

    expect_true (any (grepl ("fn1", out)))
    expect_true (any (grepl ("fn2", out)))
})

test_that ("summary local source package", {

    source ("../local-pkg.R")
    d <- make_pkg ()

    x <- report_object (
        type = "diagnostic",
        fn_name = "test_int",
        content = "a diagnostic"
    )
    attr (x, "package") <- d
    attr (x, "packageName") <- "demo"

    msgs <- testthat::capture_messages (summary (x))
    expect_true (any (grepl ("autotesting package", msgs)))
})

test_that ("summary with no documented examples", {

    x <- rbind (
        report_object (
            type = "diagnostic",
            fn_name = "fn1",
            content = "a diagnostic"
        ),
        report_object (
            type = "dummy",
            fn_name = "fn2",
            content = "no documented example"
        )
    )
    attr (x, "package") <- "autotest"
    attr (x, "packageName") <- "autotest"

    msgs <- testthat::capture_messages (summary (x))
    expect_true (any (grepl (
        "which have no",
        msgs
    )))
    expect_true (any (grepl ("fn2", msgs)))
})

test_that ("summary with and without githash", {

    x <- report_object (
        type = "diagnostic",
        fn_name = "fn1",
        content = "a diagnostic"
    )
    attr (x, "package") <- "autotest"
    attr (x, "packageName") <- "autotest"

    msgs <- testthat::capture_messages (summary (x))
    expect_false (any (grepl ("git hash", msgs)))

    attr (x, "githash") <- "abc1234"
    msgs <- testthat::capture_messages (summary (x))
    expect_true (any (grepl ("git hash", msgs)))
    expect_true (any (grepl ("abc1234", msgs)))
})
