context ("test data")

test_that ("no matching test_name", {

    test_data <- data.frame (
        test_name = "other_test",
        fn_name = "fn1",
        parameter = "x",
        test = TRUE,
        stringsAsFactors = FALSE
    )
    obj <- list (test_name = "this_test", fn_name = "fn1", parameter = "x")

    res <- test_these_data (test_data, obj)
    expect_identical (length (res), 0L)
})

test_that ("single unambiguous match", {

    test_data <- data.frame (
        test_name = "this_test",
        fn_name = "fn1",
        parameter = "x",
        test = FALSE,
        stringsAsFactors = FALSE
    )
    obj <- list (test_name = "this_test", fn_name = "fn1", parameter = "x")

    res <- test_these_data (test_data, obj)
    expect_identical (res, FALSE)
})

test_that ("narrows by fn_name", {

    test_data <- data.frame (
        test_name = c ("this_test", "this_test"),
        fn_name = c ("fn1", "fn2"),
        parameter = c ("x", "x"),
        test = c (TRUE, FALSE),
        stringsAsFactors = FALSE
    )
    obj <- list (test_name = "this_test", fn_name = "fn1", parameter = "x")

    res <- test_these_data (test_data, obj)
    expect_identical (res, TRUE)
})

test_that ("narrows by parameter after fn_name", {

    test_data <- data.frame (
        test_name = rep ("this_test", 3),
        fn_name = rep ("fn1", 3),
        parameter = c ("x", "y", "y"),
        test = c (TRUE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )
    obj <- list (test_name = "this_test", fn_name = "fn1", parameter = "x")

    res <- test_these_data (test_data, obj)
    expect_identical (res, TRUE)
})

test_that ("ambiguous test flag errors", {

    test_data <- data.frame (
        test_name = rep ("this_test", 2),
        fn_name = rep ("fn1", 2),
        parameter = rep ("x", 2),
        test = c (TRUE, FALSE),
        stringsAsFactors = FALSE
    )
    obj <- list (test_name = "this_test", fn_name = "fn1", parameter = "x")

    expect_error (
        test_these_data (test_data, obj),
        "Cannot determine single 'test' flag"
    )
})
