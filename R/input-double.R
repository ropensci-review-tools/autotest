
test_double_is_int <- function (x = NULL, ...) {
    UseMethod ("test_double_is_int", x)
}

test_double_is_int.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "double_is_int",
                   parameter_type = "numeric",
                   operation = "Check whether double is only used as int",
                   content = "int parameters should have terminal 'L'")
}

test_double_is_int.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- NULL

    if (!is.null (test_data)) {
        r <- test_double_is_int.NULL ()
        x$test <- test_these_data (test_data, r)
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test)
        res <- double_is_int (x)
    else
        res <- dbl_is_int_dummy_report (x)

    return (res)
}

double_is_int <- function (x) {

    res <- NULL

    p <- x$params [[x$i]]

    if (!is.null (attr (p, "is_int"))) {
        if (attr (p, "is_int") &
            storage.mode (p) == "double") {

            res <- test_double_is_int.NULL ()
            res$type <- "diagnostic"
            res$fn_name <- x$fn
            res$parameter <- names (x$params) [x$i]
            res$content <- paste0 ("Parameter [",
                                    names (x$params) [x$i],
                                    "] is not specified as integer, yet ",
                                    "only used as such; please use '1L' ",
                                    "for integer, or 1.0 for non-integer ",
                                    "values.")
        }
    }

    return (res)
}

dbl_is_int_dummy_report <- function (x) {

    res <- test_double_is_int.NULL ()

    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    return (res)
}

test_double_noise <- function (x = NULL, ...) {
    UseMethod ("test_double_noise", x)
}

test_double_noise.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "trivial_noise",
                   parameter_type = "numeric",
                   operation = "Add trivial noise to numeric parameter",
                   content = "(Should yield same result)")
}

test_double_noise.autotest_obj <- function (x, test_data = NULL, ...) {

    res <- NULL

    if (!is.null (test_data)) {
        r <- test_double_noise.NULL ()
        x$test <- test_these_data (test_data, r)
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test)
        res <- double_noise (x)
    else
        res <- dbl_noise_dummy_report (x)

    return (res)
}

double_noise <- function (x) {

    res <- NULL

    seed <- sample.int (.Machine$integer.max, 1L)

    res0 <- tryCatch (
                withr::with_seed (seed,
                                  do.call (x$fn, x$params)),
                      error = function (e) NULL)

    if (!is.vector (res0))
        return (NULL) # can only test effects of noise on simple vector outputs
    if (!is.numeric (res0))
        return (NULL) # can only test effects on numeric output

    x$params [[x$i]] <- x$params [[x$i]] +
        stats::runif (length (x$params [[x$i]])) * 10 * .Machine$double.eps

    Sys.sleep (0.5) # in case Sys.time is used
    res1 <- tryCatch (
                withr::with_seed (seed,
                                  do.call (x$fn, x$params)),
                      error = function (e) NULL)

    if (!is.null (res0) & !is.null (res1)) {
        different <- length (res0) != length (res1)
        if (!different)
            different <- max (abs (res0 - res1), na.rm = TRUE) >
                                (10 * .Machine$double.eps)

        if (different) {
            res <- test_double_noise.NULL ()
            res$type <- "diagnostic"
            res$fn_name <- x$fn
            res$parameter <- names (x$params) [x$i]
            res$content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] yields different result when trivial ",
                                   "noise is added")
        }
    }

    return (res)
}

dbl_noise_dummy_report <- function (x) {

    res <- test_double_noise.NULL ()

    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    return (res)
}
