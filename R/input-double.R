
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

    res0 <- tryCatch (do.call (x$fn, x$params),
                      error = function (e) NULL)
    x$params [[x$i]] <- x$params [[x$i]] +
        stats::runif (length (x$params [[x$i]])) * 10 * .Machine$double.eps
    res1 <- tryCatch (do.call (x$fn, x$params),
                     error = function (e) NULL)

    if (!is.null (res0) & !is.null (res1)) {
        tol <- abs (res0 - res1)
        if (!tol <= (10 * .Machine$double.eps)) {
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
