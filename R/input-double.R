
test_double_noise <- function (x = NULL, ...) {
    UseMethod ("test_double_noise", x)
}

test_double_noise.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   parameter_type = "numeric",
                   operation = "Add trivial noise to numeric parameter",
                   content = "(Should yield same result)")
}

test_double_noise.autotest_obj <- function (x, ...) {

    res <- NULL

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
        if (!identical (res0, res1)) {
            op <- "Add trivial noise to numeric parameter"
            content <- paste0 ("Parameter [",
                               names (x$params) [x$i],
                               "] yields different result when trivial ",
                               "noise is added")
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = x$fn,
                                         parameter = names (x$params) [x$i],
                                         parameter_type = "single numeric",
                                         operation = op,
                                         content = content))
        }
    }
}

dbl_noise_dummy_report <- function (x) {
    report_object (type = "dummy",
                   fn_name = x$fn,
                   parameter = names (x$params) [x$i],
                   parameter_type = "single numeric",
                   operation = "Add trivial noise",
                   content = "(Should yield same result)")
}
