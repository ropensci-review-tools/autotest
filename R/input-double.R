
test_single_double <- function (x) {

    res <- NULL

    if (x$test)
        res <- test_single_double_noise (x)
    else
        res <- single_dbl_dummy_report (x)

    return (res)
}

test_single_double_noise <- function (x) {

    res <- NULL

    res0 <- tryCatch (do.call (x$fn, x$params),
                      error = function (e) NULL)
    x$params [[x$i]] <- x$params [[x$i]] + 10 * .Machine$double.eps
    res1 <- tryCatch (do.call (x$fn, x$params),
                     error = function (e) NULL)

    if (!is.null (res0) & !is.null (res1)) {
        if (!identical (res0, res1)) {
            op <- "Add trivial noise to double parameter"
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

single_dbl_dummy_report <- function (x) {
    report_object (type = "dummy",
                   fn_name = x$fn,
                   parameter = names (x$params) [x$i],
                   parameter_type = "single numeric",
                   operation = "Add trivial noise",
                   content = "(Should yield same result)")
}
