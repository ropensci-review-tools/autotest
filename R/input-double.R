
test_single_double <- function (this_fn, params, i, test = TRUE) {

    res <- NULL

    if (test)
        res <- test_single_double_noise (this_fn, params, i)
    else
        res <- single_dbl_dummy_report (this_fn, params, i)

    return (res)
}

test_single_double_noise <- function (this_fn, params, i) {

    res <- NULL

    res0 <- tryCatch (do.call (this_fn, params),
                      error = function (e) NULL)
    params [[i]] <- params [[i]] + 10 * .Machine$double.eps
    res1 <- tryCatch (do.call (this_fn, params),
                     error = function (e) NULL)

    if (!is.null (res0) & !is.null (res1)) {
        if (!identical (res0, res1)) {
            op <- "Add trivial noise to double parameter"
            content <- paste0 ("Parameter [",
                               names (params) [i],
                               "] yields different result when trivial ",
                               "noise is added")
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [i],
                                         parameter_type = "single numeric",
                                         operation = op,
                                         content = content))
        }
    }
}

single_dbl_dummy_report <- function (this_fn, params, i) {
    report_object (type = "dummy",
                   fn_name = this_fn,
                   parameter = names (params) [i],
                   parameter_type = "single numeric",
                   operation = "Add trivial noise",
                   content = "(Should yield same result)")
}
