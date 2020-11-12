
test_single_double <- function (pkg, this_fn, params, i) {

    res <- NULL

    res0 <- tryCatch (do.call (this_fn, params),
                      error = function (e) NULL)
    params [[i]] <- params [[i]] + 10 * .Machine$double.eps
    res1 <- tryCatch (do.call (this_fn, params),
                     error = function (e) NULL)

    if (!is.null (res0) & !is.null (res1)) {
        if (!identical (res0, res1)) {
            op <- "add trivial noise to double parameter"
            content <- paste0 ("Parameter [",
                               names (params) [i],
                               "] yields different result when trivial ",
                               "noise is added")
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [i],
                                         operation = op,
                                         content = content))
        }
    }
}
