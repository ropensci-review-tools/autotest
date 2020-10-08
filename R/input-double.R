
test_single_double <- function (pkg, this_fn, params, i) {

    res <- NULL

    res0 <- tryCatch (do.call (this_fn, params),
                      error = function (e) NULL)
    params [[i]] <- params [[i]] + 10 * .Machine$double.eps
    res <- tryCatch (do.call (this_fn, params),
                     error = function (e) NULL)

    if (!is.null (res0) & !is.null (res)) {
        if (!identical (res0, res)) {
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [i],
                                         operation = "add trivial noise to double parameter",
                                         content = paste0 ("Parameter [",
                                                           names (params) [i],
                                                           "] yields different result when trivial ",
                                                           "noise is added")))
        }
    }
}
