autotest_single <- function (pkg,
                             params,
                             param_types,
                             this_fn,
                             test,
                             quiet) {

    if (any (params == "NULL")) {
        params <- params [params != "NULL"]
    }

    f <- file.path (tempdir (), "junk.txt")
    res <- NULL
    if (test)
        res <- catch_all_msgs (f, this_fn, params)
    if (!is.null (res))
        res$operation <- "normal function call"

    index <- which (param_types == "single")
    for (i in index) {
            params_i <- params

            p_i <- params_i [[i]]
            val_type <- NULL
            check_vec <- TRUE
            if (is_int (p_i)) {
                val_type <- "integer"
                res <- rbind (res,
                              test_single_int (pkg, this_fn, params_i, i, test))
                res <- rbind (res,
                              int_as_double (this_fn, params_i, i, test))

            } else if (methods::is (p_i, "numeric")) {
                val_type <- "numeric"
                res <- rbind (res,
                              test_single_double (this_fn, params_i, i, test))
            } else if (is.character (p_i)) {
                val_type <- "character"
                res <- rbind (res,
                              test_single_char (this_fn, params_i, i, test))
            } else if (is.logical (p_i)) {
                val_type <- "logical"
                res <- rbind (res,
                              test_single_logical (this_fn, params_i, i, test))
            } else if (methods::is (p_i, "name") |
                       methods::is (p_i, "formula")) {
                val_type <- class (p_i) [1]
                res <- rbind (res,
                              test_single_name (pkg, this_fn, params_i, i, test))
                check_vec <- FALSE
            } else {
                check_vec <- FALSE
            }

            # check response to vector input:
            if (check_vec) {
                res <- rbind (res,
                              single_doubled (this_fn,
                                              params_i,
                                              i,
                                              val_type,
                                              test))
            }
    }

    return (res [which (!duplicated (res)), ])
}

#' Do input values presumed to have length one give errors when vectors of
#' length > 1 are passed?
#' @noRd
single_doubled <- function (this_fn, params, i, val_type, test = TRUE) {

    operation <- "length 2 vector for length 1 parameter"
    res <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = paste0 ("single ", val_type),
                          operation = operation)

    if (test) {

        params [[i]] <- rep (params [[i]], 2)
        f <- tempfile (fileext = ".txt")
        msgs <- catch_all_msgs (f, this_fn, params)

        if (not_null_and_is (msgs, c ("warning", "error")))
            res <- NULL # function call should warn or error
        else {
            res$type <- "diagnostic"
            res$content <- paste0 ("Parameter [",
                                   names (params) [i],
                                   "] of function [",
                                   this_fn,
                                   "] is assumed to be a single ",
                                   val_type,
                                   ", but responds to vectors of length > 1")
        }
    }

    return (res)
}
