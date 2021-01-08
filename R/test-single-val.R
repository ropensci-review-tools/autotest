autotest_single <- function (test_obj) {

    if (any (test_obj$params == "NULL")) {
        test_obj$params <- test_obj$params [test_obj$params != "NULL"]
    }

    f <- tempfile (fileext = ".txt")
    res <- NULL
    if (test_obj$test)
        res <- catch_all_msgs (f, test_obj$fn, test_objparams)
    if (!is.null (res))
        res$operation <- "normal function call"

    index <- which (test_obj$param_types == "single")
    for (i in index) {

        test_obj$i <- i

        p_i <- test_obj$params [[i]]
        val_type <- NULL
        check_vec <- TRUE
        if (is_int (p_i)) {
            val_type <- "integer"

            res <- rbind (res, test_single_int (test_obj))

            res <- rbind (res,
                          int_as_double (test_obj$fn,
                                         test_obj$params,
                                         test_obj$i,
                                         vec = FALSE,
                                         test_obj$test))
        } else if (methods::is (p_i, "numeric")) {
            val_type <- "numeric"
            res <- rbind (res,
                          test_single_double (test_obj$fn,
                                              test_obj$params,
                                              test_obj$i,
                                              test_obj$test))
        } else if (is.character (p_i)) {
            val_type <- "character"
            res <- rbind (res,
                          test_single_char (test_obj$fn,
                                            test_obj$params,
                                            test_obj$i,
                                            test_obj$test))
        } else if (is.logical (p_i)) {
            val_type <- "logical"
            res <- rbind (res,
                          test_single_logical (test_obj$fn,
                                               test_obj$params,
                                               test_obj$i,
                                               test_obj$test))
        } else if (methods::is (p_i, "name") |
                   methods::is (p_i, "formula")) {
            val_type <- class (p_i) [1]
            res <- rbind (res,
                          test_single_name (test_obj$package,
                                            test_obj$fn,
                                            test_obj$params,
                                            test_obj$i,
                                            test_obj$test))
            check_vec <- FALSE
        } else {
            check_vec <- FALSE
        }

        # check response to vector input:
        if (check_vec) {
            res <- rbind (res,
                          single_doubled (test_obj$fn,
                                          test_obj$params,
                                          test_obj$i,
                                          val_type,
                                          test_obj$test))
        }
    }

    return (res [which (!duplicated (res)), ])
}

#' Do input values presumed to have length one give errors when vectors of
#' length > 1 are passed?
#' @noRd
single_doubled <- function (this_fn, params, i, val_type, test = TRUE) {

    operation <- "Length 2 vector for length 1 parameter"
    res <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = paste0 ("single ", val_type),
                          operation = operation,
                          content = "Should trigger message, warning, or error")

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
