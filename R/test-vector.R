autotest_vector <- function (params, this_fn, classes, quiet) {

    ret <- NULL
    f <- file.path (tempdir (), "junk.txt")

    null_params <- NULL
    if (any (params == "NULL")) {
        null_params <- params [params == "NULL"]
        params <- params [params != "NULL"]
    }

    vec_index <- which (vapply (params, function (i)
                                length (i) > 1 &&
                                    is.null (dim (i)) &&
                                    !class (i) %in% c ("call", "formula"),
                                logical (1)))
    for (v in vec_index) {
        params_v <- params
        msgs <- catch_all_msgs (f, this_fn, params_v)
        ret <- add_msg_output (ret, msgs, types = "warning",
                               operation = "normal function call")
        warn <- not_null_and_is (msgs, "warning")

        # int columns submitted as double should return different result:
        if (typeof (params_v [[v]]) == "integer" & !is.factor (params_v [[v]])) {
            res1 <- suppressWarnings (do.call (this_fn, params_v))
            params_v [[v]] <- as.numeric (params_v [[v]])
            res2 <- suppressWarnings (do.call (this_fn, params_v))
            if (!identical (res1, res2)) {
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = this_fn,
                                             parameter = names (params_v) [v],
                                             operation = "integer vector converted to numeric",
                                             content = paste0 ("Function [",
                                                               this_fn,
                                                               "] returns different values when ",
                                                               "assumed int-valued parameter [",
                                                               names (params) [v],
                                                               "] is submitted as double. ",
                                                               "Error message: ",
                                                               "different classes when ",
                                                               "submitted as ",
                                                               names (params) [v])))
            }
            params_v <- params
        }

        # class definitions for vector columns should be ignored
        if (!names (params_v) [vec_index [v]] %in% names (classes)) {
            x <- params_v [[v]]
            class (x) <- "different"
            params_v [[v]] <- x
            msgs <- catch_all_msgs (f, this_fn, params_v)
            if (not_null_and_is (msgs, "error")) {
                index <- which (msgs$type == "error")
                for (e in index) {
                    ret <- rbind (ret,
                                  report_object (type = "diagnostic",
                                                 fn_name = this_fn,
                                                 parameter = names (params_v) [v],
                                                 operation = "custom class definitions for vector input",
                                                 content = paste0 ("Function [",
                                                                   this_fn,
                                                                   "] errors on vector columns with ",
                                                                   "different classes when ",
                                                                   "submitted as ",
                                                                   names (params_v) [v],
                                                                   " Error message: ",
                                                                   msgs$content [e])))
                }
            } else {
                # TODO: Expectation - they need not be identical, because class
                # def may be carried over to result
                #expect_identical (res1, res3)
            }
            params_v <- params
        } else {
            # TODO: Implement check for all nominated classes
        }

        # List-columns
        x <- params_v [[v]]
        x <- I (as.list (x))
        params_v [[v]] <- x
        msgs <- catch_all_msgs (f, this_fn, params_v)
        if (not_null_and_is (msgs, "error")) {
            index <- which (msgs$type == "error")
            for (e in index) {
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = this_fn,
                                             parameter = names (params_v) [v],
                                             operation = "convert vector input to list-columns",
                                             content = paste0 ("Function [",
                                                               this_fn,
                                                               "] errors on list-columns ",
                                                               "when submitted as ",
                                                               names (params) [v],
                                                               " Error message: ",
                                                               msgs$content [e])))
            }
        }
    }

    return (ret)
}
