autotest_vector <- function (params,
                             param_types,
                             this_fn,
                             classes,
                             test = TRUE,
                             quiet) {

    ret <- NULL
    f <- file.path (tempdir (), "junk.txt")

    if (any (params == "NULL")) {
        params <- params [params != "NULL"]
    }

    vec_index <- which (param_types == "vector")
    for (v in vec_index) {
        params_v <- params
        msgs <- catch_all_msgs (f, this_fn, params_v)
        ret <- add_msg_output (ret, msgs, types = "warning",
                               operation = "normal function call")

        if (typeof (params_v [[v]]) == "integer" &
            !is.factor (params_v [[v]])) {
            ret <- rbind (ret,
                          int_as_double (this_fn, params_v, v, test))
        }

        ret <- rbind (ret,
                      vector_class_defs (this_fn, params_v, classes, v, test))

        # List-columns
        x <- params_v [[v]]
        x <- I (as.list (x))
        params_v [[v]] <- x
        msgs <- catch_all_msgs (f, this_fn, params_v)
        if (not_null_and_is (msgs, "error")) {
            index <- which (msgs$type == "error")
            for (e in index) {
                operation <- "convert vector input to list-columns"
                content <- paste0 ("Function [",
                                   this_fn,
                                   "] errors on list-columns ",
                                   "when submitted as ",
                                   names (params) [v],
                                   " Error message: ",
                                   msgs$content [e])
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = this_fn,
                                             parameter = names (params_v) [v],
                                             parameter_type = "generic vector",
                                             operation = operation,
                                             content = content))
            }
        }
    }

    return (ret)
}

# class definitions for vector columns should be ignored
vector_class_defs <- function (this_fn, params, classes, i, test = TRUE) {

    operation <- "custom class definitions for vector input"
    res0 <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = "generic vector",
                          operation = operation)

    if (test) {

        if (!names (params) [i] %in% names (classes)) {
            f <- tempfile (fileext = ".txt")
            x <- params [[i]]
            class (x) <- "different"
            params [[i]] <- x
            msgs <- catch_all_msgs (f, this_fn, params)
            if (not_null_and_is (msgs, "error")) {
                index <- which (msgs$type == "error")
                res <- NULL
                res0$type <- "diagnostic"
                for (e in index) {
                    res0$content <- paste0 ("Function [",
                                            this_fn,
                                            "] errors on vector columns with ",
                                            "different classes when ",
                                            "submitted as ",
                                            names (params) [i],
                                            " Error message: ",
                                            msgs$content [e])

                    res <- rbind (res, res0)
                }
            } else {
                res <- NULL
            }
        } else {
            res <- NULL
        }
    }

    return (res)
}
