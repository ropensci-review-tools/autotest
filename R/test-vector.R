autotest_vector <- function (test_obj) {

    ret <- NULL
    f <- tempfile (fileext = ".txt")

    if (any (test_obj$params == "NULL")) {
        test_obj$params <- test_obj$params [test_obj$params != "NULL"]
    }

    vec_index <- which (test_obj$param_types == "vector")
    for (v in vec_index) {
        params_v <- test_obj$params
        if (test_obj$test) {
            msgs <- catch_all_msgs (f, test_obj$fn, params_v)
            ret <- add_msg_output (ret, msgs, types = "warning",
                                   operation = "normal function call")
        }

        if (typeof (params_v [[v]]) == "integer" &
            !is.factor (params_v [[v]])) {
            ret <- rbind (ret,
                          int_as_double (test_obj$fn,
                                         params_v,
                                         v,
                                         vec = TRUE,
                                         test_obj$test))
        }

        ret <- rbind (ret,
                      vector_class_defs (test_obj$fn,
                                         params_v,
                                         test_obj$classes,
                                         v,
                                         test_obj$test))

        ret <- rbind (ret,
                      vector_as_list (test_obj$fn,
                                      params_v,
                                      v,
                                      test_obj$test))
    }

    return (ret)
}

# class definitions for vector columns should be ignored
vector_class_defs <- function (this_fn, params, classes, i, test = TRUE) {

    operation <- "Custom class definitions for vector input"
    res0 <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = "generic vector",
                          operation = operation,
                          content = "(Should yield same result)")

    if (test) {

        if (!names (params) [i] %in% names (classes)) {
            x <- params [[i]]
            class (x) <- "different"
            params [[i]] <- x

            f <- tempfile (fileext = ".txt")
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
    } else {
        res <- res0
    }

    return (res)
}

vector_as_list <- function (this_fn, params, i, test = TRUE) {

    operation <- "Convert vector input to list-columns"
    res0 <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = "generic vector",
                          operation = operation,
                          content = "(Should yield same result)")

    if (test) {

        x <- params [[i]]
        x <- I (as.list (x))
        params [[i]] <- x

        f <- tempfile (fileext = ".txt")
        msgs <- catch_all_msgs (f, this_fn, params)

        if (not_null_and_is (msgs, "error")) {
            index <- which (msgs$type == "error")
            res <- NULL
            res0$type <- "diagnostic"
            for (e in index) {
                res0$content <- paste0 ("Function [",
                                        this_fn,
                                        "] errors on list-columns ",
                                        "when submitted as ",
                                        names (params) [i],
                                        " Error message: ",
                                        msgs$content [e])
                res <- rbind (res, res0)
            }
        } else {
            res <- NULL
        }
    } else {
        res <- res0
    }

    return (res)
}
