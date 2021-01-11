autotest_vector <- function (x) {

    ret <- NULL
    f <- tempfile (fileext = ".txt")

    if (any (x$params == "NULL")) {
        x$params <- x$params [x$params != "NULL"]
    }

    vec_index <- which (x$param_types == "vector")
    for (v in vec_index) {
        x$i <- v

        params_v <- x$params
        if (x$test) {
            msgs <- catch_all_msgs (f, x$fn, params_v)
            ret <- add_msg_output (ret, msgs, types = "warning",
                                   operation = "normal function call")
        }

        if (typeof (params_v [[v]]) == "integer" &
            !is.factor (params_v [[v]])) {
            ret <- rbind (ret, int_as_double (x, vec = TRUE))
        }

        ret <- rbind (ret, vector_class_defs (x))

        ret <- rbind (ret, vector_as_list (x))
    }

    return (ret)
}

# class definitions for vector columns should be ignored
test_vec_class_defs <- function (x) {

    operation <- "Custom class definitions for vector input"
    res0 <- report_object (type = "dummy",
                           fn_name = x$fn,
                           parameter = names (x$params) [x$i],
                           parameter_type = "generic vector",
                           operation = operation,
                           content = "(Should yield same result)")

    if (x$test) {

        if (!names (x$params) [x$i] %in% names (x$classes)) {
            p <- x$params [[x$i]]
            class (p) <- "different"
            x$params [[x$i]] <- p

            f <- tempfile (fileext = ".txt")
            msgs <- catch_all_msgs (f, x$fn, x$params)

            if (not_null_and_is (msgs, "error")) {
                index <- which (msgs$type == "error")
                res <- NULL
                res0$type <- "diagnostic"
                for (e in index) {
                    res0$content <- paste0 ("Function [",
                                            x$fn,
                                            "] errors on vector columns with ",
                                            "different classes when ",
                                            "submitted as ",
                                            names (x$params) [x$i],
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

test_vec_as_list <- function (x) {

    operation <- "Convert vector input to list-columns"
    res0 <- report_object (type = "dummy",
                           fn_name = x$fn,
                           parameter = names (x$params) [x$i],
                           parameter_type = "generic vector",
                           operation = operation,
                           content = "(Should yield same result)")

    if (x$test) {

        p <- x$params [[x$i]]
        p <- I (as.list (p))
        x$params [[x$i]] <- p

        f <- tempfile (fileext = ".txt")
        msgs <- catch_all_msgs (f, x$fn, x$params)

        if (not_null_and_is (msgs, "error")) {
            index <- which (msgs$type == "error")
            res <- NULL
            res0$type <- "diagnostic"
            for (e in index) {
                res0$content <- paste0 ("Function [",
                                        x$fn,
                                        "] errors on list-columns ",
                                        "when submitted as ",
                                        names (x$params) [x$i],
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
