
autotest_vector <- function (x = NULL, ...) {
    UseMethod ("autotest_vector", x)
}

autotest_vector.NULL <- function (x = NULL, ...) {

    env <- pkgload::ns_env ("autotest")
    all_names <- ls (env, all.names = TRUE)
    tests <- all_names [grep ("^test\\_", all_names)]
    tests <- tests [which (!grepl ("^.*\\.(default|NULL)$", tests))]

    tests <- grep ("^test\\_vec\\_", tests, value = TRUE)
    tests <- unique (gsub ("\\..*$", "", tests))

    res <- lapply (tests, function (i)
                   do.call (paste0 (i, ".NULL"), list (NULL)))

    return (do.call (rbind, res))
}

autotest_vector.autotest_obj <- function (x, test_data = NULL, ...) {

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
            ret <- rbind (ret, test_int_as_dbl (x,
                                                vec = TRUE,
                                                test_data = test_data))
        } else if (typeof (params_v [[v]]) == "double" &
                   !is.factor (params_v [[v]])) {
            ret <- rbind (ret, test_double_noise (x, test_data = test_data))
        }

        ret <- rbind (ret, test_vec_class_defs (x, test_data = test_data))

        ret <- rbind (ret, test_vec_as_list (x, test_data = test_data))
    }

    return (ret)
}

test_vec_class_defs <- function (x = NULL, ...) {
    UseMethod ("test_vec_class_defs", x)
}

test_vec_class_defs.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "vector_custom_class",
                   parameter_type = "vector",
                   operation = "Custom class definitions for vector input",
                   content = "(Should yield same result)")
}


test_vec_class_defs.autotest_obj <- function (x, test_data = NULL) { # nolint

    # only mutate classes of atomic-mode vectors, for which x$class = NULL
    if (!is.null (x$class))
        return (NULL)

    res0 <- test_vec_class_defs.NULL ()
    res0$fn_name <- x$fn
    res0$parameter <- names (x$params) [x$i]
    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, res0)
        if (length (test_flag) == 1L) {
            res0$test <- test_flag
        }
        if (!res0$test)
            res0$type <- "no_test"
        x$test <- res0$test
    }

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

test_vec_as_list <- function (x = NULL, ...) {
    UseMethod ("test_vec_as_list", x)
}

test_vec_as_list.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "vector_to_list_col",
                   parameter_type = "vector",
                   operation = "Convert vector input to list-columns",
                   content = "(Should yield same result)")
}

test_vec_as_list.autotest_obj <- function (x, test_data = NULL) {

    res0 <- test_vec_as_list.NULL ()
    res0$fn_name <- x$fn
    res0$parameter <- names (x$params) [x$i]
    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, res0)
        if (length (test_flag) == 1L) {
            res0$test <- test_flag
        }
        if (!res0$test)
            res0$type <- "no_test"
        x$test <- res0$test
    }

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
                # Need to allow here for custom error messages preventing
                # list-columns, which means standard message must be caught.
                # These are currently:
                # 1. Attempts to apply base-R binary operators like `+`, `*`
                # 2. Other procedures which error on "is.atomic(x) is not TRUE"
                generic_msgs <- "binary operator|is.atomic"
                if (grepl (generic_msgs, msgs$content [e])) {
                    res0$content <- paste0 ("Function [",
                                            x$fn,
                                            "] errors on list-columns ",
                                            "when submitted as ",
                                            names (x$params) [x$i],
                                            " Error message: ",
                                            msgs$content [e])
                    res <- rbind (res, res0)
                }
            }
        } else {
            res <- NULL
        }
    } else {
        res <- res0
    }

    return (res)
}
