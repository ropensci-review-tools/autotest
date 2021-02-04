
test_negate_logical <- function (x = NULL, ...) {
    UseMethod ("test_negate_logical", x)
}

test_negate_logical.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "negate_logical",
                   parameter_type = "single logical",
                   operation = "Negate default value of logical parameter",
                   content = "(Function call should still work)")
}

test_negate_logical.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- test_negate_logical.NULL ()

    if (!is.null (test_data)) {
        x$test <- test_data$test [test_data$test_name == res$test_name]
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {

        f <- tempfile (fileext = ".txt")
        m <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (m)) {
            res$type <- m$type
            res$fn_name <- x$fn
            res$parameter <- names (x$params) [x$i]
            res$content <- m$content
        } else {
            res <- NULL
        }

        x$params [[x$i]] <- !x$params [[x$i]]
        m <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (m)) {
            # m may have multiple rows, so copy fields from NULL template to m.
            res_n <- test_negate_logical.NULL ()
            m$test_name <- res_n$test_name
            m$fn_name <- x$fn
            m$parameter <- names (x$params) [x$i]
            m$parameter_type <- res_n$parameter_type
            m$operation <- res_n$operation
            res <- rbind (res, m)
        }

    } else {

        res$fn_name <- x$fn
        res$parameter <- names (x$params) [x$i]

    }

    return (res)
}

test_int_for_logical <- function (x = NULL, ...) {
    UseMethod ("test_int_for_logical", x)
}

test_int_for_logical.NULL <- function (x = NULL, ...) {

    res <- subst_for_logical (subst = "integer")
    res$content <- paste0 ("(Function call should still work ",
                           "unless explicitly prevented)")

    return (res)
}

test_int_for_logical.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- test_int_for_logical.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    if (!is.null (test_data)) {
        x$test <- test_data$test [test_data$test_name == res$test_name]
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {
        f <- tempfile (fileext = ".txt")
        # expect substituion by int values to give warnings or errors,
        # and return FALSE otherwise
        chk <- vapply (0L:2L, function (j) {
                           p <- x$params
                           p [[x$i]] <- j
                           msgs <- catch_all_msgs (f, x$fn, p)
                           val <- TRUE
                           if (is.null (msgs)) {
                               # no errors or warnings
                               val <- FALSE
                           } else if (!any (msgs$type %in%
                                            c ("warning", "error"))) {
                               val <- FALSE
                           }
                           return (val) },
                           logical (1))
        # all `chk` should be TRUE if substituing `int` for `logical` leads to
        # errors/warnings
        if (!any (chk))
            res <- NULL
        else
            res$type <- "diagnostic"
    }

    return (res)
}

test_char_for_logical <- function (x = NULL, ...) {
    UseMethod ("test_char_for_logical", x)
}

test_char_for_logical.NULL <- function (x = NULL, ...) {

    res <- subst_for_logical (subst = "character")
    res$content <- "should trigger warning or error"

    return (res)
}

test_char_for_logical.autotest_obj <- function (x = NULL, test_data = NULL, ...) { # nolint

    res <- test_char_for_logical.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    if (!is.null (test_data)) {
        x$test <- test_data$test [test_data$test_name == res$test_name]
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {

        f <- tempfile (fileext = ".txt")
        x$params [[x$i]] <- "a"
        msgs <- catch_all_msgs (f, x$fn, x$params)
        res$type <- "diagnostic"
        if (is.null (msgs)) {
            # function returns with char as logical, so keep res_tmp
        } else if (any (msgs$type %in% c ("warning", "error"))) {
            # warnings or errors generated, so do not return res_tmp
            res <- NULL
        }
    }

    return (res)
}

#' Construt report object from results of subsituting other kinds of parameters
#' for assumed logical parameters
#' @noRd
subst_for_logical <- function (x = NULL, subst = "integer") {

    subst <- match.arg (subst, c ("integer", "character"))

    operation <- paste0 ("Substitute ",
                         subst,
                         " values for logical parameter")
    fn_name <- parameter <- content <- NA_character_
    if (!is.null (x)) {
        fn_name <- x$fn
        parameter <- names (x$params) [x$i]
        content <- paste0 ("Parameter ",
                           names (x$params) [x$i],
                           " of function [",
                           x$fn,
                           "] is assumed to be logical, ",
                           "but responds to ",
                           subst,
                           " input")
    }

    subst <- switch (EXPR = subst,
                     "integer" = "int",
                     "character" = "char")
    test_name <- paste0 ("subst_", subst, "_for_logical")

    report_object (type = "dummy",
                   test_name = test_name,
                   fn_name = fn_name,
                   parameter = parameter,
                   parameter_type = "single logical",
                   operation = operation,
                   content = content)
}
