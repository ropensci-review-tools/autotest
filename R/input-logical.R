
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
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, res)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {

        f <- tempfile (fileext = ".txt")
        m <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (m)) {
            # m may have multiple rows, so copy fields from NULL template to m.
            res_n <- test_negate_logical.NULL ()
            m$test_name <- res_n$test_name
            m$fn_name <- x$fn
            m$parameter <- names (x$params) [x$i]
            m$parameter_type <- res_n$parameter_type
            m$operation <- res_n$operation

            res <- m
        } else {
            res <- NULL
        }

        x$params [[x$i]] <- !x$params [[x$i]]
        m <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (m)) {
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
        test_flag <- test_these_data (test_data, res)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {
        f <- tempfile (fileext = ".txt")
        # Expect substituion by int values to give warnings or errors.
        # This returns TRUE is there are errors or warnings, otherwise it
        # returns FALSE.
        chk <- vapply (0L:2L, function (j) {
                           p <- x$params
                           p [[x$i]] <- j
                           msgs <- catch_all_msgs (f, x$fn, p)
                           val <- FALSE
                           if (!is.null (msgs)) {
                               if (any (msgs$type %in%
                                        c ("warning", "error"))) {
                                   val <- TRUE
                               }
                           }
                           return (val) },
                           logical (1))
        # all `chk` should be TRUE if substituing `int` for `logical` leads to
        # errors/warnings
        if (all (chk)) {
            res <- NULL
        } else {
            res$type <- "diagnostic"
        }
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
        test_flag <- test_these_data (test_data, res)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
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
                           "] is only used as a logical, ",
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
