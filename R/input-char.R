test_single_char_case_dep <- function (x = NULL, ...) {
    UseMethod ("test_single_char_case_dep", x)
}

#' @exportS3Method
test_single_char_case_dep.NULL <- function (x, ...) {

    report_object (
        type = "dummy",
        test_name = "single_char_case",
        parameter_type = "single character",
        operation = "Change case",
        content = "(Should yield same result)"
    )
}

#' @exportS3Method
test_single_char_case_dep.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- NULL


    if (!is.null (test_data)) {
        r <- test_single_char_case_dep.NULL ()
        test_flag <- test_these_data (test_data, r)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
    }

    for (lower in c (TRUE, FALSE)) {
        res <- rbind (res, case_dependency (x, lower = lower))
    }

    if (!is.null (test_data) & !x$test & !is.null (res)) {
        res$type <- "no_test"
    }

    return (res)
}

case_dependency <- function (x, lower = TRUE) {

    op <- paste0 (ifelse (lower, "lower", "upper"), "-case character parameter")
    res <- test_single_char_case_dep.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]
    res$operation <- op

    if (x$test) {

        x$params [[x$i]] <- ifelse (lower,
            tolower (x$params [[x$i]]),
            toupper (x$params [[x$i]])
        )

        f <- tempfile ()
        msgs <- catch_all_msgs (f, x$fn, x$params)
        if (is.null (msgs)) {
            res <- NULL
        } else {
            res$type <- "diagnostic"
            res$content <- "is case dependent"
        }
    }

    return (res)
}

test_single_char_as_random <- function (x = NULL, ...) {
    UseMethod ("test_single_char_as_random", x)
}

#' @exportS3Method
test_single_char_as_random.NULL <- function (x = NULL, ...) { # nolint

    report_object (
        type = "dummy",
        test_name = "random_char_string",
        parameter_type = "single character",
        operation = "random character string as parameter",
        content = "Should error"
    )
}

#' @exportS3Method
test_single_char_as_random.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- test_single_char_as_random.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    if (!is.null (test_data)) {
        r <- test_single_char_as_random.NULL ()
        test_flag <- test_these_data (test_data, r)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test) {
            res$type <- "no_test"
        }
    }

    x$params [[x$i]] <- paste0 (
        sample (c (letters, LETTERS),
            size = 10
        ),
        collapse = ""
    )

    if (char_param_is_arbitrary (x)) {
        x$test <- FALSE
        res <- NULL
    }

    if (x$test) {

        f <- tempfile ()
        msgs <- catch_all_msgs (f, x$fn, x$params)

        if (!"error" %in% msgs$type) {
            res$type <- "diagnostic"
            res$content <- "does not match arguments to expected values"
        } else {
            res <- NULL
        }
    }

    return (res)
}

#' Check whether character parameters are effectively arbitrary
#'
#' issue #65 from @helske. Arbitrary strings are not mutated to test whether
#' `match.arg()` is used.
#' @noRd
char_param_is_arbitrary <- function (x) {

    rd <- get_Rd_param (
        package = x$package_loc,
        fn_name = x$fn,
        param_name = names (x$params) [x$i]
    )

    ptn <- "name|label|text|string|arbitrary"
    any (grepl (ptn, rd))
}
