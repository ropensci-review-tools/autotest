# S3 dispatch methods for autotest_rectangular class objects.

autotest_rectangular <- function (x = NULL, test_data = NULL, ...) {
    UseMethod ("autotest_rectangular", x)
}

autotest_rectangular.NULL <- function (x = NULL, ...) { # nolint

    env <- pkgload::ns_env ("autotest")
    all_names <- ls (env, all.names = TRUE)
    tests <- all_names [grep ("^test\\_", all_names)]
    tests <- tests [which (!grepl ("^.*\\.(default|NULL)$", tests))]

    tests <- grep ("^test\\_rect\\_", tests, value = TRUE)
    tests <- unique (gsub ("\\..*$", "", tests))

    res <- lapply (tests, function (i)
                   do.call (paste0 (i, ".NULL"), list (NULL)))

    return (do.call (rbind, res))
}

autotest_rectangular.autotest_obj <- function (x, test_data, ...) { # nolint

    ret <- NULL

    x$classes <- x$classes [which (!is.na (x$classes))]

    rect_index <- which (x$param_types == "tabular")

    for (r in rect_index) {

        x$i <- r

        x$class <- NULL
        if (names (x$params) [r] %in% names (x$classes))
            x$class <- x$classes [[match (names (x$params) [r],
                                          names (x$classes))]]

        ret <- rbind (ret, test_rect_as_other (x, test_data))

        ret <- rbind (ret, test_rect_compare_outputs (x, test_data))

        # Modify class definitions for rectangular inputs if not excluded by
        # yaml class definitions
        if (!names (x$params) [r] %in% names (x$classes)) {
            ret <- rbind (ret, test_rect_extend_class (x, test_data))

            ret <- rbind (ret, test_rect_replace_class (x, test_data))
        }
    }
    return (ret)
}


test_rect_as_other <- function (x = NULL, test_data = NULL, ...) {
    UseMethod ("test_rect_as_other", x)
}

test_rect_as_other.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "rect_as_other",
                   parameter_type = "rectangular",
                   operation = "Convert one rectangular class to another",
                   content = "check for error/warning messages")
}


test_rect_as_other.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    ret <- NULL

    if (!is.null (test_data)) {
        these_tests <- test_data [test_data$operation ==
                                  "Convert one rectangular class to another", ]
        ret <- these_tests [which (!these_tests$test), ]
        these_tests <- these_tests [which (these_tests$test), ]

        if (nrow (these_tests) > 0) {
            if (x$test) {
                this_ret <- pass_rect_as_other (x$fn,
                                                x$params,
                                                x$class,
                                                x$i,
                                                x$env,
                                                these_tests)
            } else {
                this_ret <- dummy_rect_as_other (x$fn,
                                                 x$params,
                                                 x$class,
                                                 x$i,
                                                 these_tests)
            }
            ret <- rbind (ret, this_ret)
        }
    }

    return (ret)
}

test_rect_compare_outputs <- function (x = NULL, test_data = NULL, ...) {
    UseMethod ("test_rect_compare_outputs", x)
}

test_rect_compare_outputs.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
            test_name = c ("rect_compare_dims",
                           "rect_compare_col_names",
                           "rect_compare_col_structure"),
            parameter_type = "rectangular",
            operation = "Convert one rectangular class to another",
            content = c ("expect dimensions are same ",
                         "expect column names are retained ",
                         "expect all columns retain identical structure "))
}

test_rect_compare_outputs.autotest_obj <- function (x, test_data = NULL) { # nolint

    if (x$test)
        ret <- compare_rect_outputs (x$fn, x$params, x$i, x$env, test_data)
    else
        ret <- dummy_compare_rect_outputs (x$fn, x$params, x$class, x$i,
                                           test_data)

    return (ret)
}

#' Extend class structure of tabular objects, which should still work
#' @noRd
test_rect_extend_class <- function (x = NULL, test_data = NULL, ...) {
    UseMethod ("test_rect_extend_class", x)
}

test_rect_extend_class.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   parameter_type = "rectangular",
                   operation = "Extend existent class with new class",
                   content = "(Should yield same result)")
}

test_rect_extend_class.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    if (x$test)
        res <- do_extend_rect_class_struct (x$params, x$fn, x$i, x$env)
    else
        res <- dummy_extend_rect_class (x$params, x$fn, x$i)

    return (res)
}

#' Replacing class structure of tabular objects entirely should generally fail
#' @noRd
test_rect_replace_class <- function (x = NULL, test_data = NULL, ...) {
    UseMethod ("test_rect_replace_class", x)
}

test_rect_replace_class.NULL <- function (x = NULL, ...) { # nolint
    report_object (type = "dummy",
                   parameter_type = "rectangular",
                   operation = "Replace class with new class",
                   content = "(Should yield same result)")
}

test_rect_replace_class.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    this_class <- class (x$params [[x$i]]) [1]
    operation <- paste0 ("Replace class [", this_class, "] with new class")
    ret <- report_object (type = "dummy",
                          fn_name = x$fn,
                          parameter = names (x$params) [x$i],
                          parameter_type = this_class,
                          operation = operation,
                          content = "(Should yield same result)")

    if (x$test) {

        p <- x$params [[x$i]]
        x$params [[x$i]] <- structure (p, class = c ("newclass"))
        f <- tempfile (fileext = ".txt")
        msgs <- catch_all_msgs (f, x$fn, x$params)

        if (!null_or_not (msgs, "error"))
            ret <- NULL
        else {
            msgs$parameter <- rep (names (x$params) [x$i], nrow (msgs))
            ret$type <- "diagnostic"
            ret$content <- paste0 ("Function [",
                               x$fn,
                               "] does not error when class structure of [",
                               this_class, "] is removed.")
        }
    }

    return (ret)

}
