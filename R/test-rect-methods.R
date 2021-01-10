# S3 dispatch methods for autotest_rectangular class objects.


autotest_rectangular.NULL <- function (package = ".") { # nolint

    env <- pkgload::ns_env ("autotest")
    all_names <- ls (env, all.names = TRUE)
    tests <- all_names [grep ("^test\\_", all_names)]
    tests <- tests [which (!grepl ("^.*\\.(default|rect_test|NULL)$", tests))]

    tests <- grep ("^test\\_rect\\_", tests, value = TRUE)

    res <- lapply (tests, function (i)
                   do.call (paste0 (i, ".NULL"), list (NULL)))

    return (do.call (rbind, res))
}

test_rect_as_other <- function (x = NULL, ...) {
    UseMethod ("test_rect_as_other", x)
}

test_rect_as_other.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   operation = "Convert one rectangular class to another",
                   content = "check for error/warning messages")
}


test_rect_as_other.autotest_obj <- function (x, ...) { # nolint

    if (x$test)
        ret <- pass_rect_as_other (x$fn, x$params, x$class, x$i, x$env)
    else
        ret <- dummy_rect_as_other (x$fn, x$params, x$class, x$i)

    return (ret)
}

test_rect_compare_outputs <- function (x = NULL) {
    UseMethod ("test_rect_compare_outputs", x)
}

test_rect_compare_outputs.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
               operation = "Convert one rectangular class to another",
               content = c ("expect dimensions are same ",
                            "expect column names are retained ",
                            "expect all columns retain identical structure "))
}

test_rect_compare_outputs.autotest_obj <- function (x) { # nolint

    if (x$test)
        ret <- compare_rect_outputs (x$fn, x$params, x$i, x$env)
    else
        ret <- dummy_compare_rect_outputs (x$fn, x$params, x$class, x$i)

    return (ret)
}

#' Extend class structure of tabular objects, which should still work
#' @noRd
test_rect_extend_class <- function (x = NULL, ...) {
    UseMethod ("test_rect_extend_class", x)
}

test_rect_extend_class.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   operation = "Extend existent class with new class",
                   content = "(Should yield same result)")
}

test_rect_extend_class.autotest_obj <- function (x, ...) { # nolint

    if (x$test)
        res <- do_extend_rect_class_struct (x$params, x$fn, x$i, x$env)
    else
        res <- dummy_extend_rect_class (x$params, x$fn, x$i)

    return (res)
}

#' Replacing class structure of tabular objects entirely should generally fail
#' @noRd
test_rect_replace_class <- function (x = NULL, ...) {
    UseMethod ("test_rect_replace_class", x)
}

test_rect_replace_class.NULL <- function (x = NULL, ...) { # nolint
    report_object (type = "dummy",
                   operation = "Replace class with new class",
                   content = "(Should yield same result)")
}

test_rect_replace_class.autotest_obj <- function (x, ...) { # nolint

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
