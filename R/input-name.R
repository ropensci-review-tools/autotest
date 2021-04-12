
test_single_name <- function (x = NULL, ...) {
    UseMethod ("test_single_name", x)
}

test_single_name.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "name_or_formula_as_char",
                   parameter_type = "single name or formula",
                   operation = "Unquoted name/formula as quoted character",
                   content = "Capture any warnings or errors issued")
}

test_single_name.autotest_obj <- function (x, test_data = NULL) {

    ret <- test_single_name.NULL ()

    ret$fn_name <- x$fn
    ret$parameter <- names (x$params) [x$i]

    ret$operation <- paste0 ("(unquoted) ",
                             class (x$params [[x$i]]) [1],
                             " param as (quoted) character")

    if (!is.null (test_data)) {
        x$test <- test_these_data (test_data, ret)
        if (!x$test)
            ret$type <- "no_test"
    }

    if (x$test) {

        f <- tempfile (fileext = ".txt")

        x$params [[x$i]] <- as.character (x$params [[x$i]])
        msgs <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (msgs)) {
            ret$type <- msgs$type
            ret$content <- msgs$content
        } else {
            ret <- NULL
        }
    }

    return (ret)
}
