
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

    res <- test_single_name.NULL ()

    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    res$operation <- paste0 ("(unquoted) ",
                             class (x$params [[x$i]]) [1],
                             " param as (quoted) character")

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

        x$params [[x$i]] <- as.character (x$params [[x$i]])
        msgs <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (msgs)) {
            res$type <- msgs$type
            res$content <- msgs$content
        } else {
            res <- NULL
        }
    }

    return (res)
}
