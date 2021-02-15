

#' expect_autotest_no_testdata
#'
#' Expect `autotest_package()` to be clear of errors with no tests switched off
#'
#' @param object Not used here, but required for `testthat` expectations
#' @return (invisibly) The autotest object
#' @export
expect_autotest_no_testdata <- function (object = NULL) {

    x <- autotest_package (here::here (),
                           test = TRUE)

    chk <- !any (c ("warning", "error") %in% x$type)

    testthat::expect (
                      chk,
                      sprintf ("`autotest` generates warnings or errors")
                      )

    invisible (x)
}


#' expect_autotest_testdata
#'
#' Expect `autotest_package()` to be clear of errors with some tests switched
#' off, and to have note column explaining why those tests are not run.
#'
#' @param object An `autotest_package` object with a `test` column flagging
#' tests which are not to be run on the local package.
#' @return (invisibly) The autotest object
#' @export
expect_autotest_testdata <- function (object) {

    x <- autotest_package (here::here (),
                           test = TRUE,
                           test_data = object)

    chk <- !any (c ("warning", "error") %in% x$type)

    testthat::expect (
                      chk,
                      sprintf ("`autotest` generates warnings or errors")
                      )

    # Then check that test_data has `note` column with values for all tests
    # switched off:
    chk <- TRUE
    index <- which (x$type == "no_test")
    if (length (index) > 0) {
        i <- grep ("^note", names (object), ignore.case = TRUE)
        if (length (i) == 1) {
            # non-dplyr merge object$note into x
            xref <- paste0 (x$test_name, x$fn_name, x$parameter)
            tref <- paste0 (object$test_name, object$fn_name, object$parameter)
            x <- x [which (xref %in% tref), ]
            xref <- paste0 (x$test_name, x$fn_name, x$parameter)
            x$note <- object [[i]] [match (xref, tref)]

            index <- which (x$type == "no_test")
            if (length (index) > 0)
                chk <- all (nchar (x$note [index]) > 0)
        }
    }
    msg <- paste0 ("Any autotest tests which are switched off should ",
                   "have explanatory notes in a 'note' column")
    testthat::expect (
                      chk,
                      sprintf (msg)
                      )

    invisible (x)
}



#' expect_autotest_no_err
#'
#' Expect `autotest_package()` to be clear of errors
#'
#' @param object An `autotest` object to be tested
#' @return (invisibly) The same object
#' @export
expect_autotest_no_err <- function (object) {

    act <- testthat::quasi_label (rlang::enquo (object),
                                  arg = "object")

    chk <- !any ("error" %in% act$val$type)

    testthat::expect (
                      chk,
                      sprintf ("`autotest` generates errors")
                      )

    invisible(act$val)
}

#' expect_autotest_no_warn
#'
#' Expect `autotest_package()` to be clear of warnings
#'
#' @param object An `autotest` object to be tested
#' @return (invisibly) The same object
#' @export
expect_autotest_no_warn <- function (object) {

    act <- testthat::quasi_label (rlang::enquo (object),
                                  arg = "object")

    chk <- !any ("warning" %in% act$val$type)

    testthat::expect (
                      chk,
                      sprintf ("`autotest` generates warnings")
                      )

    invisible(act$val)
}

#' expect_autotest_notes
#'
#' Expect `test_data` param of `autotest_package` to have additional `note`
#' column explaining why tests have been switched off.
#'
#' @param object An `autotest` object to be tested
#' @export
expect_autotest_notes <- function (object) {

    act <- testthat::quasi_label (rlang::enquo (object),
                                  arg = "object")

    chk <- TRUE
    index <- which (act$val$type == "no_test")
    if (length (index) > 0) {
        i <- grep ("^note", names (act$val), ignore.case = TRUE)
        chk <- FALSE
        if (length (i) == 1) {
            notes <- act$val [[i]]
            chk <- all (nchar (notes [index]) > 0)
        }
    }
    msg <- paste0 ("Any autotest tests which are switched off should ",
                   "have explanatory notes in a 'note' column")
    testthat::expect (
                      chk,
                      sprintf (msg)
                      )

    invisible(act$val)
}
