
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
