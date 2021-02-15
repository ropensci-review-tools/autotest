
# testthat expection
# https://testthat.r-lib.org/articles/custom-expectation.html
expect_autotest <- function (object, n) {

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
