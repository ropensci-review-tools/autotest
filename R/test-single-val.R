
autotest_single <- function (x = NULL, ...) {
    UseMethod ("autotest_single", x)
}

autotest_single.NULL <- function (x = NULL, ...) {

    env <- pkgload::ns_env ("autotest")
    all_names <- ls (env, all.names = TRUE)
    tests <- all_names [grep ("^test\\_", all_names)]
    tests <- tests [which (!grepl ("^.*\\.(default|NULL)$", tests))]

    tests <- grep ("^test\\_(single|double|int)\\_", tests, value = TRUE)
    tests <- unique (gsub ("\\..*$", "", tests))

    res <- lapply (tests, function (i)
                   do.call (paste0 (i, ".NULL"), list (NULL)))

    return (do.call (rbind, res))
}

autotest_single.autotest_obj <- function (x, ...) {

    if (any (x$params == "NULL")) {
        x$params <- x$params [x$params != "NULL"]
    }

    f <- tempfile (fileext = ".txt")
    res <- NULL
    if (x$test)
        res <- catch_all_msgs (f, x$fn, x)
    if (!is.null (res))
        res$operation <- "normal function call"

    index <- which (x$param_types == "single")
    for (i in index) {

        x$i <- i

        val_type <- single_val_type (x$params [[i]])
        check_vec <- TRUE

        if (val_type == "integer") {

            res <- rbind (res,
                          test_single_int_range (x),
                          test_int_as_dbl (x, vec = FALSE))

        } else if (val_type == "numeric") {

            res <- rbind (res, test_double_noise (x))

        } else if (val_type == "character") {

            res <- rbind (res, test_single_char (x))

        } else if (val_type == "logical") {

            res <- rbind (res, test_single_logical (x))

        } else if (val_type %in% c ("name", "formula")) {

            res <- rbind (res, test_single_name (x))
            if (val_type %in% c ("name", "formula"))
                check_vec <- FALSE
        }

        # check response to vector input:
        if (check_vec) {
            res <- rbind (res, test_single_length (x, val_type))
        }
    }

    return (res [which (!duplicated (res)), ])
}

single_val_type <- function (x) {

    res <- ""

    if (is_int (x))
        res <- "integer"
    else if (methods::is (x, "numeric"))
        res <- "numeric"
    else if (is.character (x))
        res <- "character"
    else if (is.logical (x))
        res <- "logical"
    else if ((methods::is (x, "name") | methods::is (x, "formula")))
        res <- class (x) [1]

    return (res)
}

#' Do input values presumed to have length one give errors when vectors of
#' length > 1 are passed?
#' @noRd
test_single_length <- function (x = NULL, ...) {
    UseMethod ("test_single_length", x)
}

test_single_length.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   parameter_type = "single integer",
                   operation = "Length 2 vector for length 1 parameter",
                   content = "Should trigger message, warning, or error")
}

test_single_length.autotest_obj <- function (x, val_type) {

    operation <- "Length 2 vector for length 1 parameter"
    res <- report_object (type = "dummy",
                          fn_name = x$fn,
                          parameter = names (x$params) [x$i],
                          parameter_type = paste0 ("single ", val_type),
                          operation = operation,
                          content = "Should trigger message, warning, or error")

    if (x$test) {

        x$params [[x$i]] <- rep (x$params [[x$i]], 2)
        f <- tempfile (fileext = ".txt")
        msgs <- catch_all_msgs (f, x$fn, x$params)

        if (not_null_and_is (msgs, c ("warning", "error")))
            res <- NULL # function call should warn or error
        else {
            res$type <- "diagnostic"
            res$content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] of function [",
                                   x$fn,
                                   "] is assumed to be a single ",
                                   val_type,
                                   ", but responds to vectors of length > 1")
        }
    }

    return (res)
}
