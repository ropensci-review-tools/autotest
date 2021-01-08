
test_single_logical <- function (x) {

    res <- NULL

    res <- rbind (res, negate_logical (x))

    res <- rbind (res, int_for_logical (x))

    res <- rbind (res, char_for_logical (x))

    return (res)
}

negate_logical <- function (x) {

    if (x$test) {

        res <- NULL
        f <- tempfile (fileext = ".txt")
        res <- rbind (res,
                      catch_all_msgs (f, x$fn, x$params))
        x$params [[x$i]] <- !x$params [[x$i]]
        res <- rbind (res,
                      catch_all_msgs (f, x$fn, x$params))

    } else {

        operation <- "Negate default value of logical parameter"

        res <- report_object (type = "dummy",
                              fn_name = x$fn,
                              parameter = names (x$params) [x$i],
                              parameter_type = "single logical",
                              operation = operation,
                              content = "(Function call should still work)")


    }

    return (res)
}

int_for_logical <- function (x) {

    res <- subst_for_logical (x, subst = "integer")

    if (x$test) {
        f <- tempfile (fileext = ".txt")
        # expect substituion by int values to give warnings or errors,
        # and return FALSE otherwise
        chk <- vapply (0L:2L, function (j) {
                           p <- x$params
                           p [[x$i]] <- j
                           msgs <- catch_all_msgs (f, x$fn, p)
                           val <- TRUE
                           if (is.null (msgs)) {
                               # no errors or warnings
                               val <- FALSE
                           } else if (!any (msgs$type %in%
                                            c ("warning", "error"))) {
                               val <- FALSE
                           }
                           return (val) },
                           logical (1))
        # all `chk` should be TRUE if substituing `int` for `logical` leads to
        # errors/warnings
        if (!any (chk))
            res <- NULL
        else
            res$type <- "diagnostic"
    } else {
        res$content <- paste0 ("(Function call should still work ",
                               "unless explicitly prevented)")
    }

    return (res)
}

char_for_logical <- function (x) {

    res <- subst_for_logical (x, subst = "character")

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
    } else {
        res$content <- "Should trigger warning or error"
    }

    return (res)
}

#' Construt report object from results of subsituting other kinds of parameters
#' for assumed logical parameters
#' @noRd
subst_for_logical <- function (x, subst = "integer") {

    operation <- paste0 ("Substitute ",
                         subst,
                         " values for logical parameter")
    content <- paste0 ("Parameter ",
                       names (x$params) [x$i],
                       " of function [",
                       x$fn,
                       "] is assumed to be logical, ",
                       "but responds to ",
                       subst,
                       " input")

    return (report_object (type = "dummy",
                           fn_name = x$fn,
                           parameter = names (x$params) [x$i],
                           parameter_type = "single logical",
                           operation = operation,
                           content = content))
}
