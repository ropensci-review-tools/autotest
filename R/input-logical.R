
test_single_logical <- function (this_fn, params, i, test = TRUE) {

    res <- NULL

    res <- rbind (res, negate_logical (this_fn, params, i, test))

    res <- rbind (res, int_for_logical (this_fn, params, i, test))

    res <- rbind (res, char_for_logical (this_fn, params, i, test))

    return (res)
}

negate_logical <- function (this_fn, params, i, test = TRUE) {

    if (test) {

        res <- NULL
        f <- tempfile (fileext = ".txt")
        res <- rbind (res,
                      catch_all_msgs (f, this_fn, params))
        p <- params
        p [[i]] <- !p [[i]]
        res <- rbind (res,
                      catch_all_msgs (f, this_fn, p))

    } else {

        operation <- "negate default value of logical parameter"

        res <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [i],
                              parameter_type = "single logical",
                              operation = operation)


    }

    return (res)
}

int_for_logical <- function (this_fn, params, i, test = TRuE) {

    res <- subst_for_logical (this_fn, params, i, subst = "integer")

    if (test) {
        f <- tempfile (fileext = ".txt")
        # expect substituion by int values to give warnings or errors,
        # and return FALSE otherwise
        chk <- vapply (0L:2L, function (j) {
                           p <- params
                           p [[i]] <- j
                           msgs <- catch_all_msgs (f, this_fn, p)
                           val <- TRUE
                           if (is.null (msgs)) {
                               # no errors or warnings
                               val <- FALSE
                           } else if (!any (msgs$type %in% c ("warning", "error"))) {
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
        res$content <- NULL
    }

    return (res)
}

char_for_logical <- function (this_fn, params, i, test = TRUE) {

    res <- subst_for_logical (this_fn, params, i, subst = "character")

    if (test) {

        p [[i]] <- "a"
        msgs <- catch_all_msgs (f, this_fn, p)
        res$type <- "diagnostic"
        if (is.null (msgs)) {
            # function returns with char as logical, so keep res_tmp
        } else if (any (msgs$type %in% c ("warning", "error"))) {
            # warnings or errors generated, so do not return res_tmp
            res <- NULL
        }
    } else {
        res$content <- NULL
    }

    return (res)
}

#' Construt report object from results of subsituting other kinds of parameters
#' for assumed logical parameters
#' @noRd
subst_for_logical <- function (this_fn, params, i, subst = "integer") {

    operation <- paste0 ("substitute ",
                         subst,
                         "values for logical parameter")
    content <- paste0 ("Parameter ",
                       names (params) [i],
                       " of function [",
                       this_fn,
                       "] is assumed to be logical, ",
                       "but responds to ",
                       subst,
                       " input")

    return (report_object (type = "dummy",
                           fn_name = this_fn,
                           parameter = names (params) [i],
                           parameter_type = "single logical",
                           operation = operation,
                           content = content))
}
