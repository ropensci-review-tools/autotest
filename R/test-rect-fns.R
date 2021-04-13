
#  ----------------------------------------------------------------------
# NOTE that these tests primarily rely on the S3 methods dispatched on
# `autotest-obj` class objects and defined in `R/test-rect-methods.R`. The
# remaining funtions in this file are auxilliary functions used within those
# methods.
#  ----------------------------------------------------------------------

chk_dims <- function (x, res1, res2) {

    ret <- NULL

    if (!identical (dim (res1), dim (res2))) {

        ret <- test_rect_compare_outputs.NULL ()
        ret <- ret [grep ("compare_dims", ret$test_name), ]
        ret$type <- "diagnostic"
        ret$fn_name <- x$fn
        ret$parameter <- names (x$params) [x$i]
        ret$operation <- paste0 ("compare output dimensions for ",
                                 "different rectangular inputs")
        ret$content <- paste0 ("Function [",
                               x$fn,
                               "] errors on rectangular input for [",
                               names (x$params) [x$i],
                               "]: Dimensions differ between ",
                               class (res1) [1],
                               " and ",
                               class (res2) [1], " inputs")
    }
    return (ret)
}

chk_names <- function (x, res1, res2) {

    ret <- NULL

    if (!identical (names (res1), names (res2))) {

        ret <- test_rect_compare_outputs.NULL ()
        ret <- ret [grep ("compare_col_names", ret$test_name), ]
        ret$type <- "diagnostic"
        ret$fn_name <- x$fn
        ret$parameter <- names (x$params) [x$i]
        ret$operation <- "compare output names for different rectangular inputs"
        ret$content <- paste0 ("Function [",
                               x$fn,
                               "] errors on rectangular input for [",
                               names (x$params) [x$i],
                               "]: Column names differ between ",
                               class (res1) [1],
                               " and ",
                               class (res2) [1],
                               " inputs")
    }
    return (ret)
}

chk_columns <- function (x, res1, res2) {

    ret <- NULL

    cnames <- colnames (res1) [which (colnames (res1) %in% colnames (res2))]

    for (i in seq_along (cnames)) {

        if (!identical (res1 [[i]], res2 [[i]])) {

            ro <- test_rect_compare_outputs.NULL ()
            ro <- ro [grep ("compare_col_structure", ro$test_name), ]
            ro$type <- "diagnostic"
            ro$fn_name <- x$fn
            ro$parameter <- names (x$params) [x$i]
            ro$operation <- paste0 ("compare output columns for ",
                                    "different rectangular inputs")
            ro$content <- paste0 ("Function [",
                               x$fn,
                               "] errors on rectangular input for [",
                               names (x$params) [x$i],
                               "]: Column [",
                               names (res1) [i],
                               "] differs between ",
                               class (res1) [1],
                               " and ",
                               class (res2) [1],
                               " inputs")
            ret <- rbind (ret, ro)
        }
    }
    return (ret)
}

#' call fn with params if previous report was either empty or not an error
#'
#' @noRd
docall <- function (ret, fn, params) {
    docall <- FALSE
    if (is.null (ret))
        docall <- TRUE
    else if (!"error" %in% ret$type)
        docall <- TRUE

    return (docall)
}

#' Get classes of generic rectangular objects except those explicitly restricted
#' by class definitions/descriptions.
#' @noRd
other_rect_classes <- function (classes = NULL, this_class = NULL) {

    other <- c ("data.frame", "tibble::tibble", "data.table::data.table")
    if (!is.null (this_class)) {
        rm_this <- match (this_class [1],
                          c ("data.frame", "tbl_df", "data.table"))
        if (!is.na (rm_this))
            other <- other [-rm_this]
    }

    if (length (classes) > 0) {
        other <- other [which (!gsub (".*::", "", other) %in% classes)]
    }

    return (other)
}

#' @param x An `autotest_obj` object
#' @noRd
dummy_rect_as_other <- function (x, test_data = NULL) {

    par_type <- class (x$params [[x$i]]) [1]
    other <- other_rect_classes (x$class, par_type)
    other <- gsub ("^.*::", "", other)

    template <- test_rect_as_other.NULL ()
    res <- report_object (type = "dummy",
                          test_name = template$test_name,
                          fn_name = x$fn,
                          parameter = names (x$params) [x$i],
                          parameter_type = par_type,
                          operation = paste0 ("Convert [",
                                              par_type,
                                              "] to [",
                                              other,
                                              "]"),
                          content = template$content)

    if (!is.null (test_data)) {
        if (template$content %in% test_data$content) {
            res$test <- test_data$test [test_data$content == template$content]
        }
    }

    return (res)
}

dummy_compare_rect_outputs <- function (x, test_data) {

    par_type <- class (x$params [[x$i]]) [1]
    other <- other_rect_classes (x$class, par_type)
    other <- gsub ("^.*::", "", other)

    template <- test_rect_compare_outputs.NULL ()
    # that template has 3 rows for 3 different contents
    operations <- paste0 ("Convert [",
                          par_type,
                          "] to [",
                          other,
                          "]")
    content <- rep (template$content, each = length (other))
    operations <- rep (operations, times = 3)
    test_names <- rep (template$test_name, each = length (other))

    res <- report_object (type = "dummy",
                          test_name = test_names,
                          fn_name = x$fn,
                          parameter = names (x$params) [x$i],
                          parameter_type = par_type,
                          operation = operations,
                          content = content)

    if (!is.null (test_data)) {
        for (i in unique (res$content)) {
            index <- which (res$content == i)
            res$test [index] <- test_data$test [match (i, test_data$content)]
            res$type [index] [which (!res$test [index])] <- "no_test"
        }
    }

    return (res)
}

#' Change class of params [[i]] to other rectangular classes and capture
#' resultant return values in `this_env`
#' @noRd
pass_rect_as_other <- function (x, test_data = NULL) {

    res <- NULL

    if (!is.null (test_data)) {
        template <- test_rect_as_other.NULL ()
        index <- which (test_data$operation == template$operation &
                        test_data$content == template$content)
        if (length (index) > 0) {
            test_this <- test_data$test [index]
            if (!any (test_this))
                return (test_data [index, ])
        }
    }

    other <- other_rect_classes (x$class, class (x$params [[x$i]]))

    for (o in seq_along (other)) {
        this_ret <- pass_one_rect_as_other (x, other [o])
        res <- rbind (res, this_ret)
        if (docall (this_ret, x$fn, x$params)) {

            # much easier to call the conversions directly here, especially
            # because `data.table` requires a `data.table` aware namespace to
            # work via `do.call`.
            if (other [o] == "data.frame")
                x$params [[x$i]] <- as.data.frame (x$params [[x$i]])
            else if (other [o] == "tibble::tibble")
                x$params [[x$i]] <- tibble::as_tibble (x$params [[x$i]])
            else if (other [o] == "data.table::data.table")
                x$params [[x$i]] <- data.table::as.data.table (x$params [[x$i]])

            junk <- utils::capture.output (
                val <- suppressWarnings (
                            suppressMessages (
                                do.call (x$fn,
                                         x$params,
                                         envir = x$env,
                                         quote = TRUE)
                                ))
            )
            nm <- paste0 ("val-", gsub ("^.*::", "", other [o]))
            assign (nm, val, envir = x$env)
        }
    }

    if (!is.null (res)) {
        par_type <- class (x$params [[x$i]]) [1]
        res$parameter_type <- par_type
        res$operation <- paste0 ("check error/warning on ",
                                 par_type,
                                 " as ",
                                 other)
    }


    return (res)
}

#' The mechanism for a single conversion of `pass_rect_as_other`, converting to
#' specified `other` class, evaluating the function call, and returning the
#' standard `return_object` output containing any messages/warnings/errors
#' issued.
#' @noRd
pass_one_rect_as_other <- function (x,
                                    other = "data.frame",
                                    test_data = NULL) {

    ret <- NULL

    x$params [[x$i]] <- do.call (eval (parse (text = other)),
                                 x$params [[x$i]],
                                 quote = TRUE)
    f <- tempfile ()
    msgs <- catch_all_msgs (f, x$fn, x$params)

    if (!is.null (msgs)) {
        msgs$parameter <- rep (names (x$params) [x$i], nrow (msgs))

        if (grepl ("::", other))
            other <- strsplit (other, "::") [[1]] [2]
        ret <- add_msg_output (NULL,
                               msgs,
                               types = c ("warning", "error"),
                               operation = paste0 ("tabular as ", other))

        template <- test_rect_as_other.NULL ()
        ret$test_name <- template$test_name
    }

    return (ret)
}

#' Return a grid of all pairwise comparisons of classes for rectangular objects,
#' optionally with a specified target class, `this_class`.
#' @noRd
get_rect_comparisons <- function (nms, this_env = NULL, this_class = NULL) {

    # when nms are passed as objects from environment list, they only exist if
    # those classes do not error, so `nms` may be empty.
    ret_now <- length (nms) == 0

    envobjs <- NULL
    if (!is.null (this_env))
        envobjs <- ls (envir = this_env)

    if (is.null (this_class)) {
        if (length (nms) < 2)
            ret_now <- TRUE
    } else if (!this_class [1] %in% envobjs) {
            ret_now <- TRUE
    }
    if (ret_now)
        return (NULL)

    if (is.null (this_class)) {
        index <- t (utils::combn (length (nms), 2))
        nms <- matrix (nms [index], ncol = 2)
    } else {
        nms <- cbind (this_class, nms)
    }

    return (nms)
}

compare_rect_outputs <- function (x, this_obj = NULL) {


    nms <- c ("val-data.frame", "val-tibble", "val-data.table")
    nms <- nms [which (nms %in% ls (envir = x$env))]

    nms <- get_rect_comparisons (nms, x$env, this_obj)
    if (is.null (nms))
        return (NULL)

    res <- NULL
    for (i in seq (nrow (nms))) {
        res1 <- get (nms [i, 1], envir = x$env)
        res2 <- get (nms [i, 2], envir = x$env)

        res <- rbind (res, chk_dims (x, res1, res2))
        res <- rbind (res, chk_names (x, res1, res2))
        res <- rbind (res, chk_columns (x, res1, res2))
    }

    return (res)
}

dummy_extend_rect_class <- function (x) {

    ret <- test_rect_extend_class.NULL ()

    ret$fn_name <- x$fn
    ret$parameter <- names (x$params) [x$i]
    ret$parameter_type <- class (x$params [[x$i]]) [1]
    ret$operation <- paste0 ("Extend existent class [",
                             class (x$params [[x$i]]) [1],
                             "] with new class")

    return (ret)
}

do_extend_rect_class_struct <- function (x) {

    ret <- NULL

    p_i <- x$params [[x$i]]

    x$params [[x$i]] <- structure (p_i, class = c ("newclass", class (p_i)))

    msgs <- catch_all_msgs (x$f, x$fn, x$params)
    if (!is.null (msgs)) {
        msgs$parameter <- rep (names (x$params) [x$i], nrow (msgs))
        msgs$parameter_type <- "general tabular"

        msg_out <- add_msg_output (NULL,
                                   msgs,
                                   types = c ("warning", "error"))

        ret <- test_rect_extend_class.NULL ()
        ret$type <- msg_out$type
        ret$fn_name <- msg_out$fn_name
        ret$parameter <- msg_out$parameter
        ret$parameter_type <- msg_out$parameter_type
        ret$content <- msg_out$content
    }

    if (!"error" %in% msgs$type) {
        o <- utils::capture.output (
                temp <- suppressWarnings (do.call (x$fn,
                                                   x$params,
                                                   envir = x$env,
                                                   quote = TRUE))
        )
        assign ("val-newclass", temp, envir = x$env)

        ret <- rbind (ret,
                      compare_rect_outputs (x, this_obj = "val-newclass"))
    }

    return (ret)
}
