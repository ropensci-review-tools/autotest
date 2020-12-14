autotest_rectangular <- function (params,
                                  param_types,
                                  this_fn,
                                  classes,
                                  test,
                                  quiet) {

    ret <- NULL

    classes <- classes [which (!is.na (classes))]

    this_env <- new.env ()

    rect_index <- which (param_types == "tabular")
    for (r in rect_index) {
        params_r <- params

        if (test) {

            ret <- rbind (ret,
                          pass_rect_as_other (this_fn,
                                              params_r,
                                              classes,
                                              r,
                                              this_env))
            ret <- rbind (ret,
                          compare_rect_outputs (this_fn, params, r, this_env))

        } else {

            this_ret <- dummy_rect_as_other (this_fn, params_r, classes, r)
            par_type <- this_ret$parameter_type [1]

            types <- vapply (strsplit (this_ret$operation, " to \\["),
                             function (i) i [2],
                             character (1))
            operations <- paste0 ("Convert [",
                                  par_type,
                                  "] to [",
                                  rep (gsub ("\\]$", "", types), each = 3),
                                  "]")
            content <- c ("expect dimensions are same ",
                              "expect column names are retained ",
                              "expect all columns retain identical structure ")
            content <- rep (content, times = length (types))

            ret <- rbind (this_ret,
                          report_object (type = "dummy",
                                         fn_name = this_fn,
                                         parameter = names (params) [r],
                                         parameter_type = par_type,
                                         operation = operations,
                                         content = content))
        }



        # Modify class definitions for rectangular inputs if not excluded by
        # yaml class definitions
        if (!names (params_r) [r] %in% names (classes)) {
            ret <- rbind (ret,
                          extend_rect_class_strut (params_r,
                                                   this_fn,
                                                   r,
                                                   this_env,
                                                   test))

            ret <- rbind (ret,
                          replace_rect_class_struct (params_r,
                                                     this_fn,
                                                     r,
                                                     test))
        }
    }
    return (ret)
}

chk_dims <- function (this_fn, params, r, res1, res2) {
    ret <- NULL
    if (!identical (dim (res1), dim (res2))) {
        operation <- paste0 ("compare output dimensions for ",
                             "different rectangular inputs")
        content <- paste0 ("Function [",
                           this_fn,
                           "] errors on rectangular input for [",
                           names (params) [r],
                           "]: Dimensions differ between ",
                           class (res1) [1],
                           " and ",
                           class (res2) [1], " inputs")
        ret <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [r],
                              parameter_type = "generic tabular",
                              operation = operation,
                              content = content)
    }
    return (ret)
}

chk_names <- function (this_fn, params, r, res1, res2) {
    ret <- NULL
    if (!identical (names (res1), names (res2))) {
        operation <- "compare output names for different rectangular inputs"
        content <- paste0 ("Function [",
                           this_fn,
                           "] errors on rectangular input for [",
                           names (params) [r],
                           "]: Column names differ between ",
                           class (res1) [1],
                           " and ",
                           class (res2) [1],
                           " inputs")
        ret <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [r],
                              parameter_type = "generic tabular",
                              operation = operation,
                              content = content)
    }
    return (ret)
}

chk_columns <- function (this_fn, params, r, res1, res2) {
    ret <- NULL
    for (i in seq (ncol (res1))) {
        if (!identical (res1 [[i]], res2 [[i]])) {
            operation <- paste0 ("compare output columns for ",
                                 "different rectangular inputs")
            content <- paste0 ("Function [",
                               this_fn,
                               "] errors on rectangular input for [",
                               names (params) [r],
                               "]: Column [",
                               names (res1) [i],
                               "] differs between ",
                               class (res1) [1],
                               " and ",
                               class (res2) [1],
                               " inputs")
            ret <- rbind (ret,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [r],
                                         parameter_type = "generic tabular",
                                         operation = operation,
                                         content = content))
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
        other <- other [which (gsub (".*::", "", other) %in% names (classes))]
    }

    return (other)
}

dummy_rect_as_other <- function (fn, params, classes, i) {

    par_type <- class (params [[i]]) [1]
    other <- other_rect_classes (classes, par_type)
    other <- gsub ("^.*::", "", other)

    report_object (type = "dummy",
                   fn_name = fn,
                   parameter = names (params) [i],
                   parameter_type = par_type,
                   operation = paste0 ("Convert [",
                                       par_type,
                                       "] to [",
                                       other,
                                       "]"),
                   content = "check for error/warning messages")
}

#' Change class of params [[i]] to other rectangular classes and capture
#' resultant return values in `this_env`
#' @noRd
pass_rect_as_other <- function (fn, params, classes, i, this_env) {

    other <- other_rect_classes (classes, class (params [[i]]))

    res <- NULL

    for (o in seq_along (other)) {
        this_ret <- pass_one_rect_as_other (fn, params, i, other [o])
        res <- rbind (res, this_ret)
        if (docall (this_ret, fn, params)) {
            val <- suppressWarnings (do.call (fn, params, envir = this_env))
            nm <- paste0 ("val-", gsub ("^.*::", "", other [o]))
            assign (nm, val, envir = this_env)
        }
    }

    if (!is.null (res)) {
        par_type <- class (params [[i]]) [1]
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
pass_one_rect_as_other <- function (fn, params, i, other = "data.frame") {

    f <- tempfile (fileext = ".txt")
    ret <- NULL

    params [[i]] <- do.call (eval (parse (text = other)), params [[i]])
    msgs <- catch_all_msgs (f, fn, params)

    if (!is.null (msgs)) {
        msgs$parameter <- rep (names (params) [i], nrow (msgs))

        if (grepl ("::", other))
            other <- strsplit (other, "::") [[1]] [2]
        ret <- add_msg_output (NULL, msgs, types = c ("warning", "error"),
                               operation = paste0 ("tabular as ", other))
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
    } else if (!this_class %in% envobjs) {
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

compare_rect_outputs <- function (fn, params, i, this_env, this_obj = NULL) {


    nms <- c ("val-data.frame", "val-tibble", "val-data.table")
    nms <- nms [which (nms %in% ls (envir = this_env))]

    nms <- get_rect_comparisons (nms, this_env, this_obj)
    if (is.null (nms))
        return (NULL)

    res <- NULL
    for (i in seq (nrow (nms))) {
        res1 <- get (nms [i, 1], envir = this_env)
        res2 <- get (nms [i, 2], envir = this_env)

        res <- rbind (res, chk_dims (fn, params, i, res1, res2))
        res <- rbind (res, chk_names (fn, params, i, res1, res2))
        res <- rbind (res, chk_columns (fn, params, i, res1, res2))
    }

    return (res)
}

#' Extend class structure of tabular objects, which should still work
#' @noRd
extend_rect_class_strut <- function (params, this_fn, i, this_env, test) {

    if (test)
        res <- do_extend_rect_class_struct (params, this_fn, i, this_env)
    else
        res <- dummy_extend_rect_class (params, this_fn, i)

    return (res)
}

dummy_extend_rect_class <- function (params, this_fn, i) {

    par_type <- class (params [[i]]) [1]

    report_object (type = "dummy",
                   fn_name = this_fn,
                   parameter = names (params) [i],
                   parameter_type = par_type,
                   operation = paste0 ("Extend existent class [",
                                       par_type,
                                       "] with new class"),
                   content = "(Should yield same result)")
}

do_extend_rect_class_struct <- function (params, this_fn, i, this_env) {

    x <- params [[i]]

    params [[i]] <- structure (x, class = c ("newclass", class (x)))

    f <- tempfile (fileext = ".txt")
    msgs <- catch_all_msgs (f, this_fn, params)
    if (!is.null (msgs)) {
        msgs$parameter <- rep (names (params) [i], nrow (msgs))
        msgs$parameter_type <- "general tabular"
    }

    ret <- add_msg_output (NULL,
                           msgs,
                           types = c ("warning", "error"),
                           operation = paste0 ("rectangular parameter ",
                                               "with extended ",
                                               "class structure"))

    if (!"error" %in% msgs$type) {
        o <- utils::capture.output (
                temp <- suppressWarnings (do.call (this_fn,
                                                   params,
                                                   envir = this_env))
        )
        assign ("val-newclass", temp, envir = this_env)

        ret <- rbind (ret,
                      compare_rect_outputs (this_fn,
                                            params,
                                            i,
                                            this_env,
                                            this_obj = "val-newclass"))
    }

    return (ret)
}

#' Replacing class structure of tabular objects entirely should generally fail
#' @noRd
replace_rect_class_struct <- function (params, this_fn, i, test) {

    this_class <- class (params [[i]]) [1]
    operation <- paste0 ("replace class [", this_class, "] with new class")
    ret <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = this_class,
                          operation = operation)

    if (test) {

        x <- params [[i]]
        params [[i]] <- structure (x, class = c ("newclass"))
        f <- tempfile (fileext = ".txt")
        msgs <- catch_all_msgs (f, this_fn, params)

        if (!null_or_not (msgs, "error"))
            ret <- NULL
        else {
            msgs$parameter <- rep (names (params) [i], nrow (msgs))
            ret$type <- "diagnostic"
            ret$content <- paste0 ("Function [",
                               this_fn,
                               "] does not error when class structure of [",
                               this_class, "] is removed.")
        }
    }

    return (ret)

}
