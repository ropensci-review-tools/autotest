autotest_rectangular <- function (params, this_fn, classes, quiet) {

    ret <- NULL

    f <- file.path (tempdir (), "junk.txt")
    
    rect_index <- which (vapply (params, function (i)
                                 length (dim (i)) == 2 &
                                     !(inherits (i, "Matrix") |
                                       inherits (i, "matrix")), logical (1)))
    for (r in rect_index) {
        x <- params [[r]]
        params_r <- params

        res1 <- res2 <- res3 <- res4 <- NULL

        other <- c ("data.frame", "tibble::tibble", "data.table::data.table")
        for (o in seq_along (other)) {
            this_ret <- test_rect_as_other (this_fn, params_r, r, other [o])
            ret <- rbind (ret, this_ret)
            if (docall (this_ret, fn, params)) {
                res <- suppressWarnings (do.call (this_fn, params_r))
                assign (paste0 ("res", o), res)
            }
        }

        ret <- compare_rect_outputs (ret, res1, res2, this_fn, params, r)
        ret <- compare_rect_outputs (ret, res1, res3, this_fn, params, r)
        ret <- compare_rect_outputs (ret, res2, res3, this_fn, params, r)

        # Modify class definitions for rectangular inputs if not excluded by
        # yaml class definitions
        if (!names (params_r) [r] %in% names (classes)) {
            # extended class structure should still work:
            params_r [[r]] <- structure (x, class = c ("newclass", class (x)))
            msgs <- catch_all_msgs (f, this_fn, params_r)
            if (!is.null (msgs))
                msgs$parameter <- rep (names (params_r) [r], nrow (msgs))
            ret <- add_msg_output (ret, msgs, types = c ("warning", "error"),
                                   operation = "rectangular parameter with extended class structure")
            if (!"error" %in% msgs$type) {
                res4 <- suppressWarnings (do.call (this_fn, params_r))
            }

            ret <- compare_rect_outputs (ret, res1, res4, this_fn, params, r)
            ret <- compare_rect_outputs (ret, res2, res4, this_fn, params, r)
            ret <- compare_rect_outputs (ret, res3, res4, this_fn, params, r)

            # new class structure which exposes 'List` structure of `data.frame`
            # and should generally fail:
            params_r [[r]] <- structure (x, class = c ("newclass"))
            f <- file.path (tempdir (), "junk.txt")
            msgs <- catch_all_msgs (f, this_fn, params_r)
            if (!null_or_not (msgs, "error")) {
                msgs$parameter <- rep (names (params_r) [r], nrow (msgs))
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = this_fn,
                                             parameter = names (params) [r],
                                             operation = "tabular structure with new class structure",
                                             content = paste0 ("Function [",
                                                               this_fn,
                                                               "] should error when ",
                                                               "class structure of `data.frame` ",
                                                               "input is removed.")))
            }
        }
    }
    return (ret)
}

chk_dims <- function (this_fn, params, r, res1, res2) {
    ret <- NULL
    if (!identical (dim (res1), dim (res2))) {
        ret <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [r],
                              operation = "compare output dimensions for different rectangular inputs",
                              content = paste0 ("Function [",
                                                this_fn,
                                                "] errors on rectangular input for [",
                                                names (params) [r],
                                                "]: Dimensions differ between ",
                                                class (res1) [1],
                                                " and ",
                                                class (res2) [1], " inputs"))
    }
    return (ret)
}

chk_names <- function (this_fn, params, r, res1, res2) {
    ret <- NULL
    if (!identical (names (res1), names (res2))) {
        ret <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [r],
                              operation = "compare output names for different rectangular inputs",
                              content = paste0 ("Function [",
                                                this_fn,
                                                "] errors on rectangular input for [",
                                                names (params) [r],
                                                "]: Column names differ between ",
                                                class (res1) [1],
                                                " and ",
                                                class (res2) [1],
                                                " inputs"))
    }
    return (ret)
}

chk_columns <- function (this_fn, params, r, res1, res2) {
    ret <- NULL
    for (i in seq (ncol (res1))) {
        if (!identical (res1 [[i]], res2 [[i]])) {
            ret <- rbind (ret,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [r],
                                         operation = "compare output columns for different rectangular inputs",
                                         content = paste0 ("Function [",
                                                           this_fn,
                                                           "] errors on rectangular ",
                                                           "input for [",
                                                           names (params) [r],
                                                           "]: Column [",
                                                           names (res1) [i],
                                                           "] differs between ",
                                                           class (res1) [1],
                                                           " and ",
                                                           class (res2) [1],
                                                           " inputs")))
        }
    }
    return (ret)
}

docall <- function (ret, fn, params) {
    docall <- FALSE
    if (is.null (ret))
        docall <- TRUE
    else if (!"error" %in% ret$type)
        docall <- TRUE

    return (docall)
}

test_rect_as_other <- function (fn, params, i, other = "data.frame") {

    f <- file.path (tempdir (), "junk.txt")
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

compare_rect_outputs <- function (ret, res1, res2, fn, params, i) {
    if (!(is.null (res1) | is.null (res2))) {
        ret <- rbind (ret, chk_dims (fn, params, i, res1, res2))
        ret <- rbind (ret, chk_names (fn, params, i, res1, res2))
        ret <- rbind (ret, chk_columns (fn, params, i, res1, res2))
    }

    return (ret)
}
