autotest_rectangular <- function (params, this_fn, classes, quiet) {

    ret <- NULL
    
    rect_index <- which (vapply (params, function (i)
                                 length (dim (i)) == 2 &
                                     !(inherits (i, "Matrix") |
                                       inherits (i, "matrix")), logical (1)))
    for (r in rect_index) {
        x <- params [[r]]
        params_r <- params

        params_r [[r]] <- data.frame (x)
        res1 <- do.call (this_fn, params_r)

        params_r [[r]] <- tibble::tibble (data.frame (x))
        res2 <- do.call (this_fn, params_r)

        params_r [[r]] <- data.table::data.table (x)
        res3 <- do.call (this_fn, params_r)

        ret <- rbind (ret, chk_dims (this_fn, params, r, res1, res2))
        ret <- rbind (ret, chk_names (this_fn, params, r, res1, res2))
        ret <- rbind (ret, chk_columns (this_fn, params, r, res1, res2))

        ret <- rbind (ret, chk_dims (this_fn, params, r, res1, res3))
        ret <- rbind (ret, chk_names (this_fn, params, r, res1, res3))
        ret <- rbind (ret, chk_columns (this_fn, params, r, res1, res3))

        # Modify class definitions for rectangular inputs if not excluded by
        # yaml class definitions
        if (!names (params_r) [r] %in% names (classes)) {
            # extended class structure should still work:
            params_r [[r]] <- structure (x, class = c ("newclass", class (x)))
            res4 <- do.call (this_fn, params_r)

            ret <- rbind (ret, chk_dims (this_fn, params, r, res1, res4))
            ret <- rbind (ret, chk_names (this_fn, params, r, res1, res4))
            ret <- rbind (ret, chk_columns (this_fn, params, r, res1, res4))

            # new class structure which exposes 'List` structure of `data.frame`
            # and should generally fail:
            params_r [[r]] <- structure (x, class = c ("newclass"))
            f <- file.path (tempdir (), "junk.txt")
            msgs <- catch_all_msgs (f, this_fn, params_r)
            if (!null_or_not (msgs, "error")) {
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = this_fn,
                                             parameter = names (params) [r],
                                             content = paste0 ("Function [",
                                                               this_fn,
                                                               "] should error when ",
                                                               "class structure of `data.frame` ",
                                                               "input is removed.")))
            }
        } else {
            # TODO: Implement check for all nominated classes
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
