is_int <- function (p) {

    p_is_int <- FALSE

    if (is.numeric (p))
        if (p == round (p))
            p_is_int <- TRUE
    if (!p_is_int)
        p_is_int <- is.integer (p)

    return (p_is_int)
}

test_single_int <- function (pkg, this_fn, params, i) {

    res <- NULL

    int_range <- get_int_range (this_fn, params, i)
    if (!is.numeric (int_range)) # call with default parameters errored
        return (int_range)

    if (!any (is.finite (int_range))) {
        content <- paste0 ("Parameter [",
                           names (params) [i],
                           "] permits unrestricted integer inputs")
        res <- rbind (res,
                      report_object (type = "diagnostic",
                                     fn_name = this_fn,
                                     parameter = names (params) [i],
                                     operation = "ascertain integer range",
                                     content = content))
    } else if (!is.null (int_range)) {
        content <- paste0 ("Parameter [",
                           names (params) [i],
                           "] responds to integer values in [",
                           paste0 (int_range, collapse = ", "), "]")
        res <- rbind (res,
                      report_object (type = "diagnostic",
                                     fn_name = this_fn,
                                     parameter = names (params) [i],
                                     operation = "ascertain integer range",
                                     content = content))

        rd <- get_Rd_param (package = pkg,
                            fn_name = this_fn,
                            param_name = names (params) [i])
        range_in_rd <- vapply (int_range, function (j)
                               grepl (j, rd), logical (1))
        if (!all (range_in_rd)) {
            operation <- "match integer range to documentation"
            content <- paste0 (" Parameter range for ",
                               names (params) [i],
                               " is NOT documented")
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [i],
                                         operation = operation,
                                         content = content))
        }
    }

    return (res)
}



# Test int inputs to functions to determine accepted range of inputs.
get_int_range <- function (this_fn, params, i) {

    get_fn_response <- function (this_fn, params) {
        f <- file.path (tempdir (), "junk.txt")
        msgs <- catch_all_msgs (f, this_fn, params)
        val <- 3
        if (!is.null (msgs)) {
            if (any (msgs$type == "error"))
                val <- 1
            else if (any (msgs$type == "warning"))
                val <- 2
        }

        return (val)
    }

    # if standard call generates an error, then return that as a standard
    # data.frame object. Otherwise return value from the subsequent code is
    # the actual int range.
    if (get_fn_response (this_fn, params) == 1) { # allow warnings

        content <- paste0 ("Function [", this_fn,
                           "] does not respond appropriately for ",
                           "specified/default input [",
                           names (params) [i], " = ",
                           params [[i]], "]")
        ret <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [i],
                              content = content)
        return (ret)
    }

    # log-space search by 'step_factor':
    stepdown <- function (this_fn, params, i, maxval, step_factor = 10) {
        val <- maxval
        while (val == maxval && abs (params [[i]]) > 1) {
            params [[i]] <- ceiling (params [[i]] / step_factor)
            val <- get_fn_response (this_fn, params)
        }
        return (params [[i]])
    }

    params [[i]] <- .Machine$integer.max
    maxval <- get_fn_response (this_fn, params)
    if (maxval > 1) {
        p_i_max <- params [[i]]
    } else {
        st <- system.time (
            p_i <- stepdown (this_fn, params, i, maxval, step_factor = 10)
            ) [3]
        # then step back up factor and 10 and zoom in, but only for function
        # calls which are relatively quick, arbitrarily deemed here to mean < 10
        # seconds for coarse stepdown
        p_i_max <- p_i
        if (st < 10) {
            params [[i]] <- floor (p_i * 10)
            maxval <- get_fn_response (this_fn, params)
            p_i_max <- stepdown (this_fn, params, i, maxval, step_factor = 2)
        }
    }

    params [[i]] <- -.Machine$integer.max
    maxval <- get_fn_response (this_fn, params)
    if (maxval > 1) {
        p_i <- params [[i]]
    } else {
        p_i <- stepdown (this_fn, params, i, maxval, step_factor = 10)

        fn_resp_no_error <- function (this_fn, params, i, val) {
            params [[i]] <- val
            return (get_fn_response (this_fn, params) > 1)
        }

        if (p_i == 0) {
            if (fn_resp_no_error (this_fn, params, i, 0))
                p_i <- 0
            else if (fn_resp_no_error (this_fn, params, i, 1))
                p_i <- 1
        } else { # p_i must be < 0
            params [[i]] <- -floor (p_i * 10)
            maxval <- get_fn_response (this_fn, params)
            p_i <- stepdown (this_fn, params, i, maxval, step_factor = 2)
        }
    }

    if (p_i == -.Machine$integer.max)
        p_i <- -Inf
    if (p_i_max == .Machine$integer.max)
        p_i_max <- Inf

    return (c (p_i, p_i_max))
}
