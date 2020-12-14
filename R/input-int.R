is_int <- function (p) {

    p_is_int <- FALSE

    if (is.numeric (p))
        if (p == round (p))
            p_is_int <- TRUE
    if (!p_is_int)
        p_is_int <- is.integer (p)

    return (p_is_int)
}

test_single_int <- function (pkg, this_fn, params, i, test = TRUE) {

    res <- NULL

    if (test)
        res <- test_single_int_range (pkg, this_fn, params, i)
    else
        res <- single_int_dummy_report (this_fn, params, i)

    return (res)
}

test_single_int_range <- function (pkg, this_fn, params, i) {

    int_range <- get_int_range (this_fn, params, i)

    if (!is.numeric (int_range)) # call with default parameters errored
        return (int_range)

    res <- NULL

    if (!any (is.finite (int_range))) {
        content <- paste0 ("Parameter [",
                           names (params) [i],
                           "] permits unrestricted integer inputs")
        res <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [i],
                              parameter_type = "single integer",
                              operation = "Ascertain integer range",
                              content = content)
    } else if (!is.null (int_range)) {
        content <- paste0 ("Parameter [",
                           names (params) [i],
                           "] responds to integer values in [",
                           paste0 (int_range, collapse = ", "), "]")
        res <- report_object (type = "diagnostic",
                              fn_name = this_fn,
                              parameter = names (params) [i],
                              parameter_type = "single integer",
                              operation = "Ascertain integer range",
                              content = content)

        rd <- get_Rd_param (package = pkg,
                            fn_name = this_fn,
                            param_name = names (params) [i])
        range_in_rd <- vapply (int_range, function (j)
                               grepl (j, rd), logical (1))

        if (!all (range_in_rd)) {

            operation <- "Match integer range to documentation"
            content <- paste0 (" Parameter range for ",
                               names (params) [i],
                               " is NOT documented")
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = names (params) [i],
                                         parameter_type = "single integer",
                                         operation = operation,
                                         content = content))
        }
    }

    return (res)
}


get_fn_response <- function (this_fn, params) {
    f <- tempfile (fileext = ".txt")
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

# log-space search by 'step_factor':
stepdown <- function (this_fn, params, i, maxval, step_factor = 10) {
    val <- maxval
    while (val == maxval && abs (params [[i]]) > 1) {
        params [[i]] <- ceiling (params [[i]] / step_factor)
        val <- get_fn_response (this_fn, params)
    }
    return (params [[i]])
}


# Test int inputs to functions to determine accepted range of inputs.
get_int_range <- function (this_fn, params, i) {

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
                              parameter_type = "single integer",
                              content = content)
        return (ret)
    }

    p_i_max <- int_upper_limit (this_fn, params, i)

    p_i <- int_lower_limit (this_fn, params, i)

    return (c (p_i, p_i_max))
}

int_upper_limit <- function (this_fn, params, i) {

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

    if (p_i_max == .Machine$integer.max)
        p_i_max <- Inf

    return (p_i_max)
}

int_lower_limit <- function (this_fn, params, i) {

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

    return (p_i)
}

single_int_dummy_report <- function (this_fn, params, i) {
    report_object (type = "dummy",
                   fn_name = this_fn,
                   parameter = names (params) [i],
                   parameter_type = "single integer",
                   operation = "Ascertain integer range")
}

int_as_double <- function (this_fn, params, i,
                           vec = FALSE,
                           test = TRUE) {

    operation <- "Integer vector converted to numeric"
    if (vec)
        param_type <- "integer vector"
    else
        param_type <- "single integer"
    res <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = param_type,
                          operation = operation)

    if (test) {
        out1 <- suppressWarnings (do.call (this_fn, params))
        params [[i]] <- as.numeric (params [[i]])
        out2 <- suppressWarnings (do.call (this_fn, params))

        if (identical (out1, out2))
            res <- NULL
        else {
            res$type <- "diagnostic"
            res$content <- paste0 ("Function [",
                                   this_fn,
                                   "] returns different values when ",
                                   "assumed int-valued parameter [",
                                   names (params) [i],
                                   "] is submitted as double.")
        }
    }

    return (res)
}
