is_int <- function (p) {

    p_is_int <- FALSE

    if (is.numeric (p))
        if (p == round (p))
            p_is_int <- TRUE
    if (!p_is_int)
        p_is_int <- is.integer (p)

    return (p_is_int)
}

test_single_int_range <- function (x = NULL, ...) {
    UseMethod ("test_single_int_range", x)
}

test_single_int_range.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   parameter_type = "single integer",
                   operation = "Ascertain permissible range",
                   content = "Should either be unrestricted or correspond with documentation")
}

test_single_int_range.autotest_obj <- function (x, ...) {

    res <- NULL

    if (x$test)
        res <- single_int_range (x)
    else
        res <- single_int_dummy_report (x)

    return (res)
}

single_int_range <- function (x) {

    int_range <- get_int_range (x$fn, x$params, x$i)

    if (!is.numeric (int_range)) # call with default parameters errored
        return (int_range)

    res <- NULL

    if (!any (is.finite (int_range))) {
        content <- paste0 ("Parameter [",
                           names (x$params) [x$i],
                           "] permits unrestricted integer inputs")
        res <- report_object (type = "diagnostic",
                              fn_name = x$fn,
                              parameter = names (x$params) [x$i],
                              parameter_type = "single integer",
                              operation = "Ascertain integer range",
                              content = content)
    } else if (!is.null (int_range)) {
        content <- paste0 ("Parameter [",
                           names (x$params) [x$i],
                           "] responds to integer values in [",
                           paste0 (int_range, collapse = ", "), "]")
        res <- report_object (type = "diagnostic",
                              fn_name = x$fn,
                              parameter = names (x$params) [x$i],
                              parameter_type = "single integer",
                              operation = "Ascertain integer range",
                              content = content)

        rd <- get_Rd_param (package = x$package,
                            fn_name = x$fn,
                            param_name = names (x$params) [x$i])
        range_in_rd <- vapply (int_range, function (j)
                               grepl (j, rd), logical (1))

        if (!all (range_in_rd)) {

            operation <- "Match integer range to documentation"
            content <- paste0 (" Parameter range for ",
                               names (x$params) [x$i],
                               " is NOT documented")
            res <- rbind (res,
                          report_object (type = "diagnostic",
                                         fn_name = x$fn,
                                         parameter = names (x$params) [x$i],
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

single_int_dummy_report <- function (x) {
    report_object (type = "dummy",
                   fn_name = x$fn,
                   parameter = names (x$params) [x$i],
                   parameter_type = "single integer",
                   operation = "Ascertain integer range",
                   content = paste0 ("Should accord with documented range ",
                                     "if given, or not error otherwise"))
}

int_as_double <- function (x, vec = FALSE) {

    operation <- "Integer vector converted to numeric"

    if (vec)
        param_type <- "integer vector"
    else
        param_type <- "single integer"

    res <- report_object (type = "dummy",
                          fn_name = x$fn,
                          parameter = names (x$params) [x$i],
                          parameter_type = param_type,
                          operation = operation,
                          content = "(Should yield same result)")

    if (x$test) {
        f <- tempfile (fileext = ".txt")
        out1 <- catch_all_msgs (f, x$fn, x$params)
        if (length (out1) == 0) {
            junk <- utils::capture.output (
                out1 <- suppressWarnings (do.call (x$fn, x$params))
                )
            x$params [[x$i]] <- as.numeric (x$params [[x$i]])
            out2 <- catch_all_msgs (f, x$fn, x$params)
            if (length (out2) == 0) {
                junk <- utils::capture.output (
                    out2 <- suppressWarnings (do.call (x$fn, x$params))
                    )

                if (identical (out1, out2))
                    res <- NULL
                else {
                    res$type <- "diagnostic"
                    res$content <- paste0 ("Function [",
                                           x$fn,
                                           "] returns different values when ",
                                           "assumed int-valued parameter [",
                                           names (x$params) [x$i],
                                           "] is submitted as double.")
                }
            } else {
                res <- out2
            }
        } else {
            res <- out1
        }
    }

    return (res)
}
