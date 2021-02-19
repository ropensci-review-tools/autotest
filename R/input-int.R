is_int <- function (p) {

    p_is_int <- FALSE

    #if (is.numeric (p))
    #    if (p == round (p))
    #        p_is_int <- TRUE
    if (!p_is_int)
        p_is_int <- is.integer (p)

    return (p_is_int)
}

test_single_int_range <- function (x = NULL, ...) {

    UseMethod ("test_single_int_range", x)
}

test_single_int_range.NULL <- function (x = NULL, ...) {

    report_object (type = "dummy",
                   test_name = "int_range",
                   parameter_type = "single integer",
                   operation = "Ascertain permissible range",
                   content = paste0 ("Should either be unrestricted or ",
                                     "correspond with documentation"))
}

test_single_int_range.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- NULL

    if (!is.null (test_data)) {
        r <- test_single_int_range.NULL ()
        # These two tests are coupled, with `test` flags determined by the
        # first only
        x$test <- test_these_data (test_data, r)
        if (!x$test)
            res$type <- "no_test"
    }

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

    rd <- get_Rd_param (package = x$package_loc,
                        fn_name = x$fn,
                        param_name = names (x$params) [x$i])
    rd_range <- gregexpr ("[0-9]+", rd)
    # regmatches returns char(0) for no match, so int(0):
    rd_range <- as.integer (regmatches (rd, rd_range) [[1]])

    res_out <- test_single_int_range.NULL ()
    res_out$type <- "diagnostic"
    res_out$fn_name <- x$fn
    res_out$parameter <- names (x$params) [x$i]

    if (!any (is.finite (int_range))) {

        if (any (grepl ("unrestricted", rd))) {

            res <- NULL

        } else {

            content <- paste0 ("Parameter [",
                               names (x$params) [x$i],
                               "] permits unrestricted integer inputs, ",
                               " yet does not document this; please add ",
                               "'unrestricted' to parameter description.")
            res <- res_out
            res$content <- content
        }

    } else if (!is.null (int_range)) {

        if (length (rd_range) == 0) {

            res <- res_out
            res$content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] has no documented range")

        } else if (length (rd_range) == 1) {

            if (grepl ("negative|positive", rd)) {

                res <- NULL

            } else {

                content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] defines only one positive or negative ",
                                   "limit; plese either specify both lower and ",
                                   "upper limits, or that values must be ",
                                   "'positive' or 'negative'")
                res <- res_out
                res$content <- content
            }

        } else {

            if (any (rd_range >= max (int_range)) &
                any (rd_range <= min (int_range))) {

                res <- NULL

            } else {

                content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] responds to approximate ranges of [",
                                   paste0 (int_range, collapse = ", "),
                                   "], yet documents ranges between [",
                                   paste0 (rd_range, collapse = ", "), "]")
                res <- res_out
                res$content <- content
            }
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
        ret <- test_single_int_range.NULL ()
        ret$type <- "diagnostic"
        res$fn_name <- this_fn
        res$parameter <- names (params) [i]
        res$content <- content

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

    res <- test_single_int_range.NULL ()

    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    return (res)
}

test_int_as_dbl <- function (x = NULL, ...) {
    UseMethod ("test_int_as_dbl", x)
}

test_int_as_dbl.NULL <- function (x = NULL, ...) {
    report_object (type = "dummy",
                   test_name = "int_as_numeric",
                   parameter_type = "single integer",
                   operation = "Integer value converted to numeric",
                   content = "(Should yield same result)")
}

test_int_as_dbl.autotest_obj <- function (x, vec = FALSE, test_data = NULL) { # nolint

    if (vec)
        param_type <- "integer vector"
    else
        param_type <- "single integer"

    res <- test_int_as_dbl.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]
    res$parameter_type <- param_type

    if (!is.null (test_data)) {
        x$test <- test_these_data (test_data, res)
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {
        f <- tempfile (fileext = ".txt")
        out1 <- catch_all_msgs (f, x$fn, x$params)
        if (length (out1) == 0) {
            junk <- utils::capture.output (
                out1 <- suppressWarnings (do.call (x$fn, x$params))
                )
            x$params [[x$i]] <- x$params [[x$i]] + 0.001
            out2 <- catch_all_msgs (f, x$fn, x$params)
            if (length (out2) == 0) {
                junk <- utils::capture.output (
                    out2 <- suppressWarnings (do.call (x$fn, x$params))
                    )

                # Note that out1 can carry the `attr(., "is_int")`, so may not
                # be identical to out2
                if (max (abs (out1 - out2)) < .Machine$double.eps) {
                    res <- NULL
                } else {
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
