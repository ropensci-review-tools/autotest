is_int <- function (p) {

    p_is_int <- FALSE

    #if (is.numeric (p))
    #    if (p == round (p))
    #        p_is_int <- TRUE
    if (!p_is_int)
        p_is_int <- is.integer (p)
    if (!p_is_int & "is_int" %in% names (attributes (p)))
        p_is_int <- attr (p, "is_int")

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
        test_flag <- test_these_data (test_data, r)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
    }

    if (x$test)
        res <- single_int_range (x)
    else
        res <- single_int_dummy_report (x)

    if (!is.null (test_data) & !x$test & !is.null (res))
        res$type <- "no_test"

    return (res)
}

single_int_range <- function (x) {

    rd <- get_Rd_param (package = x$package_loc,
                        fn_name = x$fn,
                        param_name = names (x$params) [x$i])

    doc_range <- documented_int_range (rd)

    int_range <- get_int_range (x$fn, x$params, x$i, doc_range)

    if (!is.numeric (int_range)) # call with default parameters errored
        return (int_range)

    res <- NULL

    res_out <- test_single_int_range.NULL ()
    res_out$type <- "diagnostic"
    res_out$fn_name <- x$fn
    res_out$parameter <- names (x$params) [x$i]

    if (!any (is.finite (int_range))) {

        if (!any (grepl ("unrestricted", rd, ignore.case = TRUE))) {

            content <- paste0 ("Parameter [",
                               names (x$params) [x$i],
                               "] permits unrestricted integer inputs, ",
                               " yet does not document this; please add ",
                               "'unrestricted' to parameter description.")
            res <- res_out
            res$content <- content
        }

    } else if (!is.null (int_range)) {

        # Extract any numbers from param description and presume they specify
        # some kind of range
        rd_numbers <- gregexpr ("[0-9]+", rd)
        # regmatches returns char(0) for no match, so int(0):
        rd_numbers <- as.integer (regmatches (rd, rd_numbers) [[1]])

        has_neg_pos <- grepl ("negative|positive", rd, ignore.case = TRUE)

        res <- NULL

        if (!has_neg_pos) {

            if (length (rd_numbers) == 0L) {

                res <- res_out
                res$content <- paste0 ("Parameter [",
                                       names (x$params) [x$i],
                                       "] has no documented range")

            } else if (length (rd_numbers) == 1L) {

                content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] defines only one positive or negative ",
                                   "limit; plese either specify both lower ",
                                   "and upper limits, or that values must be ",
                                   "'positive' or 'negative'")
                res <- res_out
                res$content <- content

            } else if (!(any (rd_numbers >= max (int_range)) &
                         any (rd_numbers <= min (int_range)))) {

                content <- paste0 ("Parameter [",
                                   names (x$params) [x$i],
                                   "] responds to approximate ranges of [",
                                   paste0 (int_range, collapse = ", "),
                                   "], yet documents ranges between [",
                                   paste0 (rd_numbers, collapse = ", "), "]")
                res <- res_out
                res$content <- content

            }
        } # end has_neg_poas
    } # end else !is.null(int_range)

    return (res)
}

#' Get permissible integer range from a given .Rd entry
#'
#' @return Vector of two integer values of (lower, upper) limits, with default
#' of +/- Inf.
#' @noRd
documented_int_range <- function (rd) {

    int_from_rd <- function (rd, ptn) {
        g <- regmatches (rd, regexpr (ptn, rd))
        val <- suppressWarnings (
            as.integer (regmatches (g, regexpr ("[0-9]+", g)))
            )
        if (length (val) == 0L) {
            val <- NA_integer_
        }
        return (val)
    }

    doc_range <- c (-Inf, Inf)

    ptn_lower <- "(more|greater|larger)\\sthan|lower\\slimit\\sof|above"
    ptn_upper <- "(less|lower|smaller)\\sthan|upper\\slimit\\sof|below"

    # test for the upper bound
    if (any(grepl("negative integer", rd, ignore.case = TRUE))) {
        doc_range[2] <- 0L
    } else if (any(grepl(ptn_upper, rd, ignore.case = TRUE))) {
        val <- int_from_rd(rd, paste0("(", ptn_upper, ")\\s[0-9]+"))
        if (!is.na(val)) {
            doc_range[2] <- val
        }
    }
    
    # test for the lower bound
    if (any(grepl("positive integer", rd, ignore.case = TRUE))) {
        doc_range[1] <- 0L
    } else if (any(grepl(ptn_lower, rd, ignore.case = TRUE))) {
        val <- int_from_rd(rd, paste0("(", ptn_lower, ")\\s[0-9]+"))
        if (!is.na(val)) {
            doc_range[1] <- val
        }
    }

    return (doc_range)
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


#' Test int inputs to functions to determine accepted range of inputs.
#'
#' @param test_range Ranges potentially extracted from documented limits to be
#' used in testing
#' @noRd
get_int_range <- function (this_fn, params, i, test_range = c (-Inf, Inf)) {

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
        ret$fn_name <- this_fn
        ret$parameter <- names (params) [i]
        ret$content <- content

        return (ret)
    }

    p_i_max <- int_upper_limit (this_fn, params, i, test_range [2])

    p_i <- int_lower_limit (this_fn, params, i, test_range [1])

    return (c (p_i, p_i_max))
}

int_upper_limit <- function (this_fn, params, i, limit) {

    if (limit == Inf) {
        limit <- .Machine$integer.max
    }
    params [[i]] <- limit
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

int_lower_limit <- function (this_fn, params, i, limit) {

    if (limit == -Inf) {
        limit <- -.Machine$integer.max
    }
    params [[i]] <- limit
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
        test_flag <- test_these_data (test_data, res)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            res$type <- "no_test"
    }

    if (x$test) {

        f <- tempfile (fileext = ".txt")
        out1 <- catch_all_msgs (f, x$fn, x$params)

        seed <- sample.int (.Machine$integer.max, 1L)

        if (length (out1) == 0) {
            junk <- utils::capture.output (
                out1 <- suppressWarnings (
                            withr::with_seed (seed,
                                              do.call (x$fn, x$params)))
                )
            x$params [[x$i]] <- x$params [[x$i]] + 100 * .Machine$double.eps
            out2 <- catch_all_msgs (f, x$fn, x$params)
            if (length (out2) == 0) {
                Sys.sleep (0.5) # in case Sys.time is used
                junk <- utils::capture.output (
                    out2 <- suppressWarnings (
                                withr::with_seed (seed,
                                                 do.call (x$fn, x$params)))
                    )

                # Note that out1 can carry the `attr(., "is_int")`, so may not
                # be identical to out2
                no_change <- TRUE
                if (is.numeric (out1) & is.numeric (out2)) {
                    if (max (abs (out1 - out2)) > (100 * .Machine$double.eps))
                        no_change <- FALSE
                } else {
                    no_change <- length (setdiff (out1, out2)) == 0
                }

                if (no_change) {

                    res <- NULL
                } else {
                    res$type <- "diagnostic"
                    res$content <- paste0 ("Function [",
                                           x$fn,
                                           "] returns different values when ",
                                           "parameter [",
                                           names (x$params) [x$i],
                                           "] only demonstrated or documented ",
                                           " as int-valued is submitted as",
                                           " double.")
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
