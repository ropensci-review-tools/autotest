
test_single_char_case_dep <- function (x = NULL, ...) {
    UseMethod ("test_single_char_case_dep", x)
}

test_single_char_case_dep.NULL <- function (x) {

    report_object (type = "dummy",
                   test_name = "single_char_case",
                   parameter_type = "single character",
                   operation = "Change case",
                   content = "(Should yield same result)")
}

test_single_char_case_dep.autotest_obj <- function (x, test_data = NULL) { # nolint

    res <- NULL


    if (!is.null (test_data)) {
        r <- test_single_char_case_dep.NULL ()
        x$test <- test_data$test [test_data$test_name == r$test_name]
    }

    for (lower in c (TRUE, FALSE))
        res <- rbind (res, case_dependency (x, lower = lower))

    if (!is.null (test_data) & !x$test & !is.null (res))
        res$type <- "no_test"

    return (res)
}

case_dependency <- function (x, lower = TRUE) {

    op <- paste0 (ifelse (lower, "lower", "upper"), "-case character parameter")
    res <- test_single_char_case_dep.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]
    res$operation <- op

    if (x$test) {

        x$params [[x$i]] <- ifelse (lower,
                                    tolower (x$params [[x$i]]),
                                    toupper (x$params [[x$i]]))

        f <- tempfile ()
        msgs <- catch_all_msgs (f, x$fn, x$params)
        if (is.null (msgs)) {
            res <- NULL
        } else {
            res$type <- "diagnostic"
            res$content <- "is case dependent"
        }
    }

    return (res)
}

test_single_char_as_random <- function (x = NULL, ...) {
    UseMethod ("test_single_char_as_random", x)
}

test_single_char_as_random.NULL <- function (x = NULL) { # nolint

    report_object (type = "dummy",
                   test_name = "random_char_string",
                   parameter_type = "single character",
                   operation = "random character string as parameter",
                   content = "Should error")
}

test_single_char_as_random.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    res <- test_single_char_as_random.NULL ()
    res$fn_name <- x$fn
    res$parameter <- names (x$params) [x$i]

    if (!is.null (test_data)) {
        r <- test_single_char_as_random.NULL ()
        x$test <- test_data$test [test_data$test_name == r$test_name]
        if (!x$test)
            res$type <- "no_test"
    }

    x$params [[x$i]] <- paste0 (sample (c (letters, LETTERS),
                                        size = 10),
                                collapse = "")

    if (x$test) {

        f <- tempfile ()
        msgs <- catch_all_msgs (f, x$fn, x$params)

        if (!"error" %in% msgs$type) {
            res$type <- "diagnostic"
            res$content <- "does not match arguments to expected values"
        } else {
            res <- NULL
        }
    }

    return (res)
}

# currently not used
# nocov start
regex_param_descs <- function (h, params, i, msgs) {

    # The following lines are used just to test whether params[[i]] corresponds
    # to a formula input. They can't be used to parse general parameter
    # descriptions, because `parse_exprs` converts all quoted items into list
    # items, leaving no direct way of identifying which items may have been
    # quoted.
    #a <- rlang::parse_exprs (tools:::.Rd_get_metadata (h, "arguments"))
    a <- rlang::parse_exprs (get_Rd_metadata (h, "arguments"))
    ## also .Rd_get_metadata ("title", "name", "description", "value", ...)
    #rg_names <- unlist (lapply (a, function (i) eval (i) [[1]] [[1]]))
    #arg_name <- arg_names [i]
    arg_descs <- lapply (a, function (i) unlist (eval (i) [[2]]))
    arg_desc <- arg_descs [i]
    if (grepl ("~", params [[i]]) | any (grepl ("formula", arg_desc)))
        return (msgs) # do not test formulas

    # regex the actual string to extract all item descriptions.
    # These are formatted
    # "\\item { <name> } { <description> }
    # start by removing the { <name> } portion
    hc <- paste0 (paste (h), collapse = " ")
    index <- gregexpr ("\\\\item\\s+\\{(.*?)\\}", hc) [[1]]
    #index <- gregexpr ("\\\\item\\s+\\{.*\\}\\s+\\{(.*?)\\}", hc) [[1]]
    i <- as.integer (index)
    j <- i + attr (index, "match.length")
    rms <- gsub ("}", "\\}",
                 gsub ("\\item\\s+", "",
                       substring (hc, i, j)),
                 fixed = TRUE)

    # these don't necessary match the end brackets, so need extension where not
    n_open <- vapply (gregexpr ("\\{", rms), length, integer (1))
    n_closed <- vapply (gregexpr ("\\}", rms), length, integer (1))
    index <- which (n_closed < n_open)
    for (k in index) {
        pos <- gregexpr ("\\}", substring (hc, j [k], nchar (hc))) [[1]]
        #j [k] <- j [k] + pos [n_open [k] - n_closed [k]]
        add <- substring (hc,
                          j [k] + 1,
                          j [k] + pos [n_open [k] - n_closed [k]])
        add <- gsub ("}", "\\}", add, fixed = TRUE)
        add <- gsub (":", "\\:", add, fixed = TRUE)
        rms [k] <- paste0 (rms [k], add)
        rms [k] <- gsub (" {", " \\{", rms [k], fixed = TRUE)
    }

    for (i in rms)
        hc <- gsub (i, "", hc)

    # The remaining values between the curly braces are the descriptions, but
    # this requires finding matching curly braces.
    index <- gregexpr ("\\\\item\\s+\\{(.*?)\\}", hc) [[1]]
    #index <- gregexpr ("\\\\item\\s+\\{([^}]*)\\}", hc) [[1]]
    i <- as.integer (index)
    j <- i + attr (index, "match.length")
    res <- substring (hc, i, j)
    # extend the k'th element of res to match the closing curly brace
    match_res_k <- function (res, hc, i, j, k) {
        rk <- res [k]
        #(length (gregexpr ("\\{", rk) [[1]]) !=
        #    length (gregexpr ("\\}", rk) [[1]]))
        if (length (gregexpr ("\\{", rk) [[1]]) !=
            length (gregexpr ("\\}", rk) [[1]])) {
            this_i <- i [k] + 1
            if (k < length (i))
                this_j <- i [k + 1] - 1
            else
                this_j <- nchar (hc)
            opens <- gregexpr ("\\{", substring (hc, this_i, this_j)) [[1]]
            closes <- gregexpr ("\\}", substring (hc, this_i, this_j)) [[1]]
            if (length (opens) < length (closes)) {
                index <- which (opens < closes [seq (length (opens))])
            } else {
                index <- which (opens [seq (closes)] < closes)
            }
            if (length (index) < length (closes)) {
                closes <- closes [which (!seq (closes) %in% index)] [1]
            } else {
                closes <- closes [length (closes)]
            }
            rk <- substring (hc, i [k], this_i + closes + 1)
        }
        return (rk)
    }
    for (k in seq (res))
        res [k] <- match_res_k (res, hc, i, j, k)
}
# nocov end
