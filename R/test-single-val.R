autotest_single <- function (pkg, params, this_fn, test, quiet) {

    if (any (params == "NULL")) {
        params <- params [params != "NULL"]
    }

    f <- file.path (tempdir (), "junk.txt")
    res <- NULL
    if (test)
        res <- catch_all_msgs (f, this_fn, params)
    if (!is.null (res))
        res$operation <- "normal function call"

    is_single <- function (j) {
        chk <- FALSE
        if (is.null (dim (j)) && length (j) == 1) {
            if (methods::is (j, "name")) {
                val <- tryCatch (eval (parse (text = j)),
                                 error = function (e) NULL)
                if (!is.null (val))
                    chk <- length (val) == 1
            } else {
                chk <- TRUE
            }
        } else if (methods::is (j, "formula")) {
            chk <- TRUE
        }
        return (chk)
    }

    index <- which (vapply (params, function (j) is_single (j), logical (1)))
    for (i in index) {
            params_i <- params

            p_i <- params_i [[i]]
            val_type <- NULL
            check_vec <- TRUE
            if (is_int (p_i)) {
                val_type <- "integer"
                if (test)
                    res <- rbind (res,
                                  test_single_int (pkg, this_fn, params_i, i))
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                         operation = "ascertain integer range"))

            } else if (methods::is (p_i, "numeric")) {
                val_type <- "numeric"
                if (test)
                    res <- rbind (res,
                                  test_single_double (pkg, this_fn, params_i, i))
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                         operation = "add trivial noise"))
            } else if (is.character (p_i)) {
                val_type <- "character"
                if (test)
                    res <- rbind (res,
                                  test_single_char (pkg, this_fn, params_i, i))
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                         operation = "single character tests"))
            } else if (is.logical (p_i)) {
                val_type <- "logical"
                if (test)
                    res <- rbind (res,
                                  test_single_logical (pkg, this_fn, params_i, i))
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                         operation = "single logical param tests"))
            } else if (methods::is (p_i, "name") |
                       methods::is (p_i, "formula")) {
                val_type <- class (p_i) [1]
                if (test)
                    res <- rbind (res,
                                  test_single_name (pkg, this_fn, params_i, i))
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                         operation = "single formula tests"))
                check_vec <- FALSE
            } else {
                check_vec <- FALSE
            }

            # check response to vector input:
            if (check_vec) {
                if (test)
                    res <- check_vec_length (this_fn, params_i, i, val_type, res)
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                         operation = "submit vector of multiple items as single"))
            }
    }

    return (res [which (!duplicated (res)), ])
}

check_vec_length <- function (fn, params, i, val_type, res) {
    params [[i]] <- rep (params [[i]], 2)
    f <- file.path (tempdir (), "junk.txt")
    msgs <- catch_all_msgs (f, fn, params)
    if (null_or_not (msgs, c ("warning", "error"))) {
        operation <- "length 2 vector for single-length parameter"
        content <- paste0 ("parameter [",
                           names (params) [i],
                           "] is assumed to be ",
                           "a single value of ",
                           val_type,
                           " type, yet admits vectors ",
                           "of length > 1")
        res <- rbind (res,
                      report_object (type = "diagnostic",
                                     fn_name = fn,
                                     parameter = names (params) [i],
                                     operation = operation,
                                     content = content))
    }

    return (res)
}
