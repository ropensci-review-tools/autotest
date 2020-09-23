autotest_single <- function (pkg, params, this_fn, quiet) {

    null_params <- NULL
    if (any (params == "NULL")) {
        null_params <- params [params == "NULL"]
        params <- params [params != "NULL"]
    }

    f <- file.path (tempdir (), "junk.txt")
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
            if (is_int (p_i)) {
                val_type <- "integer"
                res <- rbind (res,
                              test_single_int (pkg, this_fn, params_i, i))
            } else if (is.character (p_i)) {
                val_type <- "character"
                res <- rbind (res,
                              test_single_char (pkg, this_fn, params_i, i))
            } else if (is.logical (p_i)) {
                val_type <- "logical"
                res <- rbind (res,
                              test_single_logical (pkg, this_fn, params_i, i))
            } else if (methods::is (p_i, "name") | methods::is (p_i, "formula")) {
                val_type <- class (p_i) [1]
                res <- rbind (res,
                              test_single_name (pkg, this_fn, params_i, i))
            }

            # check response to vector input:
            if (is_int (p_i) | is.character (p_i) | is.logical (p_i)) {
                params_i [[i]] <- rep (p_i, 2)
                f <- file.path (tempdir (), "junk.txt")
                msgs <- catch_all_msgs (f, this_fn, params_i)
                if (null_or_not (msgs, c ("warning", "error"))) {
                    res <- rbind (res,
                                  report_object (type = "diagnostic",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                                 operation = "length 2 vector for single-length parameter",
                                                 content = paste0 ("parameter [",
                                                                   names (params) [i],
                                                                   "] is assumed to be ",
                                                                   "a single value of ",
                                                                   val_type,
                                                                   " type, yet admits vectors ",
                                                                   "of length > 1")))
                }
            }
    }

    return (res [which (!duplicated (res)), ])
}
