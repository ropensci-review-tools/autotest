autotest_single <- function (pkg,
                             params,
                             param_types,
                             this_fn,
                             test,
                             quiet) {

    if (any (params == "NULL")) {
        params <- params [params != "NULL"]
    }

    f <- file.path (tempdir (), "junk.txt")
    res <- NULL
    if (test)
        res <- catch_all_msgs (f, this_fn, params)
    if (!is.null (res))
        res$operation <- "normal function call"

    index <- which (param_types == "single")
    for (i in index) {
            params_i <- params

            p_i <- params_i [[i]]
            val_type <- param_type <- NULL
            check_vec <- TRUE
            if (is_int (p_i)) {
                val_type <- "integer"
                param_type <- "single integer"
                res <- rbind (res,
                              test_single_int (pkg, this_fn, params_i, i, test))

            } else if (methods::is (p_i, "numeric")) {
                val_type <- "numeric"
                param_type <- "single numeric"
                res <- rbind (res,
                              test_single_double (this_fn, params_i, i, test))
            } else if (is.character (p_i)) {
                val_type <- "character"
                param_type <- "single character"
                res <- rbind (res,
                              test_single_char (this_fn, params_i, i, test))
            } else if (is.logical (p_i)) {
                val_type <- "logical"
                param_type <- "single logical"
                if (test)
                    res <- rbind (res,
                                  test_single_logical (pkg,
                                                       this_fn,
                                                       params_i,
                                                       i))
                else {
                    op <- "single logical param tests"
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                                 parameter_type = param_type,
                                                 operation = op))
                }
            } else if (methods::is (p_i, "name") |
                       methods::is (p_i, "formula")) {
                val_type <- class (p_i) [1]
                param_type <- paste0 ("single ", val_type)
                if (test)
                    res <- rbind (res,
                                  test_single_name (pkg, this_fn, params_i, i))
                else
                    res <- rbind (res,
                                  report_object (type = "dummy",
                                                 fn_name = this_fn,
                                                 parameter = names (params) [i],
                                                 parameter_type = param_type,
                                         operation = "single formula tests"))
                check_vec <- FALSE
            } else {
                check_vec <- FALSE
            }

            # check response to vector input:
            if (check_vec) {
                res <- rbind (res,
                              single_doubled (this_fn,
                                              params_i,
                                              i,
                                              val_type,
                                              test))
            }
    }

    return (res [which (!duplicated (res)), ])
}
