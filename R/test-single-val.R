autotest_single <- function (pkg, params, this_fn, quiet) {

    chk <- TRUE

    null_params <- NULL
    if (any (params == "NULL")) {
        null_params <- params [params == "NULL"]
        params <- params [params != "NULL"]
    }

    res1 <- tryCatch (do.call (this_fn, params),
                      warning = function (w) w,
                      error = function (e) e)
    warn <- FALSE
    if (methods::is (res1, "warning")) {
        cli::cli_text (cli::col_yellow ("function [", this_fn,
                                        "] issued a Warning: ",
                                        res1$message))
        warn <- TRUE
        res1 <- suppressWarnings (do.call (this_fn, params))
    }

    index <- which (vapply (params, function (j)
                            is.null (dim (j)) && length (j) == 1, logical (1)))
    for (i in index) {
            params_i <- params

            p_i <- params_i [[i]]
            val_type <- NULL
            if (is_int (p_i)) {
                val_type <- "integer"
                chk <- test_single_int (pkg, this_fn, params_i, i)
            } else if (is.character (p_i)) {
                val_type <- "character"
                chk <- test_single_char (pkg, this_fn, params_i, i)
            } else if (is.logical (p_i)) {
                val_type <- "logical"
                chk <- test_single_logical (pkg, this_fn, params_i, i)
            }

            # check response to vector input:
            if (is_int (p_i) | is.character (p_i) | is.logical (p_i)) {
                params_i [[i]] <- rep (p_i, 2)
                res1 <- tryCatch (do.call (this_fn, params_i),
                                  warning = function (w) w,
                                  error = function (e) e)
                if (!(methods::is (res1, "warning") | methods::is (res1, "error"))) {
                    warning ("parameter [", names (params) [i], "] is assumed to be ",
                             "a single value of ", val_type, " type, ",
                             "yet admits vectors of length > 1",
                             call. = FALSE, immediate. = TRUE)
                    chk <- FALSE
                }
            }
    }

    return (chk)
}
