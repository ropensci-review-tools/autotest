autotest_vector <- function (params, this_fn, classes, quiet) {

    chk <- TRUE

    null_params <- NULL
    if (any (params == "NULL")) {
        null_params <- params [params == "NULL"]
        params <- params [params != "NULL"]
    }

    vec_index <- which (vapply (params, function (i)
                                length (i) > 1 && is.null (dim (i)), logical (1)))
    for (v in vec_index) {
        params_v <- params

        res1 <- tryCatch (do.call (this_fn, params_v),
                          warning = function (w) w,
                          error = function (e) e)
        warn <- FALSE
        if (methods::is (res1, "warning")) {
            cli::cli_text (cli::col_yellow ("function [", this_fn,
                                            "] issued a Warning: ",
                                            res1$message))
            warn <- TRUE
            res1 <- suppressWarnings (do.call (this_fn, params_v))
        }

        # int columns submitted as double should return different result:
        if (typeof (params_v [[v]]) == "integer") {
            params_v [[v]] <- as.numeric (params_v [[v]])
            if (warn)
                res2 <- suppressWarnings (do.call (this_fn, params_v))
            else
                res2 <- do.call (this_fn, params_v)
            if (!identical (res1, res2)) {
                warning ("Function [", this_fn, "] returns different values when ",
                         "assumed int-valued parameter [", names (params) [v],
                         "] is submitted as double.\n Error message: ",
                         "different classes when submitted as ", names (params) [v],
                         res3$message, call. = FALSE, immediate. = TRUE)
            }
            params_v <- params
        }

        # class definitions for vector columns should be ignored
        if (!names (params_v) [vec_index [v]] %in% names (classes)) {
            x <- params_v [[v]]
            class (x) <- "different"
            params_v [[v]] <- x
            res3 <- tryCatch (
                              do.call (this_fn, params_v),
                              warning = function (w) w,
                              error = function (e) e)
            if (methods::is (res3, "error")) {
                chk <- FALSE
                warning ("Function [", this_fn, "] errors on vector columns with ",
                         "different classes when submitted as ", names (params) [v],
                         "\n  Error message: ", res3$message,
                         call. = FALSE, immediate. = TRUE)
            } else {
                # TODO: Expectation - they need not be identical, because class
                # def may be carried over to result
                #expect_identical (res1, res3)
            }
            params_v <- params
        } else {
            # TODO: Implement check for all nominated classes
        }

        # List-columns
        x <- params_v [[v]]
        x <- I (as.list (x))
        params_v [[v]] <- x
        res4 <- tryCatch (
                          do.call (this_fn, params_v),
                          warning = function (w) w,
                          error = function (e) e)
        if (methods::is (res4, "error")) {
            chk <- FALSE
            warning ("Function [", this_fn, "] errors on list-columns ",
                     "when submitted as ", names (params) [v],
                     "\n  Error message: ", res4$message,
                     call. = FALSE, immediate. = TRUE)
        } else {
            # TODO: Expectation here too
            #expect_identical (res1, res4)
        }
    }

    return (chk)
}
