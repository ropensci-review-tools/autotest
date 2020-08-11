
# Check (1) whether return values are documented at all; and (2) If so, whether
# they describe the class or type of return object. The latter is currently only
# crudely tested with a simple `grep([[Cc]lass|[Oo]bject)`.
autotest_return <- function (pkg, params, this_fn) {

    chk <- TRUE

    null_params <- NULL
    if (any (params == "NULL")) {
        null_params <- params [params == "NULL"]
        params <- params [params != "NULL"]
    }

    retval <- tryCatch (
                        do.call (this_fn, params),
                        warning = function (w) w,
                        error = function (e) e)
    if (methods::is (retval, "warning")) {
        cli::cli_text (cli::col_yellow ("Function [", this_fn,
                                        "] issued a Warning: ",
                                        retval$message))
        retval <- suppressWarnings (do.call (this_fn, params))
    }

    if (!is.null (attr (retval, "class"))) {
        Rd_value <- get_Rd_value (package = pkg, fn_name = this_fn)
        if (is.null (Rd_value)) {
            chk <- FALSE
            warning ("Function [", this_fn, "] does not specify a return value, ",
                     "yet returns a value of class [",
                     paste0 (attr (retval, "class"), collapse = ", "), "]",
                     call. = FALSE, immediate. = TRUE)
        } else {
            chk <- grepl ("[Cc]lass|[Oo]bject", Rd_value)
            if (!chk)
                warning ("Function [", this_fn, "] does not specify class of return value, ",
                         "yet returns a value of class [",
                         paste0 (attr (retval, "class"), collapse = ", "), "]",
                         call. = FALSE, immediate. = TRUE)
        }
    }

    return (chk)
}
