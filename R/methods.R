#' @export
summary.autotest_pkg <- function (x) {
    # non-dplyr grouping of fn_names
    fns <- sort (unique (x$fn_name))
    errors <- warns <- messages <- diagnostics <- rep (NA_integer_, length (fns))
    for (i in seq (fns)) {
        xi <- x [which (x$fn_name == fns [i]), ]
        errors [i] <- length (which (xi$type == "error"))
        warns [i] <- length (which (xi$type == "warning"))
        messages [i] <- length (which (xi$type == "message"))
        diagnostics [i] <- length (which (xi$type == "diagnostic"))
    }
    res <- data.frame (fn_name = fns,
                       num_errors = errors,
                       num_warnings = warns,
                       num_messages = messages,
                       num_diagnostics = diagnostics,
                       stringsAsFactors = FALSE)
    res [res == 0] <- NA_integer_
    print (res)
}
