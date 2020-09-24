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

    message ("autotesting package [", attr (x, "packageName"),
             ", v", packageVersion (attr (x, "packageName")),
             "] generated ", nrow (x), " rows of output ",
             "of the following types:")
    err_txt <- ifelse (sum (errors) == 1, "", "s")
    message ("     ", sum (errors), " error", err_txt)
    warn_txt <- ifelse (sum (warns) == 1, "", "s")
    message ("     ", sum (warns), " warning", warn_txt)
    msg_txt <- ifelse (sum (messages) == 1, "", "s")
    message ("     ", sum (messages), " message", msg_txt)
    diag_txt <- ifelse (sum (diagnostics) == 1, "", "s")
    message ("     ", sum (diagnostics), " other diagnostics", diag_txt)
    xtrim <- x [which (!grepl ("no documented example", x$content)), ]
    message ("That corresponds to ",
             round (nrow (xtrim) / length (unique (xtrim$fn_name)), 3),
             " messages per documented function (which has examples)")
    message ("")

    print (res)

    no_ex <- grep ("no documented example", x$content)
    if (length (no_ex) > 0) {
        message ("\nIn addition to the values in that table, the output ",
                 "includes ", length (no_ex), " functions which have no ",
                 "documented examples: ")
        for (i in seq_along (no_ex)) {
            message ("    ", i, ". ", x$fn_name [no_ex [i]])
        }
    }

    if (!is.null (attr (x, "githash"))) {
        message ("\n    git hash for package as analysed here:\n    [",
                 attr (x, "githash"), "]")
    }
}
