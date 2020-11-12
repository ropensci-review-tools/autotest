
#' @export
summary.autotest_package <- function (object, ...) {
    # non-dplyr grouping of fn_names
    fns <- sort (unique (object$fn_name))
    errors <- warns <- messages <- diagnostics <-
        rep (NA_integer_, length (fns))
    for (i in seq (fns)) {
        objecti <- object [which (object$fn_name == fns [i]), ]
        errors [i] <- length (which (objecti$type == "error"))
        warns [i] <- length (which (objecti$type == "warning"))
        messages [i] <- length (which (objecti$type == "message"))
        diagnostics [i] <- length (which (objecti$type == "diagnostic"))
    }
    res <- data.frame (fn_name = fns,
                       num_errors = errors,
                       num_warnings = warns,
                       num_messages = messages,
                       num_diagnostics = diagnostics,
                       stringsAsFactors = FALSE)
    res [res == 0] <- NA_integer_

    pkg_name <- attr (object, "package")
    if (pkg_is_source (pkg_name)) {
        desc <- readLines (file.path (pkg_name, "DESCRIPTION"))
        pkg_name <- gsub ("Package:\\s?", "", desc [grep ("^Package\\:", desc)])
        pkg_version <- gsub ("^\\s?Version:\\s+", "",
                             desc [grep ("\\s?Version:", desc)])
    } else {
        pkg_version <- utils::packageVersion (pkg_name)
    }

    message ("autotesting package [", attr (object, "packageName"),
             ", v", pkg_version,
             "] generated ", nrow (object), " rows of output ",
             "of the following types:")
    err_txt <- ifelse (sum (errors) == 1, "", "s")
    message ("     ", sum (errors), " error", err_txt)
    warn_txt <- ifelse (sum (warns) == 1, "", "s")
    message ("     ", sum (warns), " warning", warn_txt)
    msg_txt <- ifelse (sum (messages) == 1, "", "s")
    message ("     ", sum (messages), " message", msg_txt)
    diag_txt <- ifelse (sum (diagnostics) == 1, "", "s")
    message ("     ", sum (diagnostics), " other diagnostics", diag_txt)
    objecttrim <- object [which (!grepl ("no documented example",
                                         object$content)), ]
    message ("That corresponds to ",
             round (nrow (objecttrim) /
                    length (unique (objecttrim$fn_name)), 3),
             " messages per documented function (which has examples)")
    message ("")

    print (res)

    no_ex <- grep ("no documented example", object$content)
    if (length (no_ex) > 0) {
        message ("\nIn addition to the values in that table, the output ",
                 "includes ", length (no_ex), " functions which have no ",
                 "documented examples: ")
        for (i in seq_along (no_ex)) {
            message ("    ", i, ". ", object$fn_name [no_ex [i]])
        }
    }

    if (!is.null (attr (object, "githash"))) {
        message ("\n    git hash for package as analysed here:\n    [",
                 attr (object, "githash"), "]")
    }
}
