
# A `tryCatch` call modified to catch all messages, warnings, and errors.
# `tryCatch` can not normally do this because it only catches the first
# condition. This code is similar to the code of `demo(error.catching)`, but
# instead of assigning a global variable, it writes all messages to a local file
# and then parses the result with a separate function. See also the
# `tryCatchLog` package for further inspiration.

log_all_msgs <- function (con, this_fn) {

    x <- tryCatch (withCallingHandlers (
                                       eval (call (this_fn)),
                                       error = function(e) {
                                           write (toString (e), con, append = TRUE)
                                       },
                                       warning = function(w) {
                                           write (toString (w), con, append = TRUE)
                                           invokeRestart("muffleWarning")
                                       },
                                       message = function (z) {
                                           write (toString (z), con, append = TRUE)
                                       }),
                   error = function(e) { return ("error detected") })
    return (x)
}

parse_all_msgs <- function (f) {
    if (!file.exists (f))
        stop ("File [", f, "] does not exist")

    x <- readLines (f)
    x <- x [which (!(x == "" | duplicated (x)))]

    # some errors and warnings have different whitespace so:
    x <- x [which (!duplicated (gsub ("\\s+", "", x)))]

    ret <- NULL

    index <- grep ("Warning\\:|^simple|^Error|^simpleMessage", x)
    if (length (index) > 0) {
        for (i in index) {
            colon1 <- regexpr ("\\:", x [i])
            content <- gsub ("^\\s?", "",
                             substring (x [i], colon1 + 1, nchar (x [i])))
            types <- c ("message", "error", "warning")
            type <- NA_character_
            for (ti in types)
                if (grepl (ti, substring (x [i], 1, colon1), ignore.case = TRUE))
                    type <- ti

            ret <- rbind (ret,
                          data.frame (type = type,
                                      content = content,
                                      stringsAsFactors = FALSE)
                          )
        }
        x <- x [-index]
    }

    # anything left must be messages
    if (length (x) > 0) {
        ret <- rbind (ret,
                      data.frame (type = rep ("message", length (x)),
                                  content = x,
                                  stringsAsFactors = FALSE)
                      )
    }

    return (ret)
}

catch_all_msgs <- function (f, this_fn) {
    con <- file (f, "wt")
    suppressMessages (
        log_all_msgs (con, this_fn)
        )
    close (con)
    out <- parse_all_msgs (f)
    return (out)
}
