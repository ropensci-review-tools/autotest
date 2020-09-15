
# A `tryCatch` call modified to catch all messages, warnings, and errors.
# `tryCatch` can not normally do this because it only catches the first
# condition. This code is similar to the code of `demo(error.catching)`, but
# instead of assigning a global variable, it uses a sink to capture all output,
# and the parses the result with a separate function. See also the `tryCatchLog`
# package for further inspiration.

catch_all_msgs <- function (f, this_fn) {
    con <- file (f, "wt")
    sink (file = con, type = "message")

    tryCatch (withCallingHandlers (
                                   eval (call (this_fn)),
                                   error=function(e) message (paste0 (e)),
                                       warning=function(w) {
                                           message (paste0 (w))
                                           invokeRestart("muffleWarning")
                                       }))
    sink (file = NULL, type = "message")
    close (con, type = "wt")
}

parse_all_msgs <- function (f) {
    x <- readLines (f)
    x <- x [which (!(x == "" | duplicated (x)))]

    # some errors and warnings have different whitespace so:
    x <- x [which (!duplicated (gsub ("\\s+", "", x)))]

    ret <- NULL

    index <- grep ("Warning\\:|^Error", x)
    if (length (index) > 0) {
        for (i in index) {
            colon1 <- regexpr ("\\:", x [i])
            content <- gsub ("^\\s?", "",
                             substring (x [i], colon1 + 1, nchar (x [i])))
            type <- c ("warning", "error") [grepl ("^Error", x [i]) + 1]

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
