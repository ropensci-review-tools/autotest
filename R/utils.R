
catch_all_msgs <- function (f, this_fn) {
    con <- file (f, "wt")
    sink (file = con, type = "message")
    res <- eval (call (this_fn))
    closeAllConnections ()
}

parse_all_msgs <- function (f) {
    x <- readLines (f)

    ret <- NULL

    # warnings are always 2 lines; errors and messages only ever 1 line
    index <- grep ("Warning message:", x)
    if (length (index) > 0) {
        index_msg <- index + 1
        ret <- data.frame (type = rep ("warning", length (index)),
                           content = x [index_msg],
                           stringsAsFactors = FALSE)
        x <- x [-c (index, index_msg)]
    }

    index <- grep ("^Error", x)
    if (length (index) > 0) {
        for (i in index) {
            colon1 <- regexpr ("\\:", x [i])
            ret <- rbind (ret,
                          data.frame (type = "error",
                                      content = gsub ("^\\s?", "",
                                                      substring (x [i], colon1 + 1, nchar (x [i]))),
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
