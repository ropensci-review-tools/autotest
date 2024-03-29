
# A `tryCatch` call modified to catch all messages, warnings, and errors.
# `tryCatch` can not normally do this because it only catches the first
# condition. This code is similar to the code of `demo(error.catching)`, but
# instead of assigning a global variable, it writes all messages to a local file
# and then parses the result with a separate function. See also the
# `tryCatchLog` package for further inspiration.

log_all_msgs <- function (con, this_fn, params = NULL) {

    o <- utils::capture.output ({
        en <- new.env ()
        if (grepl (":::", this_fn)) {
            # internal fns can't be called via do.call:
            this_fn <- strsplit (this_fn, ":::") [[1]]
            this_fn <- utils::getFromNamespace (this_fn [2], this_fn [1])
        }
        x <- tryCatch (withCallingHandlers (
                                if (is.null (params))
                                   eval (call (this_fn), envir = en)
                                else
                                    do.call (this_fn, params, quote = TRUE),
                               error = function(e) {
                                   write (toString (e),
                                          con,
                                          append = TRUE)
                               },
                               warning = function(w) {
                                   write (toString (w),
                                          con,
                                          append = TRUE)
                                   invokeRestart("muffleWarning")
                               },
                               message = function (z) {
                                   write (toString (z),
                                          con,
                                          append = TRUE)
                               }),
                       error = function(e) {
                           return ("error detected")
                       })
    }) # end capture.output

    # Next 2 lines write messages to logfile, but not used here
    #if (length (o) > 1 | any (o != "NULL"))
    #    write (toString (o), con, append = TRUE)

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
        rm_lines <- NULL
        for (i in index) {
            rm_lines <- c (rm_lines, i)
            if (grepl ("\\:\\s?$", x [i])) {
                x [i] <- paste0 (x [i], x [i + 1], collapse = " ")
                rm_lines <- c (rm_lines, i + 1)
            }
            colon1 <- regexpr ("\\:", x [i])
            content <- gsub ("^\\s?", "",
                             substring (x [i], colon1 + 1, nchar (x [i])))
            loc <- substring (x [i], 1, colon1 - 1)
            if (grepl ("\\(.*\\)", loc)) {
                ws <- gregexpr ("\\s+", loc) [[1]]
                br <- gregexpr ("\\(.*\\)", loc) [[1]]
                loc <- substring (loc, max (ws [ws < br]) + 1, nchar (loc))
            } else
                loc <- NA_character_
            types <- c ("message", "error", "warning")
            type <- NA_character_
            for (ti in types)
                if (grepl (ti, substring (x [i], 1, colon1),
                           ignore.case = TRUE))
                    type <- ti

            ret <- rbind (ret,
                          report_object (type = type,
                                         fn_name = loc,
                                         parameter = NA_character_,
                                         content = content))
        }
        x <- x [-rm_lines]
    }

    # anything left must be messages
    if (length (x) > 0) {
        nac <- rep (NA_character_, length (x))
        ret <- rbind (ret,
                      report_object (type = rep ("message", length (x)),
                                     fn_name = nac,
                                     parameter = nac,
                                     content = x))
    }

    return (ret)
}

catch_all_msgs <- function (f, this_fn, params = NULL) {

    con <- file (f, "wt")
    suppressMessages (
        x <- log_all_msgs (con, this_fn, params)
        )
    close (con)
    out <- parse_all_msgs (f)
    if (!is.null (out))
        out$fn_name <- this_fn
        #out$fn_name [which (is.na (out$fn_name))] <- this_fn
    return (out)
}

add_msg_output <- function (res, msgs, types = c ("warning", "error"),
                            operation = NULL, addcall = FALSE) {
    if (any (vapply (types, function (i)
                     not_null_and_is (msgs, i), logical (1)))) {

        if (is.null (operation))
            operation <- NA_character_

        index <- which (msgs$type %in% types)
        for (i in index) {
            txt <- NULL
            if (addcall) {
                txt <- paste0 ("Function [",
                               msgs$fn_name [i],
                               "] issued ")
                if (msgs$type [i] == "error")
                    txt <- paste0 (txt, "an Error: ")
                else
                    txt <- paste0 (txt, "a Warning: ")
            }
            res <- rbind (res,
                          report_object (type = msgs$type [i],
                                         fn_name = msgs$fn_name [i],
                                         parameter = msgs$parameter [i],
                                         parameter_type = msgs$parameter_type,
                                         operation = operation,
                                         content = paste0 (txt,
                                                           msgs$content [i])))
        }
    }
    return (res)
}
