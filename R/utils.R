
# A `tryCatch` call modified to catch all messages, warnings, and errors.
# `tryCatch` can not normally do this because it only catches the first
# condition. This code is similar to the code of `demo(error.catching)`, but
# instead of assigning a global variable, it writes all messages to a local file
# and then parses the result with a separate function. See also the
# `tryCatchLog` package for further inspiration.

log_all_msgs <- function (con, this_fn, params = NULL) {

    x <- tryCatch (withCallingHandlers (
                                        if (is.null (params))
                                           eval (call (this_fn))
                                        else
                                            do.call (this_fn, params),
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
                if (grepl (ti, substring (x [i], 1, colon1), ignore.case = TRUE))
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
        ret <- rbind (ret,
                      report_object (type = rep ("message", length (x)),
                                     fn_name = rep (NA_character_, length (x)),
                                     parameter = rep (NA_character_, length (x)),
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
                                         operation = operation,
                                         content = paste0 (txt,
                                                           msgs$content [i])))
        }
    }
    return (res)
}

get_pkg_functions <- function (package) {
    if (pkg_is_source (package)) {
        fns <- gsub (".Rd$", "",
                     list.files (file.path (package, "man"), pattern = ".Rd$"))
        fn_classes <- vapply (fns, function (i)
                              tryCatch (class (get (i)) [1],
                                        error = function (e) NA_character_),
                              character (1))
        fns <- fns [grep ("function", fn_classes)]
    } else {
        fns <- ls (paste0 ("package:", package))
        fn_classes <- vapply (fns, function (i) class (get (i)) [1], character (1))
        fns <- fns [grep ("[Ff]unction|standardGeneric", fn_classes)]
    }

    other_fns <- fns_from_other_pkgs (package)

    return (fns [which (!fns %in% other_fns)])
}

fns_without_examples <- function (package, exs) {
    fns_with_exs <- unique (names (exs))
    aliases <- unlist (lapply (fns_with_exs, function (i)
                               get_fn_aliases (pkg = package,
                                               fn_name = i)))
    fns_with_exs <- unique (aliases)

    fns <- get_pkg_functions (package)

    fns <- fns [which (!fns %in% c ("%>%", fns_with_exs))]

    # Note that the vapply variable has to be "fn" for the `eval(substitute(.))`
    # call in `get_example_lines` to grab the appropriate object.
    count <- vapply (fns, function (fn)
                         length (get_example_lines (package, fn = fn)),
                     integer (1))

    fns <- fns [which (count == 0)]
    if (length (fns) == 0)
        return (NULL)

    pkg_name <- package
    if (pkg_is_source (package)) {
        desc <- readLines (file.path (package, "DESCRIPTION"))
        pkg_name <- gsub ("Package:\\s?", "", desc [grep ("^Package\\:", desc)])
    }

    return (fns [which (fns %in% ls (paste0 ("package:", pkg_name)))])
}

fns_from_other_pkgs <- function (package) {
    if (pkg_is_source (package)) {
        fp <- file.path (package)
    } else {
        fp <- file.path (R.home (), "library", package)
    }
    namespace_file <- file.path (fp, "NAMESPACE")
    if (!file.exists (namespace_file))
        stop ("There is no NAMESPACE file at [", fp, "]")

    ns <- readLines (namespace_file)
    ns <- gsub ("importFrom\\(|\\)$", "", ns [grep ("^importFrom", ns)])
    fns <- vapply (ns, function (i) strsplit (i, split = ",") [[1]] [2],
                   character (1))

    index <- grep ("\"", fns)
    if (length (index) > 0) {
        fns_sub <- gsub ("\"", "", fns [index])
        fns <- c (fns, fns_sub)
    }

    return (fns)
}
