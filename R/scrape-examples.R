# no export fns here

get_all_examples <- function (package, is_source, exclude = NULL) {

    fns <- get_pkg_functions (package)
    if (!is.null (exclude))
        fns <- fns [which (!fns %in% exclude)]

    exs <- list ()
    for (i in seq (fns)) {
        fn <- fns [i]
        exi <- get_fn_exs (package, fn, is_source = is_source)
        if (!is.null (exi)) {
            exs [[length (exs) + 1]] <- exi
            names (exs) [length (exs)] <- fns [i]
        }
    }

    # reduce only to examples from functions, not from data sets
    ex_classes <- vapply (names (exs), function (i)
                          class (get (i)) [1], character (1))
    exs <- exs [which (ex_classes == "function")]

    not_null <- vapply (exs, function (i) length (i) > 0, logical (1))
    ret <- exs [not_null]

    # remove any enclosing brackets from any example lines, and also replace any
    # single quotes with esacped double versions (because yaml::yaml.load fails
    # on the former)
    ret <- lapply (ret, function (i) {
                       lapply (i, function (j) {
                                   index <- grep ("^\\(", j)
                                   if (length (index) > 0) {
                                       j [index] <-
                                           gsub ("\\)$", "",
                                                 gsub ("^\\(", "", j [index]))
                                   }
                                   j <- gsub ("'", "\"", j, fixed = TRUE)
                                   return (j)   })
        })


    return (ret)
}

get_fn_exs <- function (pkg, fn, rm_seed = TRUE, exclude_not_run = TRUE,
                        is_source = FALSE) {
    

    ex <- get_example_lines (pkg, fn)

    if (length (ex) == 0)
        return (NULL)

    # remove comments and empty lines
    ex <- gsub ("\\#.*$", "", ex)
    ex <- ex [which (!grepl ("^\\s?$", ex))]

    if (any (grepl ("^### \\*\\* Examples", ex)))
        ex <- ex [-(1:grep ("^### \\*\\* Examples", ex))]

    if (exclude_not_run) {
        if (!is_source) {
            nr <- grep ("^## Not run:", ex)
            while (length (nr) > 0) {
                nr_end <- grep ("^## End\\(Not run\\)", ex)
                if (length (nr_end) == 0)
                    nr_end <- nr
                ex <- ex [-(nr [1]:nr_end [1])]
                nr <- grep ("^## Not run:", ex)
            }
            # Examples may also have "No test" and these have to be removed
            nt <- grep ("^## No test:", ex)
            while (length (nt) > 0) {
                    nt_end <- grep ("^## End\\(No test\\)", ex)
                    ex <- ex [-(nt [1]:nt_end [1])]
                    nt <- grep ("^## No test:", ex)
            }
        } else {
            nr <- grep ("^\\\\dontrun\\{", ex)
            while (length (nr) > 0) {
                nr_end <- match_curlies (ex [nr:length (ex)])
                ex <- ex [-(nr [1] + 0:nr_end)]
                nr <- grep ("^\\\\dontrun\\{", ex)
            }
            nt <- grep ("^\\\\donttest\\{", ex)
            while (length (nt) > 0) {
                nt_end <- match_curlies (ex [nt:length (ex)])
                ex <- ex [-(nt [1] + 0:nt_end)]
                nt <- grep ("^\\\\donttest\\{", ex)
            }
        }
    }

    if (is_source) { # rm any roxygen2 auto-generated lines
        index <- grep ("^%", ex)
        if (length (index) > 0)
            ex <- ex [-index]
    }

    if (length (ex) == 0)
        return (NULL)

    ex <- join_at_operators (ex)
    ex <- parse_expressions (ex)
    ex <- match_brackets (ex)
    if (any (grepl ("\\{", ex)))
        ex <- match_brackets (ex, curly = TRUE)
    ex <- merge_piped_lines (ex)
    ex <- merge_fn_defs (ex)
    ex <- single_clause (ex)
    ex <- join_function_lines (ex)

    # find and remove any lines for which first function call is some kind of
    # "plot" or "summary"
    plotlines <- vapply (ex, function (i) {
                             i <- gsub ("\\%", "%", i, fixed = TRUE)
                             p <- utils::getParseData (parse (text = i))
                             s <- which (p$token == "SYMBOL_FUNCTION_CALL")
                             ret <- FALSE
                             if (length (s) > 0)
                                 ret <- grepl ("plot|summary", p$text [s [1]])
                             return (ret)   }, logical (1), USE.NAMES = FALSE)
    if (any (plotlines))
        ex <- ex [-which (plotlines)]

    # find all points of function calls:
    pkg_name <- get_package_name (pkg)
    fns <- ls (paste0 ("package:", pkg_name))
    dispatches <- dispatched_fns (pkg_name)
    is_dispatch <- FALSE
    if (!is.null (dispatches)) {
        fns <- c (fns, gsub ("\\..*$", "", dispatches))
        if (fn %in% dispatches) {
            fn <- c (fn, gsub ("\\..*$", "", dispatches))
            is_dispatch <- TRUE
        }
    }

    fn_calls <- do.call (c, lapply (fns, function (i) grep (i, ex)))
    fn_calls <- sort (unique (fn_calls))
    # reduce to only final calls in a sequence
    index <- which (c (2, diff (fn_calls)) == 1)
    if (length (index) > 0)
        fn_calls <- fn_calls [-(index - 1)]
    # remove any plot or summary calls
    #index <- grepl ("^plot|^summary", ex [fn_calls])
    #index <- grepl ("plot|^summary", ex [fn_calls])
    #if (any (index))
    #    fn_calls <- fn_calls [-(which (index))]

    if (length (fn_calls) == 0)
        return (NULL)

    index <- rep (seq (length (fn_calls)),
                  times = c (fn_calls [1], diff (fn_calls)))
    exs <- split (ex [seq (length (index))], f = as.factor (index))
    # rm extraneous lines
    ret <- lapply (exs, function (i) {
                       i <- i [which (!i == "")]
                       i [!grepl ("^\\#|^plot|^summary|^print", i)] })
    
    if (rm_seed) {
        ret <- lapply (ret, function (i) {
                           i [!grepl ("^set.seed", i)]  })
    }

    # concatenate any example lines which do not call the actual function or
    # it's aliases into effective preprocessing lines for subsequent function
    # calls.
    aliases <- paste0 (get_fn_aliases (pkg, fn [1]), collapse = "|")
    index <- vapply (ret, function (i) any (grepl (aliases, i)), logical (1))
    if (length (fn) == 2) { # when it's a dispatch method
        index2 <- vapply (ret, function (i) any (grepl (fn [2], i)), logical (1))
        index <- index | index2
    }

    if (!any (index))
        return (NULL) # There are no calls to fn

    # can do the following via split, but it's a lot more convoluted than this
    # loop. Start by removing any trailing FALSE values
    ret <- ret [1:max (which (index))]
    index <- index [1:max (which (index))]
    for (i in rev (seq_along (index))) {
        if (index [i])
            here <- i
        else {
            ret [[here]] <- c (ret [[i]], ret [[here]])
        }
    }
    ret <- ret [which (index)]

    ret <- lapply (ret, function (i) {
                       attr (i, "is_dispatch") <- is_dispatch
                       return (i)   })

    return (ret)
}


get_example_lines <- function (package, fn) {
    ex <- NULL

    pkg_name <- get_package_name (package)

    if (!pkg_is_source (package)) {
        # example called for function which have no help file trigger warnings
        ex <- tryCatch (utils::example (eval (substitute (fn)),
                                        package = package,
                                        character.only = TRUE,
                                        give.lines = TRUE,
                                        lib.loc = .libPaths ()),
                        warning = function (w) NULL)

    } else {
        f <- file.path (package, "man", paste0 (fn, ".Rd"))
        ex <- readLines (f, warn = FALSE)
        ex_start <- grep ("^\\\\examples\\{", ex)
        if (length (ex_start) > 0) {
            ex <- ex [ex_start:length (ex)]

            ex_end <- match_curlies (ex)
            ex <- ex [2:ex_end]

            doload <- FALSE
            if (!paste0 ("package:", pkg_name) %in% search ()) {
                doload <- TRUE
            } else {
                v0 <- utils::packageVersion (pkg_name)
                desc <- file.path (package, "DESCRIPTION")
                d <-readLines (desc)
                v <- gsub ("^Version:\\s+", "", d [grep ("^Version:", d)])
                if (v > v0)
                    doload <- TRUE
            }
            if (doload)
                devtools::load_all (package, export_all = FALSE)
        } else {
            ex <- NULL
        }
    }

    return (ex)
}

get_package_name <- function (package) {
    pkg_name <- NULL

    if (!pkg_is_source (package)) {
        pkg_name <- package
    } else {
        desc <- readLines (file.path (package, "DESCRIPTION"))
        p <- grep ("^Package\\:", desc)
        pkg_name <- gsub ("Package:\\s?", "", desc [p])
    }

    return (pkg_name)
}

# find which functions are method dispatches, so grep can be done on the method
# and not the class
dispatched_fns <- function (pkg_name) {
    h <- utils::help (package = eval (substitute (pkg_name)), help_type = "text")
    fns <- h$info [[2]]
    fns <- gsub ("\\s.*", "", fns [which (!grepl ("^\\s", fns))])
    # reduce only to exported functions, methods, or data sets
    fns <- fns [fns %in% ls (paste0 ("package:", pkg_name))]
    # Then reduce only to functions:
    fn_classes <- vapply (fns, function (i) class (get (i)) [1], character (1))
    fns <- fns [which (fn_classes == "function")]

    index <- grep ("\\.", fns)
    classes <- gsub (".*\\.", "", fns [index])
    dispatch <- rep (FALSE, length (classes))
    for (i in seq_along (index)) {
        h <- utils::help (topic = classes [i],
                          package = eval (substitute (pkg_name)),
                          help_type = "text")
        if (length (nchar (h)) > 0)
            dispatch [i] <- TRUE
    }

    res <- NULL
    if (any (dispatch))
        res <- fns [index] [which (dispatch)]

    return (res)
}

# join multiple lines connected by operators (*, /, -, +)
join_at_operators <- function (x) {
    operators <- c ("\\+", "\\-", "\\*", "\\/",
                    "\\^", "\\*\\*",
                    "%%", "%/%",
                    "<", "<\\=", ">", ">\\=",
                    "\\=\\=", "\\!\\=", "\\|", "&")
    operators <- paste0 ("(", paste0 (operators, collapse = "|"), ")\\s*$")
    index <- rev (grep (operators, x))
    for (i in index) {
        x [i] <- paste0 (x [i], x [i + 1], collapse = " ")
        x <- x [-(i + 1)]
    }

    return (x)
}

merge_piped_lines <- function (x) {
    x <- gsub ("\\s$", "", x)
    index <- rev (grep ("%>%\\s?$|\\\\%>\\\\%\\s?$", x))
    for (i in index) {
        x [i] <- gsub ("\\s+", " ", paste0 (x [i:(i+1)], collapse = " "))
        x <- x [-(i + 1)]
        if (any (grepl ("\\\\%", x))) {
            x <- gsub ("\\\\%", "%", x)
        }
    }
    return (x)
}

merge_fn_defs <- function (x) {
    if (any (grepl ("function(\\s?)\\(", x))) {
        br_open <- lapply (gregexpr ("\\{", x), function (i) as.integer (i [i >= 0]))
        br_closed <- lapply (gregexpr ("\\}", x), function (i) as.integer (i [i >= 0]))

        br_open2 <- br_closed2 <- NULL
        for (i in seq (br_open))
            br_open2 <- c (br_open2, rep (i, length (br_open [[i]])))
        for (i in seq (br_closed))
            br_closed2 <- c (br_closed2, rep (i, length (br_closed [[i]])))

        br_open <- rev (br_open2)
        br_closed <- rev (br_closed2)
        index <- which (br_open != br_closed)
        br_open <- br_open [index]
        br_closed <- br_closed [index]
        for (i in seq_along (br_open)) {
            x1 <- x2 <- NULL
            if (br_open [i] > 1)
                x1 <- x [seq (br_open [i] - 1)]
            if (br_closed [i] < length (x))
                x2 <- x [(br_closed [i] + 1):length (x)]
            # NOTE: The following presumes that any functions defined in
            # examples and which span multiple lines are defined such that
            # fn <- function () { # line ends here
            # <some stuff>
            # } # function def ends here
            # This parsing will likely fail on cases like:
            # fn <- function () { a = 1,
            # b = 2
            # }
            # in which the lines between the braces aren't cleanly separated
            x <- c (x1,
                    paste0 (x [br_open [i]],
                            paste0 (x [(br_open [i] + 1):(br_closed [i] - 1)],
                                    collapse = ";"),
                            x [br_closed [i]]),
                    x2)
        }
    }
    return (x)
}

single_clause <- function (x) {
    # match (if|for) with anything after and NOT (if|for) with "{"
    index <- which (grepl ("^(if|for)\\s?\\(.*\\)\\s?", x) &
                    !grepl ("^(if|for)\\s?\\(.*\\)\\s?\\{", x))
    if (length (index) > 0) {
        br1 <- gregexpr ("\\(", x [index])
        br2 <- gregexpr ("\\)", x [index])
        br_end <- grep (NA, length (index))
        for (i in seq_along (br1)) {
            brseq <- nested_sequences (br1 [[i]], br2 [[i]])
            br_end [i] <- brseq$br_closed [1]
        }
        xcut <- substring (x [index], br_end + 1, nchar (x [index]))
        index <- index [grep ("^\\s*$", xcut)]
        if (length (index) > 0) {
            x [index] <- paste0 (x [index], x [index + 1], collapse = " ")
            x <- x [-(index + 1)]
        }
    }

    return (x)
}
