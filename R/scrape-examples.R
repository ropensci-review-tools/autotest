# no export fns here

get_all_examples <- function (package,
                              is_source,
                              exclude = NULL,
                              quiet = FALSE) {

    if (is_source) {
        if (!is_pkg_same (package))
            memoise::forget (m_get_pkg_functions)
    }
    fns <- m_get_pkg_functions (package)

    if (!is.null (exclude))
        fns <- fns [which (!fns %in% exclude)]
    topics <- m_fns_to_topics (fns, package)

    index <- which (!duplicated (topics$topic))
    topics <- topics [index, ]
    fns <- fns [index]

    topic <- topics$topic
    rdnames <- gsub ("\\.Rd$", "", topics$name)

    if (length (rdnames) < 10)
        quiet <- TRUE
    if (!quiet) {
        message (cli::col_green (cli::symbol$star,
                                 " Extracting example code from ",
                                 length (rdnames),
                                 " .Rd files"))
        pb <- utils::txtProgressBar (style = 3)
    }

    if (!is_source)
        package <- basename (package)

    exs <- list ()
    for (i in seq (rdnames)) {
        exi <- get_fn_exs (package, rdnames [i], topic [i],
                           is_source = is_source)
        if (!is.null (exi)) {
            attr (exi, "Rdname") <- rdnames [i]
            exs [[length (exs) + 1]] <- exi
            names (exs) [length (exs)] <- fns [i]
        }

        if (!quiet)
            utils::setTxtProgressBar (pb, i / length (rdnames))
    }
    if (!quiet) {
        close (pb)
        message (cli::col_green (cli::symbol$tick,
                                 " Extracted example code"))
    }

    not_null <- vapply (exs, function (i) length (i) > 0, logical (1))
    ret <- exs [not_null]

    ret <- rm_enclosing_brackets (ret)

    return (ret)
}

get_fn_exs <- function (package, rd_name, topic, rm_seed = TRUE,
                        exclude_not_run = TRUE, is_source = FALSE) {

    ex <- get_example_lines (package, rd_name)

    if (length (ex) == 0)
        return (NULL)

    ex <- remove_comments (ex)
    ex <- preprocess_example_lines (ex, exclude_not_run, is_source)

    if (length (ex) == 0)
        return (NULL)

    ex <- clean_example_lines (ex)
    ex <- transform_single_quotes (ex)

    fns <- find_fn_call_points (topic, package)
    is_dispatch <- attr (fns, "is_dispatch")

    fn_calls <- process_fn_calls (fns, ex)
    if (length (fn_calls) == 0)
        return (NULL)

    exs <- split_ex_by_fn_calls (ex, fn_calls)

    exs <- rm_seed_calls (exs, rm_seed)

    # concatenate any example lines which do not call the actual function or
    # it's aliases into effective preprocessing lines for subsequent function
    # calls.
    aliases <- get_fn_aliases (package, rd_name)
    aliases <- gsub ("\\.", "\\\\.", aliases)
    aliases <- gsub ("\\[", "\\\\[", aliases) # sub-set only ever has "["
    aliases <- gsub ("\\_", "\\\\_", aliases)
    aliases <- paste0 (aliases, collapse = "|")
    index <- vapply (exs, function (i) any (grepl (aliases, i)), logical (1))
    if (length (fns) == 2) { # when it's a dispatch method
        index2 <- vapply (exs, function (i)
                          any (grepl (fns [2], i)), logical (1))
        index <- index | index2
    }

    if (!any (index))
        return (NULL) # There are no calls to fn

    # can do the following via split, but it's a lot more convoluted than this
    # loop. Start by removing any trailing FALSE values
    exs <- exs [1:max (which (index))]
    index <- index [1:max (which (index))]
    for (i in rev (seq_along (index))) {
        if (index [i])
            here <- i
        else {
            exs [[here]] <- c (exs [[i]], exs [[here]])
        }
    }
    exs <- exs [which (index)]

    exs <- lapply (exs, function (i) {
                       attr (i, "is_dispatch") <- is_dispatch
                       return (i)   })

    return (exs)
}


get_example_lines <- function (package, rd_name) {

    ex <- NULL

    if (!pkg_is_source (package)) {

        ex <- get_example_lines_installed (package, rd_name)

    } else {

        ex <- get_example_lines_source (package, rd_name)
        if (!is.null (ex))
            load_all_if_needed (package)
    }

    # readLines auto-parses "\" to "\\\\", which then needs to be reverted to
    # the usual escaped-version of "\\":
    ex <- gsub ("\\\\", "\\", ex, fixed = TRUE)

    return (ex)
}

get_example_lines_installed <- function (package, rd_name) {

    if (dir.exists (package))
        package <- utils::tail (strsplit (package, .Platform$file.sep) [[1]], 1)

    libloc <- pkg_lib_path (package, root = TRUE)

    # example called for function which have no help file trigger warnings
    tryCatch (utils::example (eval (substitute (rd_name)),
                              package = package,
                              character.only = TRUE,
                              give.lines = TRUE,
                              lib.loc = libloc),
              warning = function (w) NULL)
}

get_example_lines_source <- function (package, rd_name) {

    f <- file.path (package, "man", paste0 (rd_name, ".Rd"))
    ex <- readLines (f, warn = FALSE)
    ex_start <- grep ("^\\\\examples\\{", ex)

    if (length (ex_start) > 0) {

        ex <- ex [ex_start:length (ex)]

        ex_end <- match_curlies (ex)
        ex <- ex [2:ex_end]

    } else {

        ex <- NULL
    }

    return (ex)
}

#' Load a local source package via devtools::load_all if either not loaded, or
#' if local version is > installed version.
#' @param package Full path to local source package
#' @return Nothing
#' @noRd
load_all_if_needed <- function (package) {

    pkg_name <- get_package_name (package)

    doload <- FALSE
    if (!paste0 ("package:", pkg_name) %in% search ()) {
        doload <- TRUE
    } else {
        v0 <- utils::packageVersion (pkg_name)
        desc <- file.path (package, "DESCRIPTION")
        v <- read.dcf (desc, "Version")
        if (v > v0)
            doload <- TRUE
    }
    if (doload) {
        requireNamespace ("devtools")
        devtools::load_all (package, export_all = FALSE)
    }
}

get_package_name <- function (package) {
    pkg_name <- NULL

    if (!pkg_is_source (package)) {
        pkg_name <- basename (package)
    } else {
        desc <- file.path (package, "DESCRIPTION")
        pkg_name <- read.dcf (desc, "Package")
    }

    return (pkg_name)
}

#' Remove comments from code lines
#'
#' Comments are only removed if they are not within quotations.
#' This requires first generating sequences of character indices within
#' quotations.
#'
#' @param ex Examples lines from one .Rd file
#' @return ex with all trailing comments removed
#' @noRd
remove_comments <- function (ex) {

    qts <- gregexpr ("\"|\'", ex)
    ln_nums <- lapply (seq_along (qts), function (i)
                       rep (i, length (qts [[i]])))
    qts <- cbind (unlist (ln_nums), unlist (qts))
    qts <- qts [which (qts [, 2] > 0), ]
    index <- seq (nrow (qts) / 2) * 2 - 1
    qts <- cbind (qts [index, , drop = FALSE],
                  qts [index + 1, , drop = FALSE])
    # split sequences which extend across multiple lines:
    index <- which (qts [, 1] != qts [, 3])
    if (length (index) > 0) {
        reps <- rep (1, nrow (qts))
        reps [index] <- 2
        reps <- rep (seq (nrow (qts)), times = reps)
        qts <- qts [reps, ]
        index <- which (duplicated (qts))
        qts [index - 1, 3] <- qts [index - 1, 1]
        qts [index - 1, 4] <- nchar (ex [qts [index - 1, 1]])
        qts [index, 1] <- qts [index, 3]
        qts [index, 2] <- 1
    }

    linenums <- apply (qts, 1, function (i) i [1])

    qts <- apply (qts, 1, function (i) as.vector (seq (i [2], i [4])))
    names (qts) <- linenums

    cmt <- gregexpr ("\\#", ex)
    cmt <- lapply (seq_along (cmt), function (i) {
                   if (i %in% names (qts)) {
                       qts_i <- sort (unname (unlist (qts [names (qts) == i])))
                       cmt [[i]] <- cmt [[i]] [which (!cmt [[i]] %in% qts_i)]
                       if (length (cmt [[i]]) == 0)
                           cmt [[i]] <- -1
                   }
                   return (cmt [[i]])
                  })
    ex <- vapply (seq_along (cmt), function (i) {
                  if (cmt [[i]] [1] > 0)
                      ex [i] <- strsplit (ex [1], 1, cmt [[i]] [1] - 1) [[1]] [1]
                  return (ex [i])
                  }, character (1))

    return (ex)
}

#' A few preprocessing cleaning operations for example code lines
#'
#' @param ex Example lines from one '.Rd' file
#' @return Cleaned version of ex
#' @noRd
preprocess_example_lines <- function (ex, exclude_not_run, is_source) {

    ex <- ex [which (!grepl ("^\\s?$", ex))]

    # Cut Rd lines down to example code only
    if (any (grepl ("^### \\*\\* Examples", ex)))
        ex <- ex [- (1:grep ("^### \\*\\* Examples", ex))]

    for (dontrun in c (TRUE, FALSE))
        ex <- rm_dontrun_lines (ex, is_source = is_source, dontrun = dontrun,
                                exclude_not_run = exclude_not_run)

    if (is_source) { # rm any roxygen2 auto-generated lines
        index <- grep ("^%", ex)
        if (length (index) > 0)
            ex <- ex [-index]
    }

    return (ex)
}

clean_example_lines <- function (ex) {

    ex <- join_at_operators (ex)
    ex <- parse_expressions (ex)
    ex <- match_brackets (ex)
    if (any (grepl ("\\{", ex)))
        ex <- match_brackets (ex, curly = TRUE)
    ex <- merge_piped_lines (ex)
    ex <- merge_fn_defs (ex)
    ex <- single_clause (ex)
    for (double_quote in c (TRUE, FALSE))
        ex <- multi_line_quotes (ex, double_quote = double_quote)
    ex <- join_function_lines (ex)
    ex <- rm_not_parseable (ex)
    ex <- rm_plot_lines (ex)

    return (ex)
}

#' find all points of function calls for a given Rd topic
#' @noRd
find_fn_call_points <- function (topic, package) {

    fns <- topic_to_fns (topic, package = package)

    dispatches <- dispatched_fns (package)
    is_dispatch <- any (fns %in% dispatches)

    if (is_dispatch) {
        fns_short <- vapply (fns, function (i) strsplit (i, "\\.") [[1]] [1],
                             character (1), USE.NAMES = FALSE)
        fns <- fns_short [which (!duplicated (fns_short))]
    }
    attr (fns, "is_dispatch") <- is_dispatch

    return (fns)
}

#' process function calls obtained from 'find_fn_call_points'
#'
#' @param fns Output of 'find_fn_call_points'
#' @param ex Cleaned lines of example code
#' @return Function call points reduced to unique values of only those functions
#' contained in examples
#' @noRd
process_fn_calls <- function (fns, ex) {

    # rm select functions:
    fns <- fns [!grepl ("^\\[", fns)]
    fns <- do.call (c, lapply (fns, function (i) grep (i, ex)))
    fns <- sort (unique (fns))

    # reduce to only final calls in a sequence
    index <- which (c (2, diff (fns)) == 1)
    if (length (index) > 0)
        fns <- fns [- (index - 1)]

    # remove any plot or summary calls
    #index <- grepl ("^plot|^summary", ex [fns])
    #index <- grepl ("plot|^summary", ex [fns])
    #if (any (index))
    #    fns <- fns [-(which (index))]

    return (fns)
}

#' Split lines of one example into list items separated by each call to target
#' function
#'
#' @param ex Complete code for one documented example
#' @param fn_calls Points at which focal function is called
#' @return List of code lines obtained by splitting ex and each fn_call point
#' @noRd
split_ex_by_fn_calls <- function (ex, fn_calls) {
    index <- rep (seq (length (fn_calls)),
                  times = c (fn_calls [1], diff (fn_calls)))
    split (ex [seq_along (index)], f = as.factor (index))
}

rm_seed_calls <- function (exs, rm_seed) {
    if (rm_seed) {
        exs <- lapply (exs, function (i) i [!grepl ("^set.seed", i)])
    }
    return (exs)
}

# find which functions are method dispatches, so grep can be done on the method
# and not the class
dispatched_fns <- function (package) {
    res <- m_fns_to_topics (package = package)
    index <- grep ("[[:alpha:]]?\\.[[:alpha:]]", res$alias)
    if (length (index) == 0)
        return (NULL)

    fns <- res$alias [index]
    fns_short <- vapply (fns, function (i) strsplit (i, "\\.") [[1]] [1],
                         character (1), USE.NAMES = FALSE)
    index <- which (vapply (fns_short, function (i)
                            any (grepl (i, res$alias, fixed = TRUE)),
                            logical (1), USE.NAMES = FALSE))
    return (fns_short [index])
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
        x <- x [- (i + 1)]
    }

    return (x)
}

merge_piped_lines <- function (x) {
    x <- gsub ("\\s$", "", x)
    index <- rev (grep ("%>%\\s?$|\\\\%>\\\\%\\s?$", x))
    for (i in index) {
        x [i] <- gsub ("\\s+", " ", paste0 (x [i:(i + 1)], collapse = " "))
        x <- x [- (i + 1)]
        if (any (grepl ("\\\\%", x))) {
            x <- gsub ("\\\\%", "%", x)
        }
    }
    return (x)
}

merge_fn_defs <- function (x) {
    if (any (grepl ("function(\\s?)\\(", x))) {
        br_open <- lapply (gregexpr ("\\{", x), function (i)
                           as.integer (i [i >= 0]))
        br_closed <- lapply (gregexpr ("\\}", x), function (i)
                             as.integer (i [i >= 0]))

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

# match (if|for) with anything after and NOT (if|for) with "{"
single_clause <- function (x) {
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
            x <- x [- (index + 1)]
        }
    }

    return (x)
}

# join lines which break within a single quotation
# example: curl::send_mail
# example of non-matched quotes: stats::influence.measure
multi_line_quotes <- function (x, double_quote = TRUE) {

    if (double_quote)
        q <- "\""
    else
        q <- "\'"

    index <- vapply (gregexpr (q, x), function (i)
                     length (which (i > 0)), integer (1))
    # check that single quotes are not possessive apostrophes
    chk <- grepl ("[[:alpha:]]\'s", x [which (index > 0)])
    index <- index [which (!chk)]

    index <- rev (which (index %% 2 != 0)) # unmatched quotes
    if (length (index) > 1) {
        index2 <- rep (seq (length (index) / 2), each = 2)
        # un-reverse the individual entries so they are increasing pairs of
        # [start, end] of quotes
        index <- lapply (split (index, f = factor (index2)),
                         function (i) rev (i))
        for (i in index) {
            x [i [1]] <- paste0 (x [i [1]:i [2]], collapse = " ")
            x <- x [- ((i [1] + 1):i [2])]
        }
    }
    return (x)
}

# remove all code between `dontrun` and `donttest` clauses if `exclude_not_run`,
# otherwise keep those lines but remove the `dontrun` and `donttest` statements.
# `dontrun = FALSE` removes "donttest" lines.
rm_dontrun_lines <- function (x, is_source = TRUE, dontrun = TRUE,
                              exclude_not_run = TRUE) {
    if (is_source) {
        txt <- ifelse (dontrun, "\\\\dontrun\\s?\\{",
                       "\\\\donttest\\s?\\{")
        n <- grep (txt, x)
        while (length (n) > 0) {
            n_end <- n [1] + match_curlies (x [n [1]:length (x)])
            if (exclude_not_run)
                x <- x [- (n [1]:n_end)]
            else
                x <- x [-c (n [1], n_end)]
            n <- grep (txt, x)
        }
    } else {
        txt_start <- ifelse (dontrun, "## Not run:", "## No test:")
        txt_end <- ifelse (dontrun, "##\\s+End\\s+\\(Not\\s+run\\)",
                           "##\\s+End\\s+\\(No\\s+test\\)")
        n <- grep (txt_start, x)
        while (length (n) > 0) {
            n_end <- grep (txt_end, x)
            if (length (n_end) == 0)
                n_end <- n
            if (exclude_not_run)
                x <- x [- (n [1]:n_end [1])]
            else
                x <- x [-c (n [1], n_end)]
            n <- grep (txt_start, x)
        }
    }

    return (x)
}

#' non-parseable expressions are simply defined as those which fail with `eval`.
#' These are caught via `tryCatch`, but that means expressions can not
#' themselves be wrapped in `try`, because `try` within `tryCatch` does not work
#' as expected.
#' @noRd
rm_not_parseable <- function (x) {

    index <- grep ("^\\s?try\\s?\\(", x)
    if (any (index)) {
        x [index] <- gsub ("^\\s?try\\s?\\(|\\)$", "", x [index])
    }

    dev <- options()$"device"
    options (device = NULL) # suppress plot output

    this_env <- new.env ()

    junk <- utils::capture.output (
                suppressMessages (
                suppressWarnings (
        parseable <- vapply (x, function (i) {
                                 i <- gsub ("\\%", "%", i, fixed = TRUE)
                                 p <- tryCatch (
                                            eval (parse (text = i),
                                                  envir = this_env),
                                            error = function (err) NULL)
                                 return (!is.null (p))
                               }, logical (1), USE.NAMES = FALSE)
    ))) # end capture.output and suppressMessages/Warnings

    options (device = dev)

    return (x [which (parseable)])
}


# find and remove any lines for which first function call is some kind of
# "plot" or "summary"
rm_plot_lines <- function (x) {
    plotlines <- vapply (x, function (i) {
                             i <- gsub ("\\%", "%", i, fixed = TRUE)
                             p <- utils::getParseData (parse (text = i))
                             s <- which (p$token == "SYMBOL_FUNCTION_CALL" &
                                         grepl ("plot|summary|print", p$text))
                             ret <- FALSE
                             if (length (s) > 0) {
                                 ret <- TRUE
                                 # only include if its not part of another fn
                                 if (any (p$token == "FUNCTION")) {
                                     ret <- which (p$token ==
                                                   "FUNCTION") [1] > s [1]
                                 }
                             }
                             return (ret)   },

                             logical (1),
                             USE.NAMES = FALSE)
    if (any (plotlines))
        x <- x [-which (plotlines)]

    # rm other extraneous lines, repeating plot etc
    index <- grepl ("^\\#|^plot|^summary|^print", x)
    if (any (index))
        x <- x [!index]

    return (x)
}

#' Remove enclosing brackets from example lines
#'
#' @param Extracted and cleaned list of examples for each function in a package.
#' @noRd
rm_enclosing_brackets <- function (x) {

    lapply (x, function (i) {
                a <- attributes (i)
                out <- lapply (i, function (j) {
                                   index <- grep ("^\\(", j)
                                   if (length (index) > 0) {
                                       j [index] <-
                                           gsub ("\\)$", "",
                                                 gsub ("^\\(", "", j [index]))
                                   }
                                   return (j)   })
                attributes (out) <- a
                return (out) })
}

#' yaml does not parse single quotes, so these are converted to double quotes,
#' but this requires first checking whether they are embedded within double
#' quotes in which case they are simply removed. (Example ?D under "higher
#' derivatives"). Once that's been done, the remainder are converted.
#' @noRd
transform_single_quotes <- function (x) {
    res <- vapply (x, function (i) {
        quotes2 <- gregexpr ("\"", i) [[1]]
        if (quotes2 [1] < 1)
            return (i)

        index <- 2 * seq (length (quotes2) / 2)
        index <- cbind (quotes2 [index - 1], quotes2 [index])
        index <- apply (index, 1, function (i) i [1]:i [2])
        # apply returns a matrix if all i[1]:i[2] sequences are
        # same length, otherwise it is a list
        if (is.list (index)) {
            index <- do.call (c, index)
        } else {
            index <- as.vector (index)
        }

        quotes1 <- gregexpr ("\'", i) [[1]]
        quotes1 <- quotes1 [which (quotes1 %in% index)]

        ret <- i

        if (length (quotes1) > 0) {
            index <- seq (nchar (i)) [which (!seq (nchar (i)) %in% quotes1)]

            qts <- sort (c (1, quotes1 - 1, quotes1 + 1, nchar (i)))
            qts <- matrix (qts, ncol = 2, byrow = TRUE)
            txt <- apply (qts, 1, function (j)
                          substring (i, j [1], j [2]))
            ret <- paste0 (txt, collapse = "")
        }

        # final replacement of other single quotes not enclosed in double
        ret <- gsub ("'", "\"", ret, fixed = TRUE)

        return (ret)
                           },
        character (1),
        USE.NAMES = FALSE)

    return (res)
}
