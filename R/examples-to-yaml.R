#' examples_to_yaml
#'
#' Convert examples for a specified package, optionally restricted to one or
#' more specified functions, to a list of 'autotest' 'yaml' objects to use to
#' automatically test package.
#'
#' @param package Name of package for which 'yaml' is to be generated.
#' @param functions If specified, names of functions from which examples are to
#' be obtained.
#' @param exclude Names of functions to exclude from 'yaml' template
#' @inheritParams autotest_package
#' @family yaml
#' @export
examples_to_yaml <- function (package = NULL,
                              functions = NULL,
                              exclude = NULL,
                              quiet = FALSE) {

    pkg_name <- preload_package (package)

    exclude <- exclude_functions (package, functions, exclude)

    exs <- get_all_examples (package,
                             pkg_is_source (package),
                             exclude,
                             quiet = quiet)

    ret <- list ()

    if (length (exs) < 10)
        quiet <- TRUE
    if (!quiet) {
        message (cli::col_green (cli::symbol$star,
                                 " Converting ",
                                 length (exs),
                                 " examples to yaml"))
        pb <- utils::txtProgressBar (style = 3)
    }

    if (!pkg_is_source (package) & basename (package) != package)
        package <- basename (package)

    for (i in seq_along (exs)) {

        this_fn <- names (exs) [i]
        prev_fns <- list ()
        rdname <- attr (exs [[i]], "Rdname")
        aliases <- get_fn_aliases (package, rdname)

        for (xj in exs [[i]]) {

            y <- one_ex_to_yaml (pkg = pkg_name,
                                 pkg_full = package,
                                 fn = this_fn,
                                 rdname = rdname,
                                 x = xj,
                                 aliases = aliases,
                                 prev_fns = prev_fns)

            if (!is.null (y)) {

                y <- add_class_descriptions (y, package)

                add_y <- (length (prev_fns) == 0)
                if (!add_y)
                    add_y <- !identical (y, prev_fns [[length (prev_fns)]])

                if (add_y) {
                    ret [[length (ret) + 1]] <-
                        prev_fns [[length (prev_fns) + 1]] <- y
                    names (ret) [length (ret)] <- this_fn
                }
            }
        }

        if (!quiet)
            utils::setTxtProgressBar (pb, i / length (exs))
    }

    ret <- rm_prepro_only (ret)

    if (!quiet) {
        close (pb)
        message (cli::col_green (cli::symbol$tick,
                                 " Converted examples to yaml"))
    }

    return (ret)
}

preload_package <- function (package) {
    if (pkg_is_source (package)) {

        pkg_name <- get_package_name (package)
        if (!paste0 ("package:", pkg_name) %in% search ()) {
            requireNamespace ("devtools")
            devtools::load_all (package, export_all = FALSE)
        }

        # TODO: Use g to get hash of HEAD
        #g <- rprojroot::find_root (rprojroot::is_git_root, path = package)
    } else if (!basename (package) == package) {
        # pkgs installed in tmp_loc via covr
        library (basename (package),
                 lib.loc = package,
                 character.only = TRUE)
        pkg_name <- basename (package)
    } else {
        ip <- data.frame (utils::installed.packages (),
                          stringsAsFactors = FALSE)
        if (!package %in% ip$Package) {
            stop ("package [", package, "] does not appear to be installed.")
        }
        suppressMessages (
                          library (package, character.only = TRUE)
                          )
        pkg_name <- package
    }

    return (pkg_name)
}

# combine lists of `functions` to include and `exclude` into single vector
exclude_functions <- function (package, functions, exclude = NULL) {
    if (!is.null (functions)) {
        fns <- m_get_pkg_functions (package)
        if (!all (functions %in% fns)) {
            functions <- functions [which (!functions %in% fns)]
            stop ("The following functions are not in the namespace of ",
                  "package:", package, ": [",
                  paste0 (functions, collapse = ", "), "]")
        }
        exclude <- c (exclude, fns [which (!fns %in% functions)])
    }

    return (exclude)
}

#' @param pkg Name (not file path) of package
#' @param pkg_full File path for source packages, otherwise same as `pkg`
#' @param fn Name of function for which yaml is to be constructed
#' @param rdname Name of .Rd file documenting fn
#' @param x All lines of example code for function
#' @param aliases Aliases for the function
#' @param prev_fns yaml for previous functions to be used as pre-processing
#' stages for nominated function.
#' @return An autotest 'yaml' specification of the example code given in 'x'.
#' @noRd
one_ex_to_yaml <- function (pkg, pkg_full, fn, rdname, x,
                            aliases = NULL, prev_fns = NULL) {

    x <- clean_ex_code (x)

    yaml <- c (paste0 ("package: ", pkg),
               "functions:",
               paste0 (yaml_indent (1), "- ", fn, ":"))

    fn_calls <- find_function_calls (x, fn, aliases)
    fn_short <- get_function_short_name (fn, attr (x, "is_dispatch"))

    xpre <- terminal_prepro_lines (x, fn_calls)
    x <- x [1:max (fn_calls)]

    has_prepro <- (fn_calls [1] > 1)
    yaml <- add_preprocessing_to_yaml (x, yaml, fn_calls, prev_fns)
    x <- x [fn_calls [1]:length (x)]

    x <- rm_comments_and_brackets (x)

    temp <- library_calls_to_yaml (x, has_prepro, yaml)
    yaml <- temp$yaml
    x <- temp$x
    has_prepro <- temp$has_prepro

    temp <- parse_primary_fn_calls (x, yaml, aliases, has_prepro)
    yaml <- temp$yaml
    # then remove any lines which aren't primary function calls
    x <- x [which (!x %in% temp$rm_lines)]
    rm (temp)

    yaml <- prepro_return_values (x, yaml, aliases, has_prepro)
    if (!has_prepro)
        has_prepro <- any (grepl ("- proprocess:$", yaml))
    yaml <- terminal_prepro_to_yaml (xpre, yaml, has_prepro)

    x <- unlist (lapply (x, function (i) strip_if_cond (i)))
    x <- chk_fn_calls_are_primary (x, fn, fn_short, aliases)
    if (length (x) == 0) # yaml may still have prepro lines, so return those
        return (yaml)

    x <- split_piped_lines (x)
    # Then add any lines prior to main function call to pre-processing:
    fn_calls <- grep (paste0 (paste0 (aliases, "\\s?\\("), collapse = "|"), x)
    if (fn_calls [1] > 1) {
        for (i in seq (fn_calls [1] - 1)) {
            yaml <- c (yaml, paste0 (yaml_indent (3), "- '", x [i], "'"))
        }
        x <- x [-seq (fn_calls [1] - 1)]
    }
    # And remove any lines after final function call which may have arisen
    # through splitting piped lines
    fn_calls <- grep (paste0 (paste0 (aliases, "\\s?\\("), collapse = "|"), x)
    x <- x [seq (max (fn_calls))]

    x_content <- extract_primary_call_content (x,
                                        unique (c (fn, fn_short, aliases)),
                                        pkg)
    if (length (x_content) == 0)
        return (NULL)

    x_content <- rm_assignment_operators (x_content, fn)
    x_content <- split_args_at_equals (x_content)

    yaml <- add_prev_prepro (x_content, yaml, fn, prev_fns)
    yaml <- yaml [which (!duplicated (yaml))]

    x_content <- assign_names_to_params (x_content, pkg)
    x_content <- add_default_vals_to_params (x_content, pkg)

    yaml <- add_params_to_yaml (x_content, yaml, fn)

    return (yaml)
}

#' clean any expressions that do not parse via parse_Rd.
#'
#' @param x Initial (unprocessed) lines of one example
#' currently just `\\%<operator>\\%` -> `%<operator>%`
#' @return Modified and parse-able version of x
#' @noRd
clean_ex_code <- function (x) {

    index <- which (grepl ("\\\\%", x))
    if (length (index) > 0)
        x [index] <- gsub ("\\\\%", "%", x [index])

    return (x)
}

#' @param n number of indendentations, with each one determined by
#' `options()$autotest_yaml_indent
#' @noRd
yaml_indent <- function (n = 1) {
    paste0 (rep (" ", n * options()$autotest_yaml_indent), collapse = "")
}

#' @param fn Proposed name of which which may be a method dispatch
#' @param is_dispatch Logical
#' @return fn if not dispatch, otherwise the (shorter) name of the class of
#' object on which dispatch is implemented.
#' @noRd
get_function_short_name <- function (fn, is_dispatch) {
    fn_short <- fn
    if (is_dispatch &
        any (grepl ("[[:alpha:]]\\.[[:alpha:]]", fn))) {
            fn_short <- gsub ("\\..*$", "", fn)
    }

    return (fn_short)
}

#' @param x Lines of example code
#' @param fn Name of primary function
#' @param aliases Aliases for `fn`
#' @return Index of lines in `x` which call the primary function or its alises
#' @noRd
find_function_calls <- function (x, fn, aliases) {
    is_dispatch <- attr (x, "is_dispatch")

    fn_calls <- 1 # dummy value that is never carried through
    if (is_dispatch &
        any (grepl ("[[:alpha:]]\\.[[:alpha:]]", fn))) {
            fn_short <- gsub ("\\..*$", "", fn)
            fn_calls <- grep (paste0 (fn, "|", fn_short), x)
            if (!is.null (aliases)) {
                fn_here <- paste0 (fn, "|", paste0 (aliases, collapse = "|"))
                fn_calls <- unique (c (fn_calls, grep (fn_here, x)))
            }
    } else {
        fn_here <- fn
        if (!is.null (aliases))
            fn_here <- paste0 (fn, "|", paste0 (aliases, collapse = "|"))
        fn_calls <- grep (fn_here, x)
    }

    return (fn_calls)
}

#' @param x Lines of example code
#' @param yaml yaml representation of one example
#' @param fn_calls Result of `find_function_calls`
#' @param prev_fns yaml representation of previous function calls
#' @return Potentially modified version of yaml with preprocessing lines
#' appended
#' @noRd
add_preprocessing_to_yaml <- function (x, yaml, fn_calls, prev_fns) {
    if (fn_calls [1] > 1) {
        yaml <- c (yaml,
                   paste0 (yaml_indent (2), "- preprocess:"))
        # add any pre-processing lines from prev_fns
        yaml <- c (yaml,
                   get_preprocess_lines (prev_fns))
        # Then new pre-processing lines, adding all pre-processing lines for all
        # functions
        if (fn_calls [1] > 1) {
            for (i in x [seq (fn_calls [1]) - 1])
                yaml <- c (yaml,
                           paste0 (yaml_indent (3), "- '", i, "'"))
        }
    }

    return (yaml)
}

#' @param x Lines of example code
#' @return Potentially modified version of `x` with comments at end of line
#' removed, along with any terminal bounding brackets
#' @noRd
rm_comments_and_brackets <- function (x) {
    x <- gsub ("\\s$", "", gsub ("\\#.*", "", x)) # rm comments
    index <- grep ("^\\(", x) # only lines which start with a bracket
    if (length (index) > 0)
        x [index] <- gsub ("^\\(|\\)$", "", x [index])
    return (x)
}

#' @param x Lines of example code
#' @param fn_calls Index into `x` of lines which call primary function(s)
#' @return Any lines in `x` subsequent to final `fn_calls`, which can
#' potentially be used as pre-processing lines of subsequent calls.
#' @noRd
terminal_prepro_lines <- function (x, fn_calls) {
    xpre <- NULL
    if (max (fn_calls) < length (x))
        xpre <- x [(max (fn_calls) + 1):length (x)]
    return (xpre)
}

#' @param x Lines of example code
#' @param has_prepro logical
#' @param yaml yaml representation of one example
#' @return List including potentially modified version of `yaml` with any
#' `library` calls appended to "preprocess" stage, and removed from `x`.
#' @noRd
library_calls_to_yaml <- function (x, has_prepro, yaml) {
    if (any (grepl ("^library\\s?\\(", x))) {
        index <- grep ("^library\\s?\\(", x)
        libs <- unlist (lapply (x [index], function (i)
                                strsplit (i, "\\)\\s?;") [[1]] [1]))
        if (!has_prepro) {
            yaml <- c (yaml,
                       paste0 (yaml_indent (2), "- preprocess:"))
            has_prepro <- TRUE
        }
        for (l in libs)
            yaml <- c (yaml,
                       paste0 (yaml_indent (3), "- '", l, "'"))

        # rm those lines from x if they are not compound expressions
        index2 <- !grepl ("\\)\\s?;", x [index])
        x <- x [-index [index2]]
    }

    return (list (yaml = yaml, x = x, has_prepro = has_prepro))
}

#' Parse the function calls, and only retain those for which the first enclosing
#' functions are the primary function, which notably excludes 'stopifnot'
#' statements and similar. Any of these which also assign to named variables are
#' also included in pre-processing, in case subsequent calls refer to those
#' objects
#'
#' @return List including potentially modified version of `yaml` with
#' preprocessing lines representing any primary function calls which include
#' assignment operations, along with an index of lines which may be removed from
#' `x` as they do not call primary functions or aliases.
#' @noRd
parse_primary_fn_calls <- function (x, yaml, aliases, has_prepro) {

    rm_lines <- NULL
    rm_fns <- c ("stopifnot")

    for (xi in x) {
        p <- utils::getParseData (parse (text = xi))
        syms <- which (p$token == "SYMBOL_FUNCTION_CALL")
        if (any (syms)) {
            index <- which (p$token %in% c ("LEFT_ASSIGN", "EQ_ASSIGN"))
            if (!p$text [syms [1]] %in% aliases &
                p$text [syms [1]] %in% rm_fns) {

                rm_lines <- c (rm_lines, xi)

            } else if (length (index) > 0) {
                if (index [1] < syms [1]) {

                    if (!has_prepro) {
                        yaml <- c (yaml,
                                   paste0 (yaml_indent (2), "- preprocess:"))
                        has_prepro <- TRUE
                    }
                    yaml <- c (yaml,
                               paste0 (yaml_indent (3), "- '", xi, "'"))
                }
            }
        }
    }

    return (list (yaml = yaml, rm_lines = rm_lines))
}

#' Include any primary function calls which also assign return values as
#' preprocessing steps. This differs from `parse_primary_fn_calls` because it
#' also parses expressions which assign values without necessarily being
#' function calls.
#' @noRd
prepro_return_values <- function (x, yaml, aliases, has_prepro) {
    prepro <- vapply (x, function (i) {
                          p <- utils::getParseData (parse (text = i))
                          ret <- FALSE
                          if (any (p$token == "SYMBOL_FUNCTION_CALL")) {
                              here <- which (p$token == "SYMBOL_FUNCTION_CALL" &
                                             p$text %in% aliases)
                              if (any (p$token [seq (here - 1)] %in%
                                       c ("LEFT_ASSIGN", "EQ_ASSIGN")))
                                  ret <- TRUE
                          } else { # values assigned with no function call
                              syms <- which (p$token == "SYMBOL")
                              assigns <- which (p$token %in%
                                                c ("LEFT_ASSIGN", "EQ_ASSIGN"))
                              if (length (syms) > 0 & length (assigns) > 0) {
                                  if (syms [1] < assigns [1]) {
                                      ret <- TRUE
                                  }
                              }
                          }
                          return (ret)  }, logical (1), USE.NAMES = FALSE)
    if (any (prepro)) {
        if (!has_prepro) {
            yaml <- c (yaml,
                       paste0 (yaml_indent (2), "- preprocess:"))
        }
        for (i in which (prepro)) {
            newpre <- paste0 (yaml_indent (3), "- '", x [i], "'")
            if (!newpre %in% yaml)
                yaml <- c (yaml, newpre)
        }
    }

    return (yaml)
}

#' add any terminal pre-processing lines from above
#' @param xpre Terminal pre-processing lines returned from
#' `terminal_prepro_lines()`.
#' @return Potentially modified version of `yaml` with terminal pre-processing
#' lines appended as internal pre-processing (non-terminal)
#' @noRd
terminal_prepro_to_yaml <- function (xpre, yaml, has_prepro) {
    if (!is.null (xpre)) {
        if (!has_prepro) {
            yaml <- c (yaml,
                       paste0 (yaml_indent (2), "- preprocess:"))
            has_prepro <- TRUE
        }
        for (i in xpre)
            yaml <- c (yaml, paste0 (yaml_indent (3), "- '", i, "'"))
    }
    return (yaml)
}

#' reduce function calls in x down to primary function calls
#' @return Modified verison of `x` which only includes lines which call primary
#' function or its aliases.
#' @noRd
chk_fn_calls_are_primary <- function (x, fn, fn_short, aliases) {
    chk <- vapply (x, function (i) {
                       p <- tryCatch (
                                utils::getParseData (parse (text = i)),
                                error = function (e) NULL)
                       ret <- FALSE
                       index <- NULL
                       if (!is.null (p))
                           index <- which (p$token == "SYMBOL_FUNCTION_CALL")
                       if (length (index) > 0) {
                           ret <- (p$text [index [1]] %in% aliases |
                                   p$text [index [1]] == fn_short)
                           if (any (grepl ("apply|map", p$text [index]))) {
                               # can also be part of *apply or *map functions,
                               # yet `getParseData` only parses these as SYMBOL
                               index2 <- index [1]:nrow (p)
                               index3 <- which (p$token [index2] == "SYMBOL")
                               syms <- unique (p$text [index2] [index3])
                               ret <- ret | (fn %in% syms | fn_short %in% syms)
                           }
                       }
                       return (ret) }, logical (1), USE.NAMES = FALSE)
    if (any (!chk))
        x <- x [-which (!chk)]

    return (x)
}

#' Extract content from inside primary parentheses of all primary function calls
#' @param x Lines of example code reduced down to primary function calls only
#' @param aliases List of all aliases for function(s) being called
#' @return Named list each item of which is names by the function it calls, and
#' contains a string representing the content of the primary call.
#' @noRd
extract_primary_call_content <- function (x, aliases, pkg) {

    for (a in aliases)
        x <- gsub (paste0 (a, "\\s?"), a, x)

    # find break points of primary function calls, and extend up to first
    # enclosing bracket
    br1 <- lapply (aliases, function (a) {
                       g <- paste0 (a, "\\(")
                       f <- vapply (gregexpr (g, x), function (i)
                                    i [1],
                                    integer (1))
                       f [f < 0] <- .Machine$integer.max
                       b <- gregexpr ("\\(", x)
                       vapply (seq_along (b), function (i)
                               b [[i]] [which (b [[i]] > f [i]) [1]],
                               integer (1))
                       })

    br1 <- apply (do.call (rbind, br1), 2, function (i)
                  min (i [which (!is.na (i))]))
    # those may still include assignment operators or similar, so extract actual
    # fn calls by parsing expressions
    fn_calls <- vapply (seq_along (br1), function (i) {
                            this_x <- substring (x [i], 1, br1 [i] - 1)
                            xp <- tryCatch (
                                    utils::getParseData (parse (text = this_x)),
                                    error = function (e) NULL)
                            if (is.null (xp))
                                return ("")
                            syms <- which (xp$token == "SYMBOL")
                            # last symbol must be function call:
                            xp$text [syms [length (syms)]] },
                            character (1))
    # check whether those fn_calls are internal, and append/retain `:::` if so:
    fn_calls <- vapply (seq_along (fn_calls), function (i) {
                            fout <- fn_calls [i]
                            pos <- regexpr (fout, x [i])
                            if (pos < 4)
                                return (fout)

                            if (substring (x [i], pos - 3, pos - 1) == ":::") {
                                fout <- paste0 (pkg, ":::", fout)
                            }
                            return (fout)
                            }, character (1))

    # check whether any function calls are parts of other functions (like in
    # ?Normal for erf), and replace nominated fn variable with 1.
    is_fn <- regexpr ("function\\s?\\(", x)
    fn_index <- which (is_fn > 0 & is_fn < br1)
    if (length (fn_index) > 0) {
        br2 <- gregexpr ("\\)", x)
        br2 <- vapply (seq_along (br2), function (i)
                       br2 [[i]] [which (br2 [[1]] > is_fn [1]) [1]],
                       integer (1)) [fn_index]
        #pos1 <- is_fn [fn_index] + attr (is_fn, "match.length") [fn_index]
        #fn_pars <- strsplit (substring (x [fn_index], pos1, br2 - 1), ",")
    }

    x <- substring (x, br1, nchar (x))
    # That reduces expressions down to everything after opening parenthesis of
    # first function call to one of the alias names. Now find the matching
    # closing bracket for each line
    brackets <- lapply (x, function (i) bracket_sequences_one_line (i))
    for (i in seq_along (brackets)) {
        x [i] <- substring (x [i], brackets [[i]] [1],
                            brackets [[i]] [2])
    }

    x <- x [which (vapply (x, length, integer (1), USE.NAMES = FALSE) > 0)]

    names (x) <- fn_calls

    # any failied getParseData from above is rejected here:
    x <- x [which (!names (x) == "")]

    x <- split_content_at_commas (x)

    return (x)
}

#' Split the content of primary calls at any primary dividing commas (if such
#' exist), effectively separating distinct argument values for each parameter.
#' @param x The content of primary calls as initially extracted above in
#' `extract_primary_call_content`
#' @return Potentially modified form of `x`, with each single-character list
#' item split into multiple elements, one for each parameter, around primary
#' dividing commas.
#' @noRd
split_content_at_commas <- function (x) {

    x <- lapply (x, function (i) {

                     i <- gsub ("^\\(|\\)$", "", i)
                     index1 <- gregexpr ("\\(", i) [[1]]
                     index2 <- gregexpr ("\\)", i) [[1]]
                     commas <- gregexpr (",", i) [[1]]
                     # but not selection commas in square brackets like `x[, 1]`
                     br <- bracket_sequences_one_line (i,
                                                       open_sym = "\\[",
                                                       close_sym = "\\]")
                     # that has [open1, close1, open2, close2, ...], so convert
                     # to sequences of all characters within square brackets:
                     if (length (br) == 2) {

                         index_br <- (br [1]:br [2])

                     } else if (length (br > 2)) {

                         index_br <- 2 * seq (length (br) / 2)
                         index_br <- cbind (br [index_br - 1], br [index_br])
                         index_br <- apply (index_br, 1, function (i)
                                            i [1]:i [2])
                         # apply returns a matrix if all i[1]:i[2] sequences are
                         # same length, otherwise it is a list
                         if (is.list (index_br))
                             index_br <- do.call (c, index_br)
                         else
                             index_br <- as.vector (index_br)

                         commas <- commas [which (!commas %in% index_br)]
                     }

                     index1 <- index1 [index1 > 0]
                     index2 <- index2 [index2 > 0]
                     commas <- commas [commas > 0]

                     if (length (index1) > 0 & length (index2) > 0) {

                        for (j in seq_along (index1)) {

                            index <- which (commas > index1 [j] &
                                            commas < index2 [j])
                            commas <- commas [which (!seq_along (commas) %in%
                                                     index)]
                         }
                     }

                     ret <- i

                     if (length (commas) > 0) {
                         commas <- rm_commas_in_qts (commas, i)

                         ret <- apply (commas, 1, function (j)
                                       substring (i, j [1], j [2]))
                     }

                     return (ret)
                      })

    return (x)
}

#' reduce vector of comma positions to only those which are not contained within
#' quotation marks. Also transform result to matrix with each row having [start,
#' end] positions of sequences between commas.
#'
#' @param commas vector of positions of all commas
#' @param s The string containing the commas
#' @noRd
rm_commas_in_qts <- function (commas, s) {

    qts <- gregexpr ("\"", s) [[1]]
    qts <- qts [qts > 0]
    comma_seq <- cbind (c (1, commas + 1),
                        c (commas, nchar (s)))
    comma_seq <- apply (comma_seq, 1, function (i)
                        i [1]:i [2])
    if (is.matrix (comma_seq)) {
        cs <- list ()
        for (i in seq (ncol (comma_seq)))
            cs [[i]] <- comma_seq [, i]
        comma_seq <- cs
    }
    f <- lapply (seq_along (comma_seq), function (i)
                 rep (i, length (comma_seq [[i]])))
    f <- unlist (f)
    qts_grp <- f [qts]
    n <- vapply (split (qts_grp, f = factor (qts_grp)),
                 length,
                 integer (1),
                 USE.NAMES = FALSE)
    is_even <- n %% 2 == 0
    if (any (!is_even)) {
        rm_seq <- unlist (comma_seq [!is_even])
        commas <- commas [!which (commas %in% rm_seq)]
    }

    cbind (c (1, commas + 1),
           c (commas - 1, unname (nchar (s))))
}

#' @param x content of primary function calls split into separate parameters,
#' each of which may include assignment operators
#' @return Same content but with assignment operations removed
#' @noRd
rm_assignment_operators <- function (x, fn) {
    for (i in seq_along (x)) {
        p <- tryCatch (utils::getParseData (parse (text = x [[i]])),
                       error = function (e) NULL)
        if (is.null (p))
            next

        j <- which (p$token == "SYMBOL_FUNCTION_CALL" & p$text == fn)
        k <- which (p$token == "LEFT_ASSIGN")
        if (length (j) == 0 | length (k) == 0)
            next

        if (k [1] < j [1])
            x [[i]] <- substring (x [[i]], p$col1 [j], nchar (x [[i]]))
    }

    return (x)
}

#' @param x Content of primary function calls split into separate parameters
#' @return Equivalent content with any named parameters split between names and
#' values
#' @noRd
split_args_at_equals <- function (x) {
    lapply (x, function (i) {
                res <- lapply (i, function (j) {
                                   if (!grepl ("=", j) | grepl ("^\"", j))
                                       ret <- c (NA_character_, j)
                                   else {
                                       # split at first "=", presume all others
                                       # to be internal list items or the like
                                       idx <- regexpr ("=", j)
                                       # but exclude "==":
                                       idx2 <- regexpr ("==", j)
                                       idx <- idx [which (!idx %in%
                                                          idx2 [idx2 > 0])]
                                       if (length (idx) == 0)
                                           idx <- -1L
                                       # and check that it's not an "="
                                       # contained within a parenthetical
                                       # expression:
                                       br <- regexpr ("\\((.+)?\\)", j)
                                       br <- seq (as.integer (br [[1]]),
                                                  as.integer (br [[1]]) +
                                                      attr (br, "match.length"))
                                       if (idx [1] < 0 |
                                           (as.integer (idx) %in% br))
                                           ret <- c (NA_character_, j)
                                       else
                                           ret <- c (substring (j, 1, idx - 1),
                                             substring (j, idx + 1, nchar (j)))
                                   }
                                ret <- gsub ("^\\s+|\\s+$", "", ret)
                                return (ret)
                            })
                do.call (rbind, res)  })
}

#' Add any other objects have been constructed in previous examples as
#' pre-processing steps.
#' @noRd
add_prev_prepro <- function (x, yaml, fn, prev_fns) {
    pp <- prev_preprocess (prev_fns, fn)
    po <- prev_objects (pp)
    for (i in seq_along (x)) {
        if (any (po %in% x [[i]] [, 2])) {
            # add those pre-processing steps
            iend <- length (yaml)
            if (any (grepl ("preprocess:$", yaml))) {
                istart <- grep ("preprocess:$", yaml)
                prepro <- yaml [istart:iend]
                yaml_top <- yaml [1:(istart - 1)]
            } else {
                prepro <- paste0 (yaml_indent (2), "- preprocess:")
                yaml_top <- yaml [1:iend]
            }

            # TODO: The following is not correct because it only grabs one line,
            # but there may be cases with multiple lines
            this_pp <- vapply (pp [[which (po %in% x [[i]] [, 2])]],
                               function (i)
                               paste0 (yaml_indent (3), "- '", i, "'"),
                               character (1),
                               USE.NAMES = FALSE)
            this_prepro <- c (prepro [1], this_pp)
            if (length (prepro) > 1)
                this_prepro <- c (this_prepro, prepro [2:length (prepro)])

            # stick those preprocessing lines in the yaml
            yaml <- c (yaml_top, this_prepro)
        }
    }

    return (yaml)
}


# Get preprocessing steps from previously constructed yaml representations of
# examples
prev_preprocess <- function (prev_fns, fn) {
    lapply (prev_fns, function (i) {
                out <- yaml::yaml.load (i)$functions
                out <- unlist (lapply (out, function (j)
                                       j [[fn]] [[1]]$preprocess))
                return (out [which (!duplicated (out))])
                       })
}

get_assign_position <- function (x) {
    vapply (x, function (i) {
        index1 <- regexpr ("<-", i)
        index2 <- regexpr ("=", i)
        br <- regexpr ("\\(", i)
        index1 [index1 < 0 | index1 > br] <- .Machine$integer.max
        index2 [index2 < 0 | index2 > br] <- .Machine$integer.max
        min (c (index1, index2))    },
        integer (1),
        USE.NAMES = FALSE)
}

# Get names of previous objects assigned by prev_preprocess steps
prev_objects <- function (prev_preprocesses) {
    res <- lapply (prev_preprocesses, function (i) {
                       ap <- get_assign_position (i)
                       vapply (seq_along (i), function (j)
                               substring (i [j], 1, ap [j] - 1),
                               character (1))
        })
    unique (unlist (res))
}

get_preprocess_lines <- function (x) {
    if (length (x) == 0)
        return (NULL)

    ret <- lapply (x, function (i) {
                       pre_index <- grep ("preprocess:", i)
                       par_index <- grep ("parameters:", i)
                       # prev_fns can have only pre-pro without parameters, so:
                       if (length (par_index) == 0)
                           par_index <- rep (length (i) + 1, length (pre_index))
                       res <- NULL
                       for (j in seq_along (pre_index)) {
                           index <- (pre_index [j] + 1):(par_index [j] - 1)
                           res <- c (res, i [index])
                       }
                       return (res [which (!duplicated (res))])
        })
    ret <- unlist (ret)
    ret [which (!duplicated (ret))]
}

#' @param x Content of primary function calls split into separate parameters.
#' Only those assigning to named values will have names
#' @return Equivalent to `x`, but with all unnamed parameters (those passed by
#' order) given names
#' @noRd
assign_names_to_params <- function (x, pkg) {

    pkg_env <- as.environment (paste0 ("package:", pkg))

    all_nms <- NULL
    for (i in seq (x)) {
        # assign any internal fns used in exs to pkg namespace
        fn_name <- names (x) [i]
        if (grepl (":::", fn_name)) {
            fn_name <- rm_internal_namespace (fn_name)
            tmp_fn <- utils::getFromNamespace (fn_name, pkg)
            pkg_env [[fn_name]] <- tmp_fn
        }

        pars <- formals (fun = fn_name, envir = pkg_env)
        nms <- names (pars)
        all_nms <- unique (c (all_nms, nms))
        if (any (is.na (x [[i]] [, 1]))) { # first column hold parameter names
            other_nms <- nms [which (!nms %in% x [[i]] [, 1])]
            # other_nms will be NULL for fns which have no args
            if (!all (is.null (other_nms))) {
                index <- which (is.na (x [[i]] [, 1]))
                x [[i]] [index, 1] <- other_nms [seq_along (index)]
            }
        }
    }

    # also also remove any extraneous white space
    x <- lapply (x, function (i) {
                     i [, 1] <- gsub ("^\\s?|\\s?$", "", i [, 1])
                     i [, 2] <- gsub ("^\\s?|\\s?$", "", i [, 2])
                     return (i)    })

    # rm any parameters with names that do not correspond to stated values.
    # this may sometimes fail with non-trival calls, such as starting an example
    # line which calls the primary function with `lapply`, `map`, or something
    # like that. These are virtually impossible to parse, so are caught and
    # removed here. (First element of ex will be NA for fns which have no args.)
    # Note that this allows for partial matching of argument names
    if (!"..." %in% names (pars)) {
        x <- lapply (x, function (i) {
                         index_in <- i [, 1] %in% all_nms
                         index_na <- is.na (i [, 1])
                         index_pmatch <- !is.na (pmatch (i [, 1], all_nms))
                         index <- which (index_in | index_na | index_pmatch)
                         return (i [index, , drop = FALSE])
                         })
    }

    # Default values of double quotes must also be replaced with escape
    # characters. Parameters may also be called "null" (like
    # changepoint::decision), yet this is a reserved yaml word, so must be
    # quoted.
    x <- lapply (x, function (i) {
                     i [which (i [, 2] == ""), 2] <- "\"\""
                     i [which (i [, 1] == "null"), 1] <- "\"null\""
                     return (i)    })

    return (x)
}

add_default_vals_to_params <- function (x, package) {

    this_env <- as.environment (paste0 ("package:", package))

    xout <- lapply (seq_along (x), function (i) {
                    this_fn <- names (x) [i]
                    these_pars <- x [[i]] [, 1]
                    if (grepl (":::", this_fn)) {
                        this_fn <- rm_internal_namespace (this_fn)
                        tmp_fn <- utils::getFromNamespace (this_fn, package)
                        this_env [[this_fn]] <- tmp_fn
                    }

                    fmls <- formals (this_fn, envir = this_env)
                    index <- pmatch (these_pars, names (fmls))
                    fmls <- fmls [-index [which (!is.na (index))]]
                    fmls <- fmls [which (vapply (fmls,
                                                 length,
                                                 integer (1)) > 0)]

                    # only include those formals which can be evaluated
                    index <- vapply (fmls, function (j) {
                                         res <- tryCatch (eval (j),
                                              error = function (e) NULL)
                                         !is.null (res) },
                                         logical (1))
                    fmls <- fmls [index]

                    # formal args may specify all admissable values, from which
                    # only the first is extracted here where appropriate. (Args
                    # may also be functions, calls, formulas, names, and other
                    # things which are not vectors. The special case is things
                    # like control parameter lists, which are vectors but also
                    # lists.)
                    fmls <- lapply (fmls, function (j) {
                                        out <- eval (j, envir = this_env)
                                        if (is.vector (out) & !is.list (out))
                                            out <- out [1]
                                        else
                                            out <- j
                                        return (out)    })

                    # Escaped version of `\` is `\\\\`, so all instances need to
                    # be replaced in order to represent valid yaml. These are
                    # subsequently reduced later because R's `parse` function
                    # only parses `\\` and not `\\\\`.
                    # Also insert all character fmls in escaped quotations:
                    fmls <- lapply (fmls, function (j) {
                                    if (is.character (j)) {
                                        j <- gsub ("\\", "\\\\", j,
                                                   fixed = TRUE)
                                        j <- paste0 ("\"", j, "\"")
                                    }
                                    return (j)  })
                    # convert any single-val integer-mode fmls to "1L"-format
                    fmls <- lapply (fmls, function (j) {
                                        if (is.numeric (j) &
                                            length (j) == 1 &
                                            storage.mode (j) == "integer")
                                            j <- paste0 (j, "L")
                                        return (j)
                                    })
                    out <- cbind (names (fmls),
                                  unname (do.call (c, fmls, quote = TRUE)))
                    rbind (x [[i]], out)
        })
    names (xout) <- names (x)

    # rm any which then have no parameters:
    xout <- xout [which (vapply (xout, nrow, integer (1)) > 0)]

    return (xout)
}

#' add to parameters list of yaml, duplicating fn name and preprocessing stages
#' each time
#' @param `x` List of arrays of parameter names and values, each of which
#' specifies one set of arguments submitted to `fn`.
#' @return Modified version of `yaml` which repeats all submitted stages once
#' for each list item of `x`.
#' @noRd
add_params_to_yaml <- function (x, yaml, fn) {

    fn_start <- grep (paste0 ("- ", fn, ":"), yaml)
    pre <- yaml [fn_start:length (yaml)]
    yaml <- yaml [1:(fn_start - 1)]

    for (i in seq_along (x)) {
        pre [1] <- paste0 (yaml_indent (1), "- ", names (x) [i], ":")
        yaml <- c (yaml,
                   pre,
                   paste0 (yaml_indent (2), "- parameters:"))
        # functions with no parameters - these are not processed in any
        # autotests
        if (nrow (x [[i]]) == 1 & all (is.na (x [[i]] [, 1]))) {
            yaml <- c (yaml,
                       paste0 (yaml_indent (3), "- (none)"))
        } else {
            for (j in seq (nrow (x [[i]]))) {
                # range expressions like `x:y` can not be left, because `:` is
                # YAML field delimiter, so
                val_j <- x [[i]] [j, 2]
                if (grepl ("[0-9]:[0-9]", val_j)) {
                    val_j <- eval (parse (text = val_j))
                    val_j <- paste0 ("[",
                                     paste0 (val_j, collapse = ", "),
                                     "]")
                } else if (is.list (val_j)) {
                    if (is.function (val_j [[1]]))
                        val_j <- paste0 ("\'", val_j, "\'")
                }
                yaml <- c (yaml,
                           paste0 (yaml_indent (3),
                                   "- ",
                                   x [[i]] [j, 1],
                                   ": ",
                                   val_j))
            }
        }
    }

    return (yaml)
}

get_fn_aliases <- function (pkg, fn_name) {
    if (pkg_is_source (pkg))
        return (get_aliases_source (pkg, fn_name))
    else
        return (get_aliases_non_source (pkg, fn_name))
}

# first get all aliases for all functions in package:
get_aliases_non_source <- function (pkg, fn_name) {

    if (basename (pkg) == pkg)
        loc <- file.path (R.home (), "library", pkg, "help", pkg)
    else
        loc <- file.path (pkg, "help", basename (pkg))
    e <- new.env ()
    chk <- lazyLoad (loc, envir = e) # nolint
    fns <- ls (envir = e)
    all_aliases <- lapply (fns, function (i) {
                           rd <- get (i, envir = e)
                           is_alias <- vapply (rd, function (j)
                                attr (j, "Rd_tag") == "\\alias",
                                logical (1))
                        vapply (rd [which (is_alias)], function (j)
                                j [[1]], character (1))
        })
    names (all_aliases) <- fns

    has_fn_name <- which (vapply (all_aliases, function (i)
                                  fn_name %in% i, logical (1)))
    if (length (has_fn_name) > 0) {
        aliases <- unname (unlist (all_aliases [has_fn_name]))
        classes <- vapply (aliases, function (i) {
                               pkg_env <- as.environment (paste0 ("package:",
                                                            basename (pkg)))
                               i_get <- tryCatch (get (i, envir = pkg_env),
                                                  error = function (e) NULL)
                               ret <- NA_character_
                               if (!is.null (i_get))
                                   ret <- class (i_get) [1]
                               return (ret) },
                               character (1))
        aliases <- aliases [which (classes == "function")]
    }

    return (aliases)
}


get_aliases_source <- function (pkg, fn_name) {
    f <- file.path (pkg, "man", paste0 (fn_name, ".Rd"))
    x <- readLines (f, warn = FALSE)

    aliases <- NULL

    index <- grep ("^\\\\alias\\{", x)
    if (length (index) > 0) {
        aliases <- gsub ("^\\\\alias\\{|\\}$", "", x [index])
    }
    return (aliases)
}

#' Take initial yaml output from `one_ex_to_yaml`, check function documentation
#' to see which parameters include descriptions of expected classes, and add
#' those class restrictions to `yaml` if so.
#' @noRd
add_class_descriptions <- function (yaml, package) {
    classes <- param_classes_in_desc (yaml, package)
    index <- which (!is.na (classes$class_in_desc))
    if (length (index) > 0) {
        classes <- classes [index, ]
        for (j in seq (nrow (classes)))
            yaml <- add_class_restriction (yaml, classes [j, ])
    }
    attr (yaml, "package") <- package

    return (yaml)
}

#' Some yamls have only preprocessing stages, with no actual function call, yet
#' this can not be ascertained until subsequent ones have been processed,
#' because the preprocessing may be necessary for subsequent steps. Once they've
#' all been constructed, any individual items with preprocessing only can be
#' removed
#'
#' @noRd
rm_prepro_only <- function (x) {
    has_params <- vapply (x, function (i)
                          any (grepl ("- parameters:$", i)),
                          logical (1),
                          USE.NAMES = FALSE)
    return (x [which (has_params)])
}
