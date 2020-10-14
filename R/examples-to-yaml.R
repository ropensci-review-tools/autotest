#' examples_to_yaml
#'
#' Convert examples for a specified package to an 'autotest' 'yaml' to use to
#' automatically test package
#'
#' @param package Name of package for which 'yaml' is to be generated.
#' @param exclude Names of functions to exclude from 'yaml' template
#' @export
examples_to_yaml <- function (package = NULL, exclude = NULL) {

    if (pkg_is_source (package)) {

        desc <- readLines (file.path (package, "DESCRIPTION"))
        pkg_name <- gsub ("Package:\\s?", "", desc [grep ("^Package\\:", desc)])
        if (!paste0 ("package:", pkg_name) %in% search ())
            devtools::load_all (package, export_all = FALSE)

        # TODO: Use g to get hash of HEAD
        #g <- rprojroot::find_root (rprojroot::is_git_root, path = package)
    } else {
        ip <- data.frame (utils::installed.packages ())
        if (!package %in% ip$Package) {
            stop ("package [", package, "] does not appear to be installed.")
        }
        suppressMessages (
                          library (package, character.only = TRUE)
                          )
        pkg_name <- package
    }

    exs <- get_all_examples (package, pkg_is_source (package), exclude)

    ret <- list ()
    for (i in seq (exs)) {
        this_fn <- names (exs) [i]
        prev_fns <- list ()
        aliases <- get_fn_aliases (package, this_fn)
        for (xj in exs [[i]]) {
            y <- one_ex_to_yaml (pkg = pkg_name,
                                 fn = this_fn,
                                 x = xj,
                                 aliases = aliases,
                                 prev_fns = prev_fns)
            if (!is.null (y)) {
                ret [[length (ret) + 1]] <- prev_fns [[length (prev_fns) + 1]] <- y
                names (ret ) [length (ret)] <- this_fn
            }
        }
    }

    return (ret)
}

# convert one example from get_fn_exs to yaml output
one_ex_to_yaml <- function (pkg, fn, x, aliases = NULL, prev_fns = NULL) {

    i1 <- paste0 (rep (" ", 4), collapse = "")
    i2 <- paste0 (rep (" ", 8), collapse = "")
    i3 <- paste0 (rep (" ", 12), collapse = "")

    yaml <- c (paste0 ("package: ", pkg),
               "functions:",
               paste0 (i1, "- ", fn, ":"))

    fn_calls <- find_function_calls (x, fn, aliases)
    fn_short <- get_function_short_name (fn, attr (x, "is_dispatch"))

    xpre <- terminal_prepro_lines (x, fn_calls)
    x <- x [1:max (fn_calls)]

    has_prepro <- (fn_calls [1] > 1)
    yaml <- add_preprocessing_to_yaml (x, yaml, fn_calls, prev_fns, i2, i3)
    x <- x [fn_calls [1]:length (x)]

    x <- rm_comments_and_brackets (x)

    temp <- library_calls_to_yaml (x, has_prepro, yaml, i2, i3)
    yaml <- temp$yaml
    x <- temp$x
    has_prepro <- temp$has_prepro

    temp <- parse_primary_fn_calls (x, yaml, aliases, has_prepro, i2, i3)
    yaml <- temp$yaml
    # then remove any lines which aren't primary function calls
    x <- x [which (!x %in% temp$rm_lines)]
    rm (temp)

    yaml <- prepro_return_values (x, yaml, aliases, has_prepro, i2, i3)
    if (!has_prepro)
        has_prepro <- any (grepl ("- proprocess:$", yaml))
    yaml <- terminal_prepro_to_yaml (xpre, yaml, has_prepro, i2, i3)

    x <- unlist (lapply (x, function (i) strip_if_cond (i)))
    x <- chk_fn_calls_are_primary (x, fn, fn_short, aliases)
    if (length (x) == 0)
        return (NULL)

    x <- split_piped_lines (x)
    # Then add any lines prior to main function call to pre-processing:
    fn_calls <- grep (paste0 (paste0 (aliases, "\\s?\\("), collapse = "|"), x)
    if (fn_calls [1] > 1) {
        for (i in seq (fn_calls [1] - 1)) {
            yaml <- c (yaml, paste0 (i3, "- '", x [i], "'"))
        }
        x <- x [-seq (fn_calls [1] - 1)]
    }
    # And remove any lines after final function call which may have arisen
    # through splitting piped lines
    fn_calls <- grep (paste0 (paste0 (aliases, "\\s?\\("), collapse = "|"), x)
    x <- x [seq (max (fn_calls))]

    x_content <- extract_primary_call_content (x, aliases)

    x_content <- rm_assignment_operators (x_content, fn)
    x_content <- split_args_at_equals (x_content)

    yaml <- add_prev_prepro (x_content, yaml, fn, prev_fns, i2, i3)
    yaml <- yaml [which (!duplicated (yaml))]

    x_content <- assign_names_to_params (x_content, pkg)

    # Finally, check documentation to see whether those parameters include
    # descriptions of expected classes
    classes <- param_classes_in_desc (x_content, yaml, package, fn)

    yaml <- add_params_to_yaml (x_content, yaml, fn, i1, i2, i3)

    return (yaml)
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
add_preprocessing_to_yaml <- function (x, yaml, fn_calls, prev_fns, i2, i3) {
    if (fn_calls [1] > 1) {
        yaml <- c (yaml,
                   paste0 (i2, "- preprocess:"))
        # add any pre-processing lines from prev_fns
        yaml <- c (yaml,
                   get_preprocess_lines (prev_fns))
        # Then new pre-processing lines, adding all pre-processing lines for all
        # functions
        if (fn_calls [1] > 1) {
            for (i in x [seq (fn_calls [1]) - 1])
                yaml <- c (yaml,
                           paste0 (i3, "- '", i, "'"))
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
library_calls_to_yaml <- function (x, has_prepro, yaml, i2, i3) {
    if (any (grepl ("^library\\s?\\(", x))) {
        index <- grep ("^library\\s?\\(", x)
        libs <- unlist (lapply (x [index], function (i) strsplit (i, "\\)\\s?;") [[1]] [1]))
        if (!has_prepro) {
            yaml <- c (yaml,
                       paste0 (i2, "- preprocess:"))
            has_prepro <- TRUE
        }
        for (l in libs)
            yaml <- c (yaml,
                       paste0 (i3, "- '", l, "'"))

        # rm those lines from x if they are not compound expressions
        index2 <- !grepl ("\\)\\s?;", x [index])
        x <- x [-index [index2] ]
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
parse_primary_fn_calls <- function (x, yaml, aliases, has_prepro, i2, i3) {
    rm_lines <- NULL
    rm_fns <- c ("stopifnot")
    for (xi in x) {
        p <- utils::getParseData (parse (text = xi))
        syms <- which (p$token == "SYMBOL_FUNCTION_CALL")
        if (any (syms)) {
            if (!p$text [syms [1]] %in% aliases & p$text [syms [1]] %in% rm_fns) {
                rm_lines <- c (rm_lines, xi)
            } else if (any (p$token %in% c ("LEFT_ASSIGN", "EQ_ASSIGN"))) {
                if (which (p$token %in% c ("LEFT_ASSIGN", "EQ_ASSIGN")) [1] < syms [1]) {
                    if (!has_prepro) {
                        yaml <- c (yaml,
                                   paste0 (i2, "- preprocess:"))
                        has_prepro <- TRUE
                    }
                    #if (p$text [syms [1]] != fn)
                    #    rm_lines <- c (rm_lines, xi)
                    yaml <- c (yaml,
                               paste0 (i3, "- '", xi, "'"))
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
prepro_return_values <- function (x, yaml, aliases, has_prepro, i2, i3) {
    prepro <- vapply (x, function (i) {
                          p <- utils::getParseData (parse (text = i))
                          ret <- FALSE
                          if (any (p$token == "SYMBOL_FUNCTION_CALL")) {
                              here <- which (p$token == "SYMBOL_FUNCTION_CALL" &
                                             p$text %in% aliases)
                              if (any (p$token [seq (here - 1)] %in% c ("LEFT_ASSIGN", "EQ_ASSIGN")))
                                  ret <- TRUE
                          } else { # values assigned with no function call
                              syms <- which (p$token == "SYMBOL")
                              assigns <- which (p$token %in% c ("LEFT_ASSIGN", "EQ_ASSIGN"))
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
                       paste0 (i2, "- preprocess:"))
        }
        for (i in which (prepro)) {
            newpre <- paste0 (i3, "- '", x [i], "'")
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
terminal_prepro_to_yaml <- function (xpre, yaml, has_prepro, i2, i3) {
    if (!is.null (xpre)) {
        if (!has_prepro) {
            yaml <- c (yaml,
                       paste0 (i2, "- preprocess:"))
            has_prepro <- TRUE
        }
        for (i in xpre)
            yaml <- c (yaml, paste0 (i3, "- '", i, "'"))
    }
    return (yaml)
}

#' reduce function calls in x down to primary function calls
#' @return Modified verison of `x` which only includes lines which call primary
#' function or its aliases.
#' @noRd
chk_fn_calls_are_primary <- function (x, fn, fn_short, aliases) {
    chk <- vapply (x, function (i) {
                       p <- utils::getParseData (parse (text = i))
                       ret <- FALSE
                       index <- which (p$token == "SYMBOL_FUNCTION_CALL")
                       if (length (index) > 0) {
                           ret <- (p$text [index [1]] %in% aliases |
                                   p$text [index [1]] == fn_short)
                           # can also be part of *apply or *map functions, yet
                           # `getParseData` only parses these as SYMBOL
                           if (any (grepl ("apply|map", p$text [index]))) {
                               index2 <- index [1]:nrow (p)
                               syms <- unique (p$text [index2] [which (p$token [index2] == "SYMBOL")])
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
extract_primary_call_content <- function (x, aliases) {
    br1 <- lapply (aliases, function (a) {
                       x <- gsub (paste0 (a, "\\s?"), a, x)
                       g <- paste0 (a, "\\(")
                       vapply (gregexpr (g, x), function (i) i [1], integer (1))
                       })
    nchars <- rep (NA, length (x))
    for (i in seq (br1)) {
        nchars [which (br1 [[i]] > 0)] <- i
    }
    nchars <- nchar (aliases) [nchars]

    br1 <- apply (do.call (rbind, br1), 2, function (i) min (i [i > 0])) + nchars
    # those may still include assignment operators or similar, so extract actual
    # fn calls by parsing expressions
    fn_calls <- vapply (seq_along (br1), function (i) {
                            this_x <- substring (x [i], 1, br1 [i] - 1)
                            xp <- utils::getParseData (parse (text = this_x))
                            syms <- which (xp$token == "SYMBOL")
                            # last symbol must be function call:
                            xp$text [syms [length (syms)] ] },
                            character (1))

    x <- substring (x, br1, nchar (x))
    # That reduces expressions down to everything after opening parenthesis of
    # first function call to one of the alias names. Now find the matching
    # closing bracket for each line
    brackets <- lapply (x, bracket_sequences_one_line)
    for (i in seq_along (brackets)) {
        x [i] <- substring (x [i], brackets [[i]] [1],
                            brackets [[i]] [2])
    }

    x <- x [which (vapply (x, length, integer (1), USE.NAMES = FALSE) > 0)]

    names (x) <- fn_calls

    return (split_content_at_commas (x))
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
                     index1 <- index1 [index1 > 0]
                     index2 <- index2 [index2 > 0]
                     commas <- commas [commas > 0]
                     if (length (index1) > 0 & length (index2) > 0) {
                        for (j in seq_along (index1)) {
                            index <- which (commas > index1 [j] &
                                            commas < index2 [j])
                            commas <- commas [which (!seq_along (commas) %in% index)]
                         }
                     }
                     # do not split if the value is in double quotes
                     if (grepl ("^\"", i) & grepl ("\"$", i))
                         commas <- cbind (1, nchar (i))
                     else
                         commas <- cbind (c (1, commas + 1),
                                          c (commas - 1, nchar (i)))

                     apply (commas, 1, function (j)
                            substring (i, j [1], j [2]))
                      })

    return (x)
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
                                       c (NA_character_, j)
                                   else {
                                       #strsplit (j, "=") [[1]]   })
                                       # split at first "=", presume all
                                       # others to be internal list items
                                       # or the like
                                       indx <- regexpr ("=", j)
                                       c (substring (j, 1, indx - 1),
                                          substring (j, indx + 1, nchar (j)))
                                   }
                            })
                do.call (rbind, res)  })
}

#' Add any other objects have been constructed in previous examples as
#' pre-processing steps.
#' @noRd
add_prev_prepro <- function (x, yaml, fn, prev_fns, i2, i3) {
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
                prepro <- paste0 (i2, "- preprocess:")
                yaml_top <- yaml [1:iend]
            }

            # TODO: The following is not correct because it only grabs one line, but
            # there may be cases with multiple lines
            this_pp <- vapply (pp [[which (po %in% x [[i]] [, 2])]], function (i)
                               paste0 (i3, "- '", i, "'"), character (1),
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
                out <- unlist (lapply (out, function (j) j [[fn]] [[1]]$preprocess))
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
                       res <- NULL
                       for (j in seq_along (pre_index))
                           res <- c (res, i [(pre_index [j] + 1):(par_index [j] - 1)])
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
        pars <- formals (fun = names (x) [i], envir = pkg_env)
        nms <- names (pars)
        all_nms <- unique (c (all_nms, nms))
        if (any (is.na (x [[i]] [, 1]))) { # first column hold parameter names
            other_nms <- nms [which (!nms %in% x [[i]] [, 1])]
            # other_nms will be NULL for fns which have no args
            if (!all (is.null (other_nms))) {
                index <- which (is.na (x [[i]] [, 1]))
                x [[i]] [index, 1] <- other_nms [seq (index)]
            }
        }
    }
    
    # also also remove any extraneous white space
    x <- lapply (x, function (i) {
                     i [, 1] <- gsub ("^\\s?|\\s?$", "", i [, 1])
                     i [, 2] <- gsub ("^\\s?|\\s?$", "", i [, 2])
                     return (i)    })

    # this may sometimes fail with non-trival calls, such as starting an example
    # line which calls the primary function with `lapply`, `map`, or something
    # like that. These are virtually impossible to parse, so are caught and
    # removed here. (First element of ex will be NA for fns which have no args.)
    x <- lapply (x, function (i) i [which (i [, 1] %in% all_nms |
                                           is.na (i [, 1])), , drop = FALSE])

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

#' check whether params in `x` have classes that are specified in the
#' description of `fn_name`
#' @param x Content of primary function calls split into separate parameters.
#' @return Named logical vector indicating whether classes of objects are
#' specified in descriptions of each parameter
#' @noRd
param_classes_in_desc <- function (x, yaml, pkg, fn_name) {

    if (pkg_is_source (pkg)) {
        f <- file.path (pkg, "man", paste0 (fn_name, ".Rd"))
        x <- get_Rd_metadata (tools::parse_Rd (f), "arguments")
    } else {
        r <- tools::Rd_db (pkg)
        aliases <- lapply (r, function (i) get_Rd_metadata (i, "alias"))
        i <- vapply (aliases, function (i) fn_name %in% i, logical (1))
        r <- r [[which (i) [1] ]]
        x <- get_Rd_metadata (tools::parse_Rd (r), "arguments")
    }

    x <- strsplit (x, "\\n") [[1]]
    x <- x [x != ""]

    param_names <- unlist (lapply (x, function (i)
                                   eval (parse (text = i)) [[1]] [[1]]))
    param_descs <- unlist (lapply (x, function (i)
                                   eval (parse (text = i)) [[2]] [[1]]))

    res <- parse_yaml_template (yaml)
    e <- new.env ()
    eval (parse (text = res$preprocess [[fn]]), envir = e)
    classes <- unique (unlist (lapply (ls (envir = e), function (i)
                                       class (get (i, envir = e)))))
    class_in_desc <- vapply (param_descs, function (i) {
                                 i_s <- gsub ("\"|\'|\`", "", strsplit (i, " ") [[1]])
                                 chk <- vapply (i_s, function (j)
                                                any (grepl (j, classes)),
                                                logical (1),
                                                USE.NAMES = FALSE)
                                 return (any (chk)) },
                                 logical (1),
                                 USE.NAMES = FALSE)

    names (class_in_desc) <- param_names

    return (class_in_desc)
}


#' add to parameters list of yaml, duplicating fn name and preprocessing stages
#' each time
#' @param `x` List of arrays of parameter names and values, each of which
#' specifies one set of arguments submitted to `fn`.
#' @return Modified version of `yaml` which repeats all submitted stages once
#' for each list item of `x`.
#' @noRd
add_params_to_yaml <- function (x, yaml, fn, i1, i2, i3) {

    fn_start <- grep (paste0 ("- ", fn, ":"), yaml)
    pre <- yaml [fn_start:length (yaml)]
    yaml <- yaml [1:(fn_start - 1)]

    for (i in seq_along (x)) {
        pre [1] <- paste0 (i1, "- ", names (x) [i], ":")
        yaml <- c (yaml,
                   pre,
                   paste0 (i2, "- parameters:"))
        # functions with no parameters - these are not processed in any
        # autotests
        if (nrow (x [[i]]) == 1 & all (is.na (x [[i]] [, 1]))) {
            yaml <- c (yaml,
                       paste0 (i3, "- (none)"))
        } else {
            for (j in seq (nrow (x [[i]]))) {
                yaml <- c (yaml,
                           paste0 (i3, "- ", x [[i]] [j, 1], ": ", x [[i]] [j, 2]))
            }
        }
    }

    return (yaml)
}

# merge multi-line expressions to single line:
match_brackets <- function (x, curly = FALSE) {

    open_sym <- "\\("
    close_sym <- "\\)"
    both_sym <- "\\((.+)?\\)"
    collapse_sym <- " "
    if (curly) {
        open_sym <- "\\{"
        close_sym <- "\\}"
        both_sym <- "\\{(.+)?\\}"
        collapse_sym <- "; "
    }

    # get rid of any comments:
    x <- vapply (x, function (i) gsub ("\\#.*", "", i),
                 character (1),
                 USE.NAMES = FALSE)
    # remove empty lines
    x <- x [which (!grepl ("^\\s?$", x))]

    brseq <- bracket_sequences (x, open_sym, close_sym, both_sym)
    br_open <- brseq$br_open
    br_closed <- brseq$br_closed
    if (length (br_open) == 0 & length (br_closed) == 0)
        return (x)
    else if (any (is.na (br_open)) & any (is.na (br_closed)))
        return (NULL) # error in parsing brackets

    has_gg_pluses <- FALSE
    for (i in seq_along (br_open)) {

        xmid <- x [br_open [i]:br_closed [i]]
        if (grepl ("\\{\\s?$", xmid [1])) # join line after opening curly bracket
            xmid <- c (paste0 (xmid [1:2], collapse = " "), xmid [3:length (xmid)])
        if (grepl ("^\\s?\\}", xmid [length (xmid)])) # join line before closing curly 
        {
            if (length (xmid) > 2)
                xmid <- c (xmid [1:(length (xmid) - 2)],
                           paste0 (xmid [(length (xmid) - 1):length (xmid)], collapse = " "))
            else
                xmid <- paste0 (xmid, collapse = " ")
        }
        # plus any ggplot-type lines with terminal "+". Formulae can also end
        # with "+", so presume only "ggplot" commands will have this, and grep
        # for that also
        index <- grep ("\\+\\s?$", xmid)
        if (length (index) > 0 & any (grepl ("gg", xmid))) {
            has_gg_pluses <- TRUE
            rms <- NULL
            for (j in index) {
                if (j < length (xmid)) {
                        xmid [j + 1] <- paste0 (xmid [j], xmid [j + 1], collapse = " ")
                        rms <- c (rms, j)
                }
            }
            if (!is.null (rms))
                xmid <- xmid [-rms]
        }

        x [br_closed [i]] <- paste0 (xmid, collapse = collapse_sym)
    }
    # then remove all of the intervening lines:
    if (length (br_open) > 0) {
        index <- unlist (lapply (seq_along (br_open), function (i)
                                 br_open [i]:(br_closed [i] - 1)))
        x <- x [-index]
    }
    
    x <- gsub ("\\s+", " ", x)

    if (has_gg_pluses) {
        index <- grep ("\\+\\s?$", x)
        index2 <- cumsum (c (FALSE, diff (index) > 1))
        index <- lapply (split (index, f = as.factor (index2)), function (i)
                         c (i, max (i) + 1))

        for (i in index) {
            x [i [1] ] <- paste0 (x [i], collapse = " ")
        }
        x <- x [-unlist (lapply (index, function (i) i [-1]))]
    }

    # catch instances where curly brackets are only used on first condition,
    # with second condition being a single line
    if (curly) {
        index <- rev (grep ("else\\s?$", x))
        if (length (index) > 0) {
            for (i in index) {
                x [i] <- paste0 (x [i], " ", x [i + 1])
                x <- x [-(i + 1)]
            }
        }
    }

    return (x)
}

bracket_sequences <- function (x, open_sym, close_sym, both_sym) {

    # `gregexpr` return -1 for no match; these are removed here
    br_open <- lapply (gregexpr (open_sym, x), function (i)
                       as.integer (i [i >= 0]))
    br_closed <- lapply (gregexpr (close_sym, x), function (i)
                       as.integer (i [i >= 0]))
    br_both <- lapply (gregexpr (both_sym, x), function (i)
                       as.integer (i [i >= 0]))

    # remove any that are inside quotations, like L#44 in stats::spline
    quotes <- gregexpr ("\"", x)
    for (i in seq (x)) {
        if (any (quotes [[i]] > 0)) {
            index <- seq (length (quotes [[i]]) / 2) * 2
            qstart <- quotes [[i]] [index - 1]
            qend <- quotes [[i]] [index]
            qindex <- unlist (lapply (seq_along (qstart), function (i)
                                      qstart [i]:qend [i]))
            br_open [[i]] <- br_open [[i]] [!br_open [[i]] %in% qindex]
            br_closed [[i]] <- br_closed [[i]] [!br_closed [[i]] %in% qindex]
            br_both [[i]] <- br_both [[i]] [!br_both [[i]] %in% qindex]
        }
    }

    # examples may have rogue brackets, like in stats::spline, where it arises
    # in a plot axis label (line#62)
    if (length (unlist (br_open)) != length (unlist (br_closed)))
        return (list (br_open = NA,
                      br_closed = NA))

    # Remove all instances of matched brackets on one line
    for (i in seq (x)) {
        while (any (br_both [[i]] > 0)) {
            index <- which (br_open [[i]] == br_both [[i]])
            index2 <- which (br_closed [[i]] > br_open [[i]] [index]) [1]
            br_closed [[i]] <- br_closed [[i]] [-index2]
            br_open [[i]] <- br_open [[i]] [-index]
            br_both [[i]] <- br_both [[i]] [-1]

            if (length (br_open [[i]]) > 0 & length (br_closed [[i]]) > 0)
                br_both [[i]] <- br_open [[i]] [1]
        }
    }
    # convert to sequences of line numbers where brackets close, noting that
    # there may be multiple matched closing brackets on one line, hence the
    # `length` function here. There may also be values of -1 from the initial
    # `gregexpr` above; these need to be ignored here
    br_open2 <- br_closed2 <- NULL
    for (i in seq (br_open))
        br_open2 <- c (br_open2, rep (i, length (br_open [[i]])))
    for (i in seq (br_closed))
        br_closed2 <- c (br_closed2, rep (i, length (br_closed [[i]])))

    # no matching brackets just gives empty lines for all that follows:
    nested <- nested_sequences (br_open2, br_closed2)
    br_open <- rev (nested$br_open) # rev to ensure lines are sequentially joined
    br_closed <- rev (nested$br_closed)
    index <- which (!duplicated (cbind (br_open, br_closed)))
    br_open <- br_open [index]
    br_closed <- br_closed [index]

    list (br_open = br_open,
          br_closed = br_closed)
}

# return positions of first and last matching brackets on one line
bracket_sequences_one_line <- function (x) {
    open_sym <- "\\("
    close_sym <- "\\)"
    br_open <- lapply (gregexpr (open_sym, x), function (i)
                       as.integer (i [i >= 0])) [[1]]
    br_closed <- lapply (gregexpr (close_sym, x), function (i)
                       as.integer (i [i >= 0])) [[1]]

    return (c (br_open [1], br_closed [length (br_closed)]))
}

# check for nesting where another bracket opens before current one has
# been closed.
# open:     x       x + 1
# closed:   x + 2   x + 3
# the actual grouping should be (x, x + 3). The following moves the x to 2nd
# position and deletes the first.
nested_sequences <- function (br_open, br_closed) {
    i2 <- seq_along (br_open) [-1]
    i1 <- i2 - 1
    index <- which (br_open [i2] < br_closed [i1])
    if (length (index) > 0) {
        for (i in index)
            br_open [i + 1] <- br_open [i]
        br_open <- br_open [-index]
        br_closed <- br_closed [-index]
    }
    list (br_open = br_open,
          br_closed = br_closed)
}

# Expressions are multiple lines of code embedded within curly brackets. When
# individual components of these span multiple lines, they must first be
# concatenated to single lines, then the whole thing concatenated with each of
# these single lines terminated with a semi-colon. This function must be run
# prior to standard "match_brackets" calls.
# example: stats::approx
parse_expressions <- function (x) {
    brseq <- bracket_sequences (x, open_sym = "\\{", close_sym = "\\}", both_sym = "\\{(.+)?\\}")
    br_open <- brseq$br_open
    br_closed <- brseq$br_closed
    
    for (i in seq_along (br_open)) {
        xmid <- x [br_open [i]:br_closed [i]]
        if (length (xmid) > 2) {
            # rm content up to first curly
            cstart <- which (vapply (gregexpr ("\\{", xmid), function (i)
                                     any (i > 0), logical (1))) [1]
            j <- regexpr ("\\{", xmid [cstart])
            xstart <- substring (xmid [cstart], 1, j)
            xmid [cstart] <- gsub (".*\\{", "", xmid [cstart])

            # rm content after last curly
            cend <- which (vapply (gregexpr ("\\}", xmid), function (i)
                                   any (i > 0), logical (1)))
            cend <- utils::tail (cend, 1)
            j <- regexpr ("\\}", xmid [cend])
            xend <- substring (xmid [cend], j, nchar (xmid [cend]))
            xmid [cend] <- gsub ("\\}.*", "", xmid [cend])

            # rm blank lines
            xmid <- xmid [which (!grepl ("^\\s?$", xmid))]

            # join any `if ... else ...` lines
            index <- rev (grep ("^\\s*else", xmid))
            if (length (index) > 0) {
                xmid [index - 1] <- paste0 (xmid [index - 1], xmid [index])
                xmid <- xmid [-index]
            }
            index <- rev (grep ("^\\s*if\\s*\\(", xmid))
            if (length (index) > 0) {
                # check that if clauses do not continue to next line without
                # "{", and concatenate any which do
                br1 <- gregexpr ("\\(", xmid [index])
                br2 <- gregexpr ("\\)", xmid [index])
                br_end <- grep (NA, length (index))
                for (j in seq_along (br1)) {
                    brseq <- nested_sequences (br1 [[j]], br2 [[j]])
                    br_end [j] <- brseq$br_closed [1]
                }
                xmid_after <- substring (xmid [index], br_end + 1, nchar (xmid [index]))
                index2 <- grep ("^\\s*$", xmid_after)
                if (length (index2) > 0) {
                    xmid [index [index2]] <- paste0 (xmid [index [index2]],
                                                     xmid [index [index2] + 1])
                    xmid <- xmid [-(index [index2] + 1)]
                }
            }

            xmid <- match_brackets (c (xstart, match_brackets (xmid), xend), curly = TRUE)

            xfirst <- xlast <- NULL
            if (br_open [i] > 1)
                xfirst <- x [1:(br_open [i] - 1)]
            if (br_closed [i] < length (x))
                xlast <- x [(br_closed [i] + 1):length (x)]

            x <- c (xfirst, xmid, xlast)
        }
    }
    return (x)
}

# Join function defititions within examples into single lines. This presumes
# `match_brackets` has already been run, so merging is only ever between
# isolation "f <- function (...)" lines and subsequent definitions with or
# without curly brackets.
# example: stats::binomial
join_function_lines <- function (x) {
    fns <- rev (grep ("\\sfunction\\s?\\(", x))
    if (length (fns) > 0) {
        fn_defs <- x [fns]
        # remove everything prior to "function(":
        fn_start <- regexpr ("function\\s?\\(", fn_defs)
        fn_defs <- substring (fn_defs, fn_start, nchar (fn_defs))
        # Then everything up to closing bracket of fn def:
        br_open <- gregexpr ("\\(", fn_defs)
        br_closed <- gregexpr ("\\)", fn_defs)
        for (i in seq_along (br_open)) {
            temp <- nested_sequences (br_open [[i]], br_closed [[i]])
            br_open [[i]] <- temp$br_open
            br_closed [[i]] <- temp$br_closed
        }
        br_open <- vapply (br_open, function (i) i [1], integer (1))
        br_closed <- vapply (br_closed, function (i) i [1], integer (1))

        fn_defs <- substring (fn_defs, br_closed + 1, nchar (fn_defs))

        defs_on_next_line <- rev (grep ("^\\s?$", fn_defs))
        if (length (defs_on_next_line) > 0) {
            index <- fns [defs_on_next_line]
            x [index] <- paste0 (x [index], x [index + 1])
            x <- x [-(index + 1)]
        }
    }

    return (x)
}

# strip any if conditionals from any example lines which include the focal
# function, returning the functional lines alone from all (if + else) conditions
# x here is a single line only
strip_if_cond <- function (x) {
    if (grepl ("^\\s?if", x)) {
        br_open <- gregexpr ("\\(", x) [[1]]
        br_closed <- gregexpr ("\\)", x) [[1]]
        ns <- nested_sequences (br_open, br_closed)
        br_open <- ns$br_open
        br_closed <- ns$br_closed

        # strip first conditional:
        xi <- substring (x, br_closed [1] + 1, nchar (x))
        if (grepl ("^\\s?\\{", xi)) {
            br_open <- gregexpr ("\\{", xi) [[1]]
            br_closed <- gregexpr ("\\}", xi) [[1]]
            ns <- nested_sequences (br_open, br_closed)
            br_open <- ns$br_open
            br_closed <- ns$br_closed
            x1 <- substring (xi, br_open [1] + 1, br_closed [1] - 1)
            x2 <- substring (xi, br_closed [1] + 1, nchar (x))
            if (grepl ("else", x2)) {
                x2 <- gsub ("\\s?else\\s?", "", x2)
                br_open <- gregexpr ("\\{", x2) [[1]]
                if (br_open [1] > 0) {
                    br_closed <- gregexpr ("\\}", x2) [[1]]
                    ns <- nested_sequences (br_open, br_closed)
                    br_open <- ns$br_open
                    br_closed <- ns$br_closed
                    x2 <- substring (x2, br_closed [1] + 1, nchar (x2))
                }
            }
            x <- c (x1, x2)
        }
    }
    return (x)
}

# crude un-piping operation to split marittr-piped expressions into multiple,
# distinct base-R lines. Note that the matching of brackets is currently
# inadequate, and will only work if each expression contains only one primary
# parenthesised expression.
unpipe <- function (x) {
    x <- strsplit (x, "%>%") [[1]]
    for (i in seq_along (x)) {
        x [i] <- paste0 ("var", i, " <- ", x [i])
        if (i > 1) {
            br1 <- gregexpr ("\\(", x [i]) [[1]] [1]
            br2 <- max (gregexpr ("\\)", x [i]) [[1]])
            not_empty <- grepl ("[A-Za-z0-9]", substring (x [i], br1, br2))
            comma <- ""
            if (not_empty)
                comma <- ", "
            x [i] <- paste0 (substring (x [i], 1, br1),
                             "var", i - 1, comma,
                             substring (x [i], br1 + 1, nchar (x [i])))
        }
    }
    return (x)
}

split_piped_lines <- function (x) {
    index <- rev (grep ("(.*)%>%(.*)", x))
    for (i in index) {
        xinsert <- unpipe (x [i])
        if (i == 1) {
            if (length (x) == 1) {
                x <- xinsert
            } else {
                x <- c (xinsert, x [2:length (x)])
            }
        } else if (i == length (x)) {
            if (length (x) == 1) {
                x <- xinsert
            } else {
                x <- c (x [1:(i - 1)], xinsert)
            }
        } else {
            x <- c (x [1:(i - 1)], xinsert, x [(i + 1):length (x)])
        }
    }
    return (x)
}

get_fn_aliases <- function (pkg, fn_name) {
    if (pkg_is_source (pkg))
        return (get_aliases_source (pkg, fn_name))
    else
        return (get_aliases_non_source (pkg, fn_name))
}

get_aliases_non_source <- function (pkg, fn_name) {
    # first get all aliases for all functions in package:
    loc <- file.path (R.home (), "library", pkg, "help", pkg)
    e <- new.env ()
    chk <- lazyLoad (loc, envir = e)
    fns <- ls (envir = e)
    all_aliases <- lapply (fns, function (i) {
                           rd <- get (i, envir = e)
                           is_alias <- vapply (rd, function (j)
                                attr (j, "Rd_tag") == "\\alias",
                                logical (1))
                        vapply (rd [which (is_alias)], function (j) j [[1]], character (1))
        })
    names (all_aliases) <- fns

    #x <- get (fn_name, envir = e)

    has_fn_name <- which (vapply (all_aliases, function (i) fn_name %in% i, logical (1)))
    if (length (has_fn_name) > 0) {
        aliases <- unname (unlist (all_aliases [has_fn_name]))
        classes <- vapply (aliases, function (i) {
                               pkg_env <- as.environment (paste0 ("package:", pkg))
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
