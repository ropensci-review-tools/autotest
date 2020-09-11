#' examples_to_yaml
#'
#' Convert examples for a specified package to an 'autotest' 'yaml' to use to
#' automatically test package
#'
#' @param package Name of package for which 'yaml' is to be generated.
#' @param exclude Names of functions to exclude from 'yaml' template
#' @export
examples_to_yaml <- function (package = NULL, exclude = NULL) {
    if (!package %in% search ())
        suppressMessages (
                          library (package, character.only = TRUE)
        )

    exs <- get_all_examples (package)
    exs <- exs [which (!names (exs) %in% exclude)]

    ret <- list ()
    for (i in seq (exs)) {
        this_fn <- names (exs) [i]
        prev_fns <- list ()
        for (xj in exs [[i]]) {
            y <- one_ex_to_yaml (pkg = package, fn = this_fn, x = xj, prev_fns = prev_fns)
            ret [[length (ret) + 1]] <- prev_fns [[length (prev_fns) + 1]] <- y
            names (ret ) [length (ret)] <- this_fn
            prev_fns [[length (prev_fns) + 1]] <- y
        }
    }

    return (ret)
}

# convert one example from get_fn_exs to yaml output
one_ex_to_yaml <- function (pkg, fn, x, prev_fns = NULL) {

    i1 <- paste0 (rep (" ", 4), collapse = "")
    i2 <- paste0 (rep (" ", 8), collapse = "")
    i3 <- paste0 (rep (" ", 12), collapse = "")

    yaml <- c (paste0 ("package: ", pkg),
               "functions:",
               paste0 (i1, "- ", fn, ":"))

    is_dispatch <- attr (x, "is_dispatch")
    # find which lines call `fn`:
    fn_short <- fn
    if (is_dispatch &
        any (grepl ("[[:alpha:]]\\.[[:alpha:]]", fn))) {
            fn_short <- gsub ("\\..*$", "", fn)
            fn_calls <- grep (paste0 (fn, "|", fn_short), x)
    } else {
        fn_calls <- grep (fn, x)
    }
    # rm all lines after final fn_calls, but keep to add as terminal
    # pre-processing lines
    xpre <- NULL
    if (max (fn_calls) < length (x))
        xpre <- x [(max (fn_calls) + 1):length (x)]
    x <- x [1:max (fn_calls)]

    has_prepro <- FALSE
    if (fn_calls [1] > 1) {
        has_prepro <- TRUE
        yaml <- c (yaml,
                   paste0 (i2, "- preprocess:"))
        # add any pre-processing lines from prev_fns
        yaml <- c (yaml,
                   get_preprocess_lines (prev_fns))
        # Then new pre-processing lines, adding all pre-processing lines for all
        # functions
        for (i in seq_along (fn_calls))
            for (j in x [seq (fn_calls [i]) - 1])
                yaml <- c (yaml,
                           paste0 (i3, "- '", j, "'"))
    }

    # get fn formals:
    pkg_env <- as.environment (paste0 ("package:", pkg))
    pars <- formals (fun = fn, envir = pkg_env)
    nms <- names (pars)

    # capture content between parentheses:
    x <- x [fn_calls [1]:length (x)]
    # remove comments at end of lines:
    x <- gsub ("\\s$", "", gsub ("\\#.*", "", x))
    # remove terminal bounding brackets if any:
    index <- grep ("^\\(", x) # only lines which start with a bracket
    if (length (index) > 0)
        x [index] <- gsub ("^\\(|\\)$", "", x [index])

    # Parse the function calls, and only retain those for which the first
    # enclosing functions are the primary function, which notably excludes
    # 'stopifnot' statements and similar. Any of these which also assign to
    # named variables are also included in pre-processing, in case subsequent
    # calls refer to those objects
    rm_lines <- NULL
    rm_fns <- c ("stopifnot")
    for (xi in x) {
        p <- utils::getParseData (parse (text = xi))
        syms <- which (p$token == "SYMBOL_FUNCTION_CALL")
        if (any (syms)) {
            if (p$text [syms [1]] != fn & p$text [syms [1]] %in% rm_fns) {
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
    # then remove any lines which aren't primary function calls
    x <- x [which (!x %in% rm_lines)]
    # also check whether any assign return values, and copy these to
    # pre-processing:
    prepro <- vapply (x, function (i) {
                          p <- utils::getParseData (parse (text = i))
                          ret <- FALSE
                          if (any (p$token == "SYMBOL_FUNCTION_CALL")) {
                              here <- which (p$token == "SYMBOL_FUNCTION_CALL" &
                                             p$text == fn)
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
            has_prepro <- TRUE
        }
        for (i in which (prepro)) {
            newpre <- paste0 (i3, "- '", x [i], "'")
            if (!newpre %in% yaml)
                yaml <- c (yaml, newpre)
        }
    }

    # add any terminal pre-processing lines from above
    if (!is.null (xpre)) {
        if (!has_prepro) {
            yaml <- c (yaml,
                       paste0 (i2, "- preprocess:"))
            has_prepro <- TRUE
        }
        for (i in xpre)
            yaml <- c (yaml, paste0 (i3, "- '", i, "'"))
    }

    x <- unlist (lapply (x, function (i) strip_if_cond (i)))
    # only proceed if primary function calls in all lines of x are for the focal
    # function
    chk <- vapply (x, function (i) {
                       p <- utils::getParseData (parse (text = i))
                       ret <- FALSE
                       index <- which (p$token == "SYMBOL_FUNCTION_CALL")
                       if (length (index) > 0) {
                           ret <- (p$text [index [1]] == fn |
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
    if (length (x) == 0)
        return (NULL)

    ex <- regmatches (x, gregexpr("(?=\\().*?(?<=\\))$", x, perl=T))
    ex <- ex [which (vapply (ex, length, integer (1), USE.NAMES = FALSE) > 0)]
    # split at commas, but only those within primary enclosing parentheses:
    ex <- lapply (ex, function (i) {
                      i <- gsub ("^\\(", "", gsub ("\\)$", "", i))
                      index1 <- gregexpr ("\\(", i) [[1]]
                      index2 <- gregexpr ("\\)", i) [[1]]
                      commas <- gregexpr (",", i) [[1]]
                      index1 <- index1 [index1 > 0]
                      index2 <- index2 [index2 > 0]
                      commas <- commas [commas > 0]
                      if (length (index1) > 0) {
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

    # remove any assignment operators, to reduce to bare function calls
    for (i in seq_along (ex)) {
        p <- utils::getParseData (parse (text = ex [[i]]))
        j <- which (p$token == "SYMBOL_FUNCTION_CALL" & p$text == fn)
        k <- which (p$token == "LEFT_ASSIGN")
        if (length (j) == 0 | length (k) == 0)
            next

        if (k [1] < j [1])
            ex [[i]] <- substring (ex [[i]], p$col1 [j], nchar (ex [[i]]))
    }

    # split all example lines around "=", but only if they're not quoted
    ex <- lapply (ex, function (i) {
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

    # check whether any other objects have been constructed in previous examples
    # = previous pre-processing steps:
    pp <- prev_preprocess (prev_fns, fn)
    po <- prev_objects (pp)
    for (i in seq_along (ex)) {
        if (any (po %in% ex [[i]] [, 2])) {
            # add those pre-processing steps
            #iend <- grep ("parameters:$", yaml) - 1
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
            this_pp <- vapply (pp [[which (po %in% ex [[i]] [, 2])]], function (i)
                               paste0 (i3, "- '", i, "'"), character (1),
                               USE.NAMES = FALSE)
            this_prepro <- c (prepro [1], this_pp)
            if (length (prepro) > 1)
                this_prepro <- c (this_prepro, prepro [2:length (prepro)])

            # stick those preprocessing lines in the yaml
            yaml <- c (yaml_top, this_prepro)
        }
    }

    # Remove any duplicated lines. This works because duplicated always flags
    # the 2nd instances which are by definition redundant
    d <- duplicated (yaml)
    if (any (d))
        yaml <- yaml [-which (d)]

    # assign names to any unnamed parameters:
    for (i in seq (ex)) {
        if (any (is.na (ex [[i]] [, 1]))) {
            other_nms <- nms [which (!nms %in% ex [[i]] [, 1])]
            index <- which (is.na (ex [[i]] [, 1]))
            ex [[i]] [index, 1] <- other_nms [seq (index)]
        }
    }

    # remove any extraneous white space
    ex <- lapply (ex, function (i) {
                      i [, 1] <- gsub ("^\\s?|\\s?$", "", i [, 1])
                      i [, 2] <- gsub ("^\\s?|\\s?$", "", i [, 2])
                      return (i)    })

    # this may sometimes fail with non-trival calls, such as starting an example
    # line which calls the primary function with `lapply`, `map`, or something
    # like that. These are virtually impossible to parse, so are caught and
    # removed here.
    ex <- lapply (ex, function (i) i [which (i [, 1] %in% nms), , drop = FALSE])
    # Default values of double quotes must also be replaced with escape
    # characters. Parameters may also be called "null" (like
    # changepoint::decision), yet this is a reserved yaml word, so must be
    # quoted.
    ex <- lapply (ex, function (i) {
                      i [which (i [, 2] == ""), 2] <- "\"\""
                      i [which (i [, 1] == "null"), 1] <- "\"null\""
                      return (i)    })

    # add to parameters list of yaml, duplicating fn name and preprocessing
    # stages each time:
    fn_start <- grep (paste0 ("- ", fn, ":"), yaml)
    pre <- yaml [fn_start:length (yaml)]
    yaml <- yaml [1:(fn_start - 1)]
    for (i in seq (ex)) {
        yaml <- c (yaml,
                   pre,
                   paste0 (i2, "- parameters:"))
        for (j in seq (nrow (ex [[i]]))) {
            yaml <- c (yaml,
                       paste0 (i3, "- ", ex [[i]] [j, 1], ": ", ex [[i]] [j, 2]))
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

    # `gregexpr` return -1 for no match; these are removed here
    br_open <- lapply (gregexpr (open_sym, x), function (i)
                       as.integer (i [i >= 0]))
    br_closed <- lapply (gregexpr (close_sym, x), function (i)
                       as.integer (i [i >= 0]))
    br_both <- lapply (gregexpr (both_sym, x), function (i)
                       as.integer (i [i >= 0]))
    for (i in seq (x)) {
        br_open [[i]] <- br_open [[i]] [which (!br_open [[i]] <= 0)]
        br_closed [[i]] <- br_closed [[i]] [which (!br_closed [[i]] <= 0)]
        br_both [[i]] <- br_both [[i]] [which (!br_both [[i]] <= 0)]
        # Then remove all instances of matched brackets on one line
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
            for (j in index) {
                xmid [j + 1] <- paste0 (xmid [j], xmid [j + 1], collapse = " ")
            }
            xmid <- xmid [-index]
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
