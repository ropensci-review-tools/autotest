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

    brseq <- bracket_sequences (x, open_sym, close_sym, both_sym)
    br_open <- brseq$br_open
    br_closed <- brseq$br_closed
    if (length (br_open) == 0 & length (br_closed) == 0)
        return (x)
    else if (any (is.na (br_open)) & any (is.na (br_closed)))
        return (NULL) # error in parsing brackets

    x <- match_one_brackets (x, br_open, br_closed, collapse_sym)

    # catch instances where curly brackets are only used on first condition,
    # with second condition being a single line
    if (curly)
        x <- catch_curly_else (x)

    return (x)
}

match_one_brackets <- function (x, br_open, br_closed, collapse_sym) {

    has_gg_pluses <- FALSE

    for (i in seq_along (br_open)) {

        xmid <- x [br_open [i]:br_closed [i]]
        if (grepl ("\\{\\s?$", xmid [1])) {
            # join line after opening curly bracket
            xmid <- c (paste0 (xmid [1:2], collapse = " "),
                       xmid [3:length (xmid)])
        }
        if (grepl ("^\\s?\\}", xmid [length (xmid)])) {
            # join line before closing curly
            if (length (xmid) > 2)
                xmid <- c (xmid [1:(length (xmid) - 2)],
                           paste0 (xmid [(length (xmid) - 1):length (xmid)],
                                   collapse = " "))
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
                        xmid [j + 1] <- paste0 (xmid [j],
                                                xmid [j + 1],
                                                collapse = " ")
                        rms <- c (rms, j)
                }
            }
            if (!is.null (rms))
                xmid <- xmid [-rms]
        }

        x [br_closed [i]] <- paste0 (xmid, collapse = collapse_sym)
    }

    x <- rm_intervening_lines (x, br_open, br_closed)

    x <- gsub ("\\s+", " ", x)

    x <- rm_final_ggplus_lines (x, has_gg_pluses)

    return (x)
}

#' remove intervening lines, making sure to remove any
#' pipes into ggplot2 expression from preceding lines:
#' @noRd
rm_intervening_lines <- function (x, br_open, br_closed) {

    if (length (br_open) > 0) {

        index <- unlist (lapply (seq_along (br_open), function (i)
                                 br_open [i]:(br_closed [i] - 1)))
        index2 <- (index - 1) [index > 1]

        pipe_sym <- "\\\\%>\\\\%$"

        terminal_pipe <- grep (pipe_sym, x [index2])

        if (length (terminal_pipe) > 0) {

            terminal_pipe <- index2 [terminal_pipe]
            x [terminal_pipe] <- gsub (pipe_sym, "", x [terminal_pipe])
        }
        x <- x [-index]
    }

    return (x)
}

rm_final_ggplus_lines <- function (x, has_gg_pluses) {

    if (has_gg_pluses) {


        index <- grep ("\\+\\s?$", x)
        index2 <- cumsum (c (FALSE, diff (index) > 1))
        index <- lapply (split (index, f = as.factor (index2)), function (i)
                         c (i, max (i) + 1))

        for (i in index) {
            x [i [1]] <- paste0 (x [i], collapse = " ")
        }
        x <- x [-unlist (lapply (index, function (i) i [-1]))]
    }

    return (x)
}

catch_curly_else <- function (x) {

    index <- rev (grep ("else\\s?$", x))

    if (length (index) > 0) {
        for (i in index) {
            x [i] <- paste0 (x [i], " ", x [i + 1])
            x <- x [- (i + 1)]
        }
    }

    return (x)
}

#' Get sequences of line & character numbers within quotations
#'
#' @param x Lines of code
#' @return List of sequence indices, one for each line of `x`, identifying
#' characters within quotations
#' @noRd
quote_sequences <- function (x) {

    qts <- gregexpr ("\\\"|\\\'", x)
    qts_not_esc <- gregexpr ("'", x)
    qts <- lapply (seq_along (qts), function (i) {
                   if (qts [[i]] [1] > 0) {
                       qts [[i]] <- qts [[i]] [which (!qts [[i]] %in%
                                                      qts_not_esc [[i]])]
                   }
                   if (length (qts [[i]]) == 0)
                       qts [[i]] <- -1L
                   return (qts [[i]])
                              })
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
        qts [index - 1, 4] <- nchar (x [qts [index - 1, 1]])
        qts [index, 1] <- qts [index, 3]
        qts [index, 2] <- 1
    }

    linenums <- apply (qts, 1, function (i) i [1])

    qts <- apply (qts, 1, function (i) as.vector (seq (i [2], i [4])))
    if (!is.list (qts)) # apply when all vecs have same length
        qts <- lapply (list (qts), function (i) as.vector (i))
    names (qts) <- linenums

    return (qts)
}

bracket_sequences <- function (x, open_sym, close_sym, both_sym) {

    # `gregexpr` return -1 for no match; these are removed here
    br_open <- lapply (gregexpr (open_sym, x), function (i)
                       as.integer (i [i >= 0]))
    br_closed <- lapply (gregexpr (close_sym, x), function (i)
                       as.integer (i [i >= 0]))

    # remove any that are inside quotations, like L#44 in stats::spline
    qts <- quote_sequences (x)
    quotes <- gregexpr ("\"|\'", x)
    for (i in seq (x)) {
        if (any (quotes [[i]] > 0)) {
            index <- seq (length (quotes [[i]]) / 2) * 2
            qstart <- quotes [[i]] [index - 1]
            qend <- quotes [[i]] [index]
            qindex <- unlist (lapply (seq_along (qstart), function (i)
                                      qstart [i]:qend [i]))
            br_open [[i]] <- br_open [[i]] [!br_open [[i]] %in% qindex]
            br_closed [[i]] <- br_closed [[i]] [!br_closed [[i]] %in% qindex]
        }
    }

    # examples may have rogue brackets, like in stats::spline, where it arises
    # in a plot axis label (line#62)
    if (length (unlist (br_open)) != length (unlist (br_closed)))
        return (list (br_open = NA,
                      br_closed = NA))

    # Remove all instances of matched brackets on one line
    for (i in seq (x)) {
        len <- min (c (length (br_open [[i]]), length (br_closed [[i]])))
        index <- which (br_open [[i]] [seq (len)] < br_closed [[i]] [seq (len)])
        if (length (index) > 0) {
            br_open [[i]] <- br_open [[i]] [-index]
            br_closed [[i]] <- br_closed [[i]] [-index]
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
    # rev to ensure lines are sequentially joined
    br_open <- rev (nested$br_open)
    br_closed <- rev (nested$br_closed)
    index <- which (!duplicated (cbind (br_open, br_closed)))
    br_open <- br_open [index]
    br_closed <- br_closed [index]

    list (br_open = br_open,
          br_closed = br_closed)
}

# return positions of paris of outer matching brackets on one line, as [open1,
# close1, open2, close2, ...]. Matching brackets enclosed within others are
# removed, and can be found by iterating over the inner portion once the outer
# brakcets have been identified.
bracket_sequences_one_line <- function (x,
                                        open_sym = "\\(",
                                        close_sym = "\\)") {

    br_open <- lapply (gregexpr (open_sym, x), function (i)
                       as.integer (i [i >= 0])) [[1]]
    br_closed <- lapply (gregexpr (close_sym, x), function (i)
                       as.integer (i [i >= 0])) [[1]]

    if (length (br_open) > 1 & length (br_closed) > 1) {
        while (br_open [2] < br_closed [1]) {
            br_open <- br_open [-2]
            br_closed <- br_closed [-1]
            if (length (br_open) < 2)
                break
        }
    }

    # unparseable junk lines need not have matching brackets:
    if (length (br_open) != length (br_closed)) {
        len <- min (c (length (br_open), length (br_closed)))
        br_open <- br_open [seq (len)]
        br_closed <- br_closed [seq (len)]
    }

    return (as.vector (rbind (br_open, br_closed)))
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

    brseq <- bracket_sequences (x,
                                open_sym = "\\{",
                                close_sym = "\\}",
                                both_sym = "\\{(.+)?\\}")
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
                xmid_after <- substring (xmid [index],
                                         br_end + 1,
                                         nchar (xmid [index]))
                index2 <- grep ("^\\s*$", xmid_after)
                if (length (index2) > 0) {
                    xmid [index [index2]] <- paste0 (xmid [index [index2]],
                                                     xmid [index [index2] + 1])
                    xmid <- xmid [- (index [index2] + 1)]
                }
            }

            xmid <- match_brackets (c (xstart, match_brackets (xmid), xend),
                                    curly = TRUE)

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
            x <- x [- (index + 1)]
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
                    x2 <- substring (x2, br_open [1] + 1, br_closed [1] - 1)
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

# match opening and corresponding closing curly brackets in terms of line
# numbers of input character vector, `x`.
match_curlies <- function (x) {
    opens <- vapply (gregexpr ("\\{", x), function (i) {
                if (all (i <= 0))
                    return (0L)
                else
                    return <- length (i)
                           }, integer (1))
    closes <- vapply (gregexpr ("\\}", x), function (i) {
                if (all (i <= 0))
                    return (0L)
                else
                    return <- length (i)
                           }, integer (1))
    oc <- cumsum (opens) - cumsum (closes)
    return (which (oc == 0) [1] - 1)
}
