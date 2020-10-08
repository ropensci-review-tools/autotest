
get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools")

test_single_char <- function (pkg, this_fn, params, i) {

    h <- tools::Rd_db (package = pkg)
    is_source <- FALSE
    if (length (h) == 0) {
        is_source <- TRUE
    } else {
        h <- h [grep (paste0 ("^", this_fn, "\\.Rd$"), names (h))]
        if (length (h) != 1) {
            message ("No single help topic for [", this_fn, "] found")
            return (NULL)
        }
    }

    # check whether vectors of 2 characters error or warn:
    p <- params
    p [[i]] <- rep (p [[i]], 2)
    f <- file.path (tempdir (), "junk.txt")
    msgs <- catch_all_msgs (f, this_fn, p)

    if (is.null (msgs)) {
        msgs <- report_object (type = "diagnostic",
                               fn_name = this_fn,
                               parameter = names (params) [i],
                               operation = "length 2 vector for length 1 param",
                               content = paste0 ("Parameter [",
                                                 names (params) [i],
                                                 "] of function [", this_fn,
                                                 "] is assumed to be a single ",
                                                 "character, but responds to ",
                                                 "vectors of length > 1"))
    } else
        msgs <- NULL

    # check case sensitivity:
    p <- params
    p [[i]] <- tolower (p [[i]])
    these_msgs <- catch_all_msgs (f, this_fn, p)
    if (!is.null (these_msgs)) {
        msgs <- rbind (msgs,
                       report_object (type = "diagnostic",
                                      fn_name = this_fn,
                                      parameter = names (params) [i],
                                      operation = "lower case character parameter",
                                      content = paste0 ("Parameter [",
                                                        names (params) [i],
                                                        "] of function [", this_fn,
                                                        "] is assumed to be a single ",
                                                        "character, but is ",
                                                        "case dependent")))
    }
    p [[i]] <- toupper (p [[i]])
    these_msgs <- catch_all_msgs (f, this_fn, p)
    if (!is.null (these_msgs)) {
        msgs <- rbind (msgs,
                       report_object (type = "diagnostic",
                                      fn_name = this_fn,
                                      parameter = names (params) [i],
                                      operation = "upper case character parameter",
                                      content = paste0 ("Parameter [",
                                                        names (params) [i],
                                                        "] of function [", this_fn,
                                                        "] is assumed to be a single ",
                                                        "character, but is ",
                                                        "case dependent")))
    }

    # check whether match.arg is used
    p [[i]] <- paste0 (sample (c (letters, LETTERS), size = 10), collapse = "")
    these_msgs <- catch_all_msgs (f, this_fn, p)
    if (!"error" %in% these_msgs$type) {
        msgs <- rbind (msgs,
                       report_object (type = "diagnostic",
                                      fn_name = this_fn,
                                      parameter = names (params) [i],
                                      operation = "random character string as parameter",
                                      content = paste0 ("Parameter [",
                                                        names (params) [i],
                                                        "] of function [", this_fn,
                                                        "] is assumed to be a single ",
                                                        "character, but does not ",
                                                        "match arguments to ",
                                                        "expected values")))
    }

    if (is_source)
        return (msgs) # TODO: Remove that and process the remainder for src pkgs

    # The following lines are used just to test whether params[[i]] corresponds
    # to a formula input. They can't be used to parse general parameter
    # descriptions, because `parse_exprs` converts all quoted items into list
    # items, leaving no direct way of identifying which items may have been
    # quoted.
    #a <- rlang::parse_exprs (tools:::.Rd_get_metadata (h, "arguments"))
    a <- rlang::parse_exprs (get_Rd_metadata (h, "arguments"))
    ## also .Rd_get_metadata ("title", "name", "description", "value", ...)
    arg_names <- unlist (lapply (a, function (i) eval (i) [[1]] [[1]]))
    arg_descs <- lapply (a, function (i) unlist (eval (i) [[2]]))
    arg_name <- arg_names [i]
    arg_desc <- arg_descs [i]
    if (grepl ("~", params [[i]]) | any (grepl ("formula", arg_desc)))
        return (msgs) # do not test formulas

    # regex the actual string to extract all item descriptions. These are formatted
    # "\\item { <name> } { <description> }
    # start by removing the { <name> } portion
    hc <- paste0 (paste (h), collapse = " ")
    index <- gregexpr ("\\\\item\\s+\\{(.*?)\\}", hc) [[1]]
    #index <- gregexpr ("\\\\item\\s+\\{.*\\}\\s+\\{(.*?)\\}", hc) [[1]]
    i <- as.integer (index)
    j <- i + attr (index, "match.length")
    rms <- gsub ("}", "\\}", gsub ("\\item\\s+", "", substring (hc, i, j)), fixed = TRUE)

    # these don't necessary match the end brackets, so need extension where not
    n_open <- vapply (gregexpr ("\\{", rms), length, integer (1))
    n_closed <- vapply (gregexpr ("\\}", rms), length, integer (1))
    index <- which (n_closed < n_open)
    for (k in index) {
        pos <- gregexpr ("\\}", substring (hc, j [k], nchar (hc))) [[1]]
        #j [k] <- j [k] + pos [n_open [k] - n_closed [k]]
        add <- substring (hc, j [k] + 1, j [k] + pos [n_open [k] - n_closed [k]])
        add <- gsub ("}", "\\}", add, fixed = TRUE)
        add <- gsub (":", "\\:", add, fixed = TRUE)
        rms [k] <- paste0 (rms [k], add)
        rms [k] <- gsub (" {", " \\{", rms [k], fixed = TRUE)
    }

    for (i in rms)
        hc <- gsub (i, "", hc)

    # The remaining values between the curly braces are the descriptions, but
    # this requires finding matching curly braces.
    index <- gregexpr ("\\\\item\\s+\\{(.*?)\\}", hc) [[1]]
    #index <- gregexpr ("\\\\item\\s+\\{([^}]*)\\}", hc) [[1]]
    i <- as.integer (index)
    j <- i + attr (index, "match.length")
    res <- substring (hc, i, j)
    # extend the k'th element of res to match the closing curly brace
    match_res_k <- function (res, hc, i, j, k) {
        rk <- res [k]
        #(length (gregexpr ("\\{", rk) [[1]]) !=
        #    length (gregexpr ("\\}", rk) [[1]])) 
        if (length (gregexpr ("\\{", rk) [[1]]) !=
            length (gregexpr ("\\}", rk) [[1]])) {
            this_i <- i [k] + 1
            if (k < length (i))
                this_j <- i [k + 1] - 1
            else
                this_j <- nchar (hc)
            opens <- gregexpr ("\\{", substring (hc, this_i, this_j)) [[1]]
            closes <- gregexpr ("\\}", substring (hc, this_i, this_j)) [[1]]
            if (length (opens) < length (closes)) {
                index <- which (opens < closes [seq (length (opens))])
            } else {
                index <- which (opens [seq (closes)] < closes)
            }
            if (length (index) < length (closes)) {
                closes <- closes [which (!seq (closes) %in% index)] [1]
            } else {
                closes <- closes [length (closes)]
            }
            rk <- substring (hc, i [k], this_i + closes + 1)
        }
        return (rk)
    }
    for (k in seq (res))
        res [k] <- match_res_k (res, hc, i, j, k)

    return (msgs)
}
