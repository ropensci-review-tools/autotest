
get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools")

test_single_char <- function (pkg, this_fn, params, i) {

    chk <- TRUE

    h <- tools::Rd_db (package = pkg)
    h <- h [grep (paste0 ("^", this_fn, "\\.Rd$"), names (h))]
    if (length (h) != 1) {
        stop ("No single help topic for [", this_fn, "] found")
    }
    h <- h [[1]]

    get1 <- function (val) {
        params [[i]] <- val
        tryCatch (
                  utils::capture.output (do.call (this_fn, params)),
                  error = function (e) e,
                  warning = function (w) w)
    }

    # check whether vectors of 2 characters error or warn:
    val0 <- get1 (params [[i]])
    val1 <- get1 (rep (params [[i]], 2))
    if (!(any (methods::is (val1, "error")) | methods::is (val1, "warning"))) {
        message ("Parameter ", names (params) [i], " of function [",
                 this_fn, "] is assumed to a single character, ",
                 "but responds to vectors of length > 1")
        chk <- FALSE
    }

    # check case sensitivity:
    val1 <- get1 (tolower (params [[i]]))
    val2 <- get1 (toupper (params [[i]]))
    if (any (methods::is (val1, "error")) | methods::is (val1, "warning") |
        methods::is (val2, "error") | methods::is (val2, "warning")) {
        message ("Parameter [", names (params) [i], "] of function [",
                 this_fn, "] is assumed to a single character, ",
                 "but is case dependent")
        chk <- FALSE
    }

    # check whether match.arg is used
    val1 <- get1 (paste0 (sample (c (letters, LETTERS), size = 10), collapse = ""))
    if (!methods::is (val1, "error")) {
        message ("Parameter ", names (params) [i], " of function [",
                 this_fn, "] is assumed to a single character, ",
                 "but does not match arguments to expected values")
        chk <- FALSE
    }

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
        return (TRUE) # do not test formulas

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

    return (chk)
}
