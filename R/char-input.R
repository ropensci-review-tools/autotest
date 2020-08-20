
test_single_char <- function (pkg, this_fn, params, i) {

    h <- tools::Rd_db (package = pkg)
    h <- h [grep (paste0 ("^", this_fn, "\\.Rd$"), names (h))]
    if (length (h) != 1) {
        stop ("No single help topic for [", this_fn, "] found")
    }
    h <- h [[1]]

    # this does not work, because `parse_exprs` converts all quoted items into
    # list items, leaving no direct way of identifying which items may have been
    # quoted.
    #a <- rlang::parse_exprs (tools:::.Rd_get_metadata (h, "arguments"))
    ## also .Rd_get_metadata ("title", "name", "description", "value", ...)
    #arg_names <- unlist (lapply (a, function (i) eval (i) [[1]] [[1]]))
    #arg_descs <- lapply (a, function (i) unlist (eval (i) [[2]]))

    # regex the actual string to extract all item descriptions. These are formatted
    # "\\item { <name> } { <description> }
    # start by removing the { <name> } portion
    hc <- paste0 (paste (h), collapse = " ")
    index <- gregexpr ("\\\\item\\s+\\{(.*?)\\}", hc) [[1]]
    #index <- gregexpr ("\\\\item\\s+\\{.*\\}\\s+\\{(.*?)\\}", hc) [[1]]
    i <- as.integer (index)
    j <- i + attr (index, "match.length")
    rms <- gsub ("}", "\\}", gsub ("\\item\\s+", "", substring (hc, i, j)), fixed = TRUE)
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
}
