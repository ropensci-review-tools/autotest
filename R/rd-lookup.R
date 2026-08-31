m_rd_db <- memoise::memoise (function (package, dir = NULL) {
    if (is.null (dir)) {
        tools::Rd_db (package = package)
    } else {
        tools::Rd_db (package = package, dir = dir)
    }
})

get_Rd_value <- function (package, fn_name) { # nolint
    val <- NULL

    if (pkg_is_source (package)) {
        f <- file.path (package, "man", paste0 (fn_name, ".Rd"))
        rd <- suppressWarnings (tools::parse_Rd (f))
    } else {
        if (basename (package) == package) {
            x <- m_rd_db (package = package)
        } else {
            # packages installed into local tempdir via covr:
            x <- m_rd_db (
                package = basename (package),
                dir = package
            )
        }
        rd <- x [[paste0 (fn_name, ".Rd")]]
    }


    val <- get_Rd_metadata (rd, "value")
    if (length (val) == 0) {
        return (NULL)
    }

    # Convert actual value to text:
    f <- tempfile (fileext = ".txt")
    tools::Rd2txt (rd, out = f)
    # backspace character constructed at runtime (rather than embedded
    # literally as an escape) because a literal backspace byte in this file
    # corrupts source-position tracking in 'lintr's fixed_regex_linter,
    # crashing the linter on later lines.
    rd_txt <- gsub (paste0 ("\\_\\", intToUtf8 (8)), "", readLines (f))
    sec_index <- grep ("^[[:alpha:]].*:$", rd_txt)
    i0 <- grep ("^Value:$", rd_txt)
    if (i0 == max (sec_index)) {
        index <- (i0 + 1):length (rd_txt)
    } else {
        i1 <- sec_index [which (sec_index > i0) [1]]
        index <- (i0 + 1):(i1 - 1)
    }
    val <- rd_txt [index]

    return (val)
}

get_Rd_param <- function (package, fn_name, param_name) { # nolint

    a <- m_fns_to_topics (package = package)

    if (pkg_is_source (package)) {

        f <- file.path (
            package,
            "man",
            a$name [a$alias == fn_name]
        )
        rd <- suppressWarnings (tools::parse_Rd (f))
    } else {

        if (basename (package) == package) {

            x <- m_rd_db (package = package)
        } else {

            # packages installed into local tempdir via covr:
            x <- m_rd_db (
                package = basename (package),
                dir = package
            )
        }

        rd <- x [[a$name [a$alias == fn_name]]]
    }

    index <- vapply (
        rd, function (i) {
            attr (i, "Rd_tag") == "\\arguments"
        },
        logical (1)
    )
    index <- which (index)
    if (length (index) == 0) {
        return (NULL)
    } else if (length (index) == 1) {
        rd <- rd [[index]]
    } else {
        rd <- rd [index]
    }

    # rm just list items that are just line breaks
    len <- vapply (rd, length, integer (1))
    rd <- rd [which (len > 1)]
    rd <- lapply (rd, unlist)
    params <- vapply (rd, function (i) i [1], character (1))
    rd <- vapply (
        rd, function (i) paste (i [-1], collapse = ""),
        character (1)
    )

    ret <- NA_character_
    if (param_name %in% params) {
        ret <- rd [which (params == param_name)]
    }
    return (ret)
}
