# tools::Rd_db() re-parses a package's entire Rd database from scratch on
# every call. get_Rd_value()/get_Rd_param() are called once per traced
# parameter, so an unmemoised Rd_db() call here makes autotest_package()
# scale very badly (observed multi-GB/multi-minute blowups) for packages
# with many documented topics, such as base 'stats'.
m_rd_db <- memoise::memoise (function (package, dir = NULL) {
    if (is.null (dir)) {
        tools::Rd_db (package = package)
    } else {
        tools::Rd_db (package = package, dir = dir)
    }
})

# Get the 'value' field from an Rd entry for a given package function:
get_Rd_value <- function (package, fn_name) { # nolint
    val <- NULL

    if (pkg_is_source (package)) {
        f <- file.path (package, "man", paste0 (fn_name, ".Rd"))
        suppressWarnings (
            rd <- tools::parse_Rd (f)
        )
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


    # just to check whether there is a return value:
    val <- get_Rd_metadata (rd, "value")
    if (length (val) == 0) {
        return (NULL)
    }

    # Then get actual value by converting to text:
    f <- tempfile (fileext = ".txt")
    tools::Rd2txt (rd, out = f)
    rd_txt <- gsub ("\\_\\\b", "", readLines (f))
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
        suppressWarnings (
            rd <- tools::parse_Rd (f)
        )
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
        rd, function (i) paste0 (i [-1], collapse = ""),
        character (1)
    )

    ret <- NA_character_
    if (param_name %in% params) {
        ret <- rd [which (params == param_name)]
    }
    return (ret)
}
