
# logical test whether a report data.frame is null, or whether it does not
# contain expected type ("not_this"). A result might, for example, be expected
# to be an error, so testing `null_or_not("error")` will give `TRUE` if no error
# occurs.
null_or_not <- function (x, not_this) {
    res <- is.null (x)
    if (!res) {
        for (i in not_this) {
            res <- c (res, i %in% x$type)
        }
    }

    return (!any (res))
}

not_null_and_is <- function (x, is_this) {
    res <- FALSE
    if (!is.null (x))
        if (is_this %in% x$type)
            res <- TRUE
    return (res)
}

report_object <- function (type = "diagnostic",
                           fn_name = NA_character_,
                           parameter = NA_character_,
                           operation = NA_character_,
                           content = NA_character_) {
    data.frame (type = type,
                fn_name = fn_name,
                parameter = parameter,
                operation = operation,
                content = content,
                stringsAsFactors = FALSE)
}

# same criteria as rprojroot::is_r_package, but without extra dependency.
pkg_is_source <- function (package) {
    is_source <- FALSE
    if (file.exists (package)) {
        if (file.exists (file.path (package, "DESCRIPTION"))) {
            desc <- readLines (file.path (package, "DESCRIPTION"))
            if (any (grepl ("^Package\\:", desc)))
                is_source <- TRUE
        }
    }
    return (is_source)
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

get_git_hash <- function (package) {
    wd <- setwd (package)
    x <- system2 ("git", c ("log", "-1"), stdout = TRUE) [1]
    setwd (wd)
    return (gsub ("commit\\s+", "", x))
}
