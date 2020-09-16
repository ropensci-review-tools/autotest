
# logical test whether a report data.frame is null, or whether it does not
# contain expected type ("not_this"). A result might, for example, be expected
# to be an error, so testing `null_or_not("error")` will give `TRUE` if no error
# occurs.
null_or_not <- function (x, not_this) {
    res <- is.null (x)
    if (!res) {
        for (i in not_this) {
            res <- c (res, !i %in% x$type)
        }
    }

    return (any (res))
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
                           content = NA_character_) {
    data.frame (type = type,
                fn_name = fn_name,
                parameter = parameter,
                content = content,
                stringsAsFactors = FALSE)
}
