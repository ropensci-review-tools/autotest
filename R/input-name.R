test_single_name <- function (x) {

    this_class <- class (x$params [[x$i]]) [1]
    operation <- paste0 ("(unquoted) ",
                         this_class,
                         " param as (quoted) character")
    res <- report_object (type = "dummy",
                          fn_name = x$fn,
                          parameter = names (x$params) [x$i],
                          parameter_type = "single formula or name",
                          operation = operation)

    if (test) {

        f <- tempfile (fileext = ".txt")

        x$params [[x$i]] <- as.character (x$params [[x$i]])
        msgs <- catch_all_msgs (f, x$fn, x$params)
        if (!is.null (msgs)) {
            res$type <- msgs$type
            res$content <- msgs$content
        } else {
            res <- NULL
        }
    }

    return (res)
}
