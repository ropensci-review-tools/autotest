test_single_name <- function (pkg, this_fn, params, i, test = TRUE) {

    this_class <- class (params [[i]]) [1]
    operation <- paste0 ("(unquoted) ",
                         this_class,
                         " param as (quoted) character")
    res <- report_object (type = "dummy",
                          fn_name = this_fn,
                          parameter = names (params) [i],
                          parameter_type = "single formula or name",
                          operation = operation)

    if (test) {

        f <- tempfile (fileext = ".txt")

        params [[i]] <- as.character (params [[i]])
        msgs <- catch_all_msgs (f, this_fn, params_i)
        if (!is.null (msgs)) {
            res$type <- msgs$type
            res$content <- msgs$content
        } else {
            res <- NULL
        }
    }

    return (res)
}
