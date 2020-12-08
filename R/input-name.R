test_single_name <- function (pkg, this_fn, params, i) {

    res <- NULL

    f <- file.path (tempdir (), "junk.txt")

    params_i <- params
    this_class <- class (params [[i]]) [1]
    params_i [[i]] <- as.character (params_i [[i]])
    msgs <- catch_all_msgs (f, this_fn, params_i)
    if (!is.null (msgs)) {
        operation <- paste0 ("(unquoted) ",
                             this_class,
                             " param as (quoted) character")
        res <- rbind (res,
                      report_object (type = msgs$type,
                                     fn_name = msgs$fn_name,
                                     parameter = names (params) [i],
                                     parameter_type = "single formula or name",
                                     operation = operation,
                                     content = msgs$content))
    }

    return (res)
}
