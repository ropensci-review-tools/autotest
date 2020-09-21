test_single_name <- function (pkg, this_fn, params, i) {

    res <- NULL

    f <- file.path (tempdir (), "junk.txt")

    params_i <- params
    params_i [[i]] <- as.character (params_i [[i]])
    msgs <- catch_all_msgs (f, this_fn, params_i)
    if (!is.null (msgs)) {
        res <- rbind (res,
                      report_object (type = msgs$type,
                                     fn_name = msgs$fn_name,
                                     parameter = names (params) [i],
                                     content = msgs$content))
    }

    return (res)
}
