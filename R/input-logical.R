
test_single_logical <- function (pkg, this_fn, params, i) {

    res <- NULL

    f <- file.path (tempdir (), "junk.txt")
    res <- rbind (res,
                  catch_all_msgs (f, this_fn, params))
    p <- params
    p [[i]] <- !p [[i]]
    res <- rbind (res,
                  catch_all_msgs (f, this_fn, p))

    for (j in 0L:2L) {
        p [[i]] <- j
        res_ints <- catch_all_msgs (f, this_fn, params)
    }
    if (is.null (res_ints)) {
        operation <- "substitute integer values for logical parameter"
        content <- paste0 ("Parameter ",
                           names (params) [i],
                           " of function [",
                           this_fn,
                           "] is assumed to be logical, ",
                           "but responds to ",
                           "general integer values.")
        res <- rbind (res,
                      report_object (type = "diagnostic",
                                     fn_name = this_fn,
                                     parameter = names (params) [i],
                                     parameter_type = "single logical",
                                     operation = operation,
                                     content = content))
    }

    p [[i]] <- "a"
    res_char <- catch_all_msgs (f, this_fn, p)
    if (null_or_not (res_char, "error")) {
        operation <- "substitute character values for logical parameter"
        content <- paste0 ("Parameter ",
                           names (params) [i],
                           " of function [",
                           this_fn,
                           "] is assumed to be logical, ",
                           "but responds to ",
                           "character input")
        res <- rbind (res,
                      report_object (type = "diagnostic",
                                     fn_name = this_fn,
                                     parameter = names (params) [i],
                                     parameter_type = "single logical",
                                     operation = operation,
                                     content = content))
    }

    return (res)
}
