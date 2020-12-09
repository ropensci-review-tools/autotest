
test_single_logical <- function (this_fn, params, i) {

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
        res <- rbind (res,
                      subst_for_logical (this_fn, params, i,
                                         subst = "integer"))
    }

    p [[i]] <- "a"
    res_char <- catch_all_msgs (f, this_fn, p)
    if (null_or_not (res_char, "error")) {
        res <- rbind (res,
                      subst_for_logical (this_fn, params, i,
                                         subst = "character"))
    }

    return (res)
}

#' Construt report object from results of subsituting other kinds of parameters
#' for assumed logical parameters
#' @noRd
subst_for_logical <- function (this_fn, params, i, subst = "integer") {

    operation <- paste0 ("substitute ",
                         subst,
                         "values for logical parameter")
    content <- paste0 ("Parameter ",
                       names (params) [i],
                       " of function [",
                       this_fn,
                       "] is assumed to be logical, ",
                       "but responds to ",
                       subst,
                       " input")

    return (report_object (type = "diagnostic",
                           fn_name = this_fn,
                           parameter = names (params) [i],
                           parameter_type = "single logical",
                           operation = operation,
                           content = content))
}
