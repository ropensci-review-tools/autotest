
# Check (1) whether return values are documented at all; and (2) If so, whether
# they describe the class or type of return object. The latter is currently only
# crudely tested with a simple `grep([[Cc]lass|[Oo]bject)`.
autotest_return <- function (pkg, params, this_fn, package = NULL) {

    # package is !NULL when passed as installed package location from
    # autotest_package. This is used in `get_Rd_value`
    if (is.null (package))
        package <- pkg

    ret <- NULL
    f <- file.path (tempdir (), "junk.txt")

    null_params <- NULL
    if (any (params == "NULL")) {
        null_params <- params [params == "NULL"]
        params <- params [params != "NULL"]
    }

    msgs <- catch_all_msgs (f, this_fn, params)
    ret <- add_msg_output (ret, msgs, types = "warning",
                           operation = "normal function call")

    retval <- tryCatch (do.call (this_fn, params),
                        warning = function (w) w,
                        error = function (e) e)
    if (methods::is (retval, "error")) {
        ret <- rbind (ret,
                      report_object (type = "error",
                                     fn_name = this_fn,
                                     parameter = NA_character_,
                                     operation = "error from normal operation",
                                     content = retval$message))
        return (ret)
    }

    if (!is.null (attr (retval, "class"))) {
        Rd_value <- get_Rd_value (package = package, fn_name = this_fn)
        if (is.null (Rd_value)) {
            ret <- rbind (ret,
                          report_object (type = "diagnostic",
                                         fn_name = this_fn,
                                         parameter = NA_character_,
                                         operation = "check that description has return value",
                                         content = paste0 ("Function [",
                                                           this_fn,
                                                           "] does not specify a return value, ",
                                                           "yet returns a value of class [",
                                                           paste0 (attr (retval, "class"),
                                                                   collapse = ", "),
                                                           "]")))
        } else {
            chk <- any (grepl ("[Cc]lass|[Oo]bject", Rd_value))
            if (!chk)
                chk <- any (grepl ("vector|data(\\.?)frame", Rd_value),
                            ignore.case = TRUE)
            if (!chk)
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = this_fn,
                                             parameter = NA_character_,
                                             operation = "compare class of return value with description",
                                             content = paste0 ("Function [",
                                                               this_fn,
                                                               "] does not specify class of ",
                                                               "return value, yet returns a ",
                                                               "value of class [",
                                                               paste0 (attr (retval, "class"),
                                                                       collapse = ", "),
                                                               "]")))
        }
    }

    return (ret)
}
