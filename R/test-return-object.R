
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
        #params <- m_get_param_lists (package)
        #classes <- pkg_param_classes (package)
        aliases <- m_fns_to_topics (package = package)
        rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == this_fn])
        Rd_value <- get_Rd_value (package = package, fn_name = rdname)
        chk <- vapply (attr (retval, "class"), function (i)
                       grepl (i, Rd_value),
                       logical (1))
        if (!any (chk)) {
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
            txt <- NULL
            #chk <- any (grepl ("[Cc]lass|[Oo]bject", Rd_value))
            # Get class of returned object, along with matched value from man
            # entry
            retclasses <- attr (retval, "class")
            r <- gregexpr (paste0 (retclasses, collapse = "|"), Rd_value)
            i <- vapply (r, function (i) i [1] > 0, logical (1))
            if (any (i)) {
                # can be multiple return classes, so match the first one from
                # descr with the returned object
                r_i <- r [which (i)]
                Rd_i <- Rd_value [which (i)]
                desc_classes <- lapply (seq_along (which (i)), function (j) {
                    pos1 <- as.integer (r_i [[j]])
                    pos2 <- as.integer (r_i [[j]] + attr (r_i [[j]], "match.length") - 1)
                    substring (Rd_i [j], pos1, pos2)     })
                desc_classes <- unique (unlist (desc_classes))

                actual_class <- retclasses [which (retclasses %in% desc_classes)]

                if (actual_class [1] != retclasses [1]) {
                    txt <- paste0 ("Function returns an object of primary class [",
                                   retclasses [1],
                                   "] yet documentation says value is of class [",
                                   paste0 (desc_classes, collapse = ", "),
                                   "]")
                }

            } else {
                retclasses_mod <- gsub ("\\_|\\.", "", retclasses)
                r <- gregexpr (paste0 (retclasses_mod, collapse = "|"), Rd_value)
                i <- vapply (r, function (i) i [1] > 0, logical (1))
                if (any (i)) {
                    r_i <- r [[which (i)]]
                    desc_class <- substring (Rd_value [i], r_i, nchar (Rd_value [i]))
                    desc_class <- substring (desc_class, 1, regexpr ("\\s+", desc_class) - 1)
                    actual_class <- retclasses [which (retclasses_mod == desc_class)]

                    txt <- paste0 ("Function returns an object of class [",
                                   actual_class,
                                   "] yet documentation describes class of value as [",
                                   desc_class,
                                   "]")

                    if (actual_class != retclasses [1])
                        txt <- c (txt, paste0 ("Function returns an object of primary class [",
                                               retclasses [1],
                                               "] yet documentation says value is of class [",
                                               desc_class,
                                               "]"))
                }
            }

            if (!is.null (txt))
                for (i in txt) {
                    ret <- rbind (ret,
                                  report_object (type = "diagnostic",
                                                 fn_name = this_fn,
                                                 parameter = NA_character_,
                                                 operation = "compare class of return value with description",
                                                 content = i))
                }
        }
    }

    return (ret)
}
