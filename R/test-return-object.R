
# Check (1) whether return values are documented at all; and (2) If so, whether
# they describe the class or type of return object. The latter is currently only
# crudely tested with a simple `grep([[Cc]lass|[Oo]bject)`.
autotest_return <- function (x) {

    ret <- test_return_object (x)

    return (ret)
}

test_return_object <- function (x) {

    ret <- test_return_success (x)
    ret <- rbind (ret,
                  test_return_class (x))
    ret <- rbind (ret,
                  test_return_is_described (x))
    ret <- rbind (ret,
                  test_return_val_matches_desc (x))
    ret <- rbind (ret,
                  test_return_primary_val_matches_desc (x))

    return (ret)
}

capture_return_object <- function (x) {

    ret <- NULL
    f <- tempfile (fileext = ".txt")

    if (any (x$params == "NULL")) {
        x$params <- x$params [x$params != "NULL"]
    }

    msgs <- catch_all_msgs (f, x$fn, x$params)
    ret <- add_msg_output (ret, msgs, types = "warning",
                           operation = "normal function call")

    o <- utils::capture.output (
        retobj <- tryCatch (do.call (x$fn, x$params),
                            warning = function (w) w,
                            error = function (e) e)
        )

    return (retobj)
}
m_capture_return_object <- memoise::memoise (capture_return_object)

test_return_success <- function (x = NULL, ...) {
    UseMethod ("test_return_success", x)
}

test_return_success.NULL <- function (x = NULL) {

    report_object (type = "dummy",
                   fn_name = x$fn,
                   parameter = "(return object)",
                   operation = paste0 ("Check that function successfully ",
                                       "returns an object"))
}

test_return_success.autotest_obj <- function (x, ...) { # nolint

    ret <- NULL

    retobj <- m_capture_return_object (x)

    if (methods::is (retobj, "error")) {
        ret <- report_object (type = "error",
                              fn_name = x$fn,
                              parameter = "(return object)",
                              operation = "error from normal operation",
                              content = retobj$message)
    }

    return (ret)
}

test_return_class <- function (x = NULL, ...) {
    UseMethod ("test_return_class", x)
}

test_return_class.NULL <- function (x = NULL) {
    report_object (type = "dummy",
                   fn_name = x$fn,
                   parameter = "(return object)",
                   operation = "Check that description has return value")
}

test_return_class.autotest_obj <- function (x, ...) {

    ret <- NULL

    retobj <- m_capture_return_object (x)

    if (!is.null (attr (retobj, "class"))) {
        ret <- test_return_is_described (x, retobj)
    }

    return (ret)
}

test_return_is_described <- function (x = NULL, ...) {
    UseMethod ("test_return_is_described", x)
}

test_return_is_described.NULL <- function (x = NULL, ...) {

    op1 <- "Check whether description of return value specifies class"
    op2 <- "Compare class of return value with class given in description"

    report_object (type = "dummy",
                   fn_name = x$fn,
                   parameter = "(return object)",
                   operation = c (op1, op2))
}

test_return_is_described.autotest_obj <- function (x = NULL, retobj, ...) { # nolint

    aliases <- m_fns_to_topics (package = x$package_loc)
    rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == x$fn])
    Rd_value <- get_Rd_value (package = x$package_loc, fn_name = rdname) # nolint

    ret <- NULL

    if (is.null (Rd_value)) {

        operation <- "Check that description has return value"
        content <- paste0 ("Function [",
                           x$fn,
                           "] does not specify a return value.")
        ret <- report_object (type = "warning",
                              fn_name = x$fn,
                              parameter = "(return object)",
                              operation = operation,
                              content = content)
    }

    return (ret)
}

test_return_val_matches_desc <- function (x, ...) {
    UseMethod ("test_return_val_matches_desc", x)
}

test_return_val_matches_desc.autotest_obj <- function (x) { # nolint

    ret <- NULL

    aliases <- m_fns_to_topics (package = x$package_loc)
    rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == x$fn])
    Rd_value <- get_Rd_value (package = x$package_loc, fn_name = rdname) # nolint

    retobj <- m_capture_return_object (x)

    chk <- vapply (attr (retobj, "class"), function (i)
                   grepl (i, Rd_value),
                   logical (1))

    if (!any (chk)) {

        operation <- "Check that description has return value"
        content <- paste0 ("Function [",
                           x$fn,
                           "] returns a value of class [",
                           paste0 (attr (retobj, "class"), collapse = ", "),
                           "], which differs from the value ",
                           "provided in the description")
        ret <- report_object (type = "diagnostic",
                              fn_name = x$fn,
                              parameter = "(return object)",
                              operation = operation,
                              content = content)
    }

    return (ret)
}

test_return_primary_val_matches_desc <- function (x, ...) { # nolint
    UseMethod ("test_return_primary_val_matches_desc", x)
}

test_return_primary_val_matches_desc.autotest_obj <- function (x) { # nolint

    ret <- NULL

    retobj <- m_capture_return_object (x)

    aliases <- m_fns_to_topics (package = x$package_loc)
    rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == x$fn])
    Rd_value <- get_Rd_value (package = x$package_loc, fn_name = rdname) # nolint

    retobj <- m_capture_return_object (x)

    chk <- vapply (attr (retobj, "class"), function (i)
                   grepl (i, Rd_value),
                   logical (1))

    if (any (chk)) {

        txt <- compare_return_classes (Rd_value, retobj)

        if (!is.null (txt)) {
            operation <- "Compare class of return value with description"
            for (i in txt) {
                ret <- rbind (ret,
                              report_object (type = "diagnostic",
                                             fn_name = x$fn,
                                             parameter = "(return object)",
                                             operation = operation,
                                             content = i))
            }
        }
    }

    return (ret)
}

compare_return_classes <- function (Rd_value, retval) { # nolint

    txt <- NULL
    # Get class of returned object, along with matched value from man
    # entry
    retclasses <- attr (retval, "class")
    r <- gregexpr (paste0 (retclasses, collapse = "|"), Rd_value)
    i <- vapply (r, function (i) i [1] > 0, logical (1))

    if (any (i)) {

        # can be multiple return classes, so match the first one from
        # descr with the returned object
        r_i <- r [which (i)]
        rd_i <- Rd_value [which (i)]
        desc_classes <- lapply (seq_along (which (i)), function (j) {
                                    pos1 <- as.integer (r_i [[j]])
                                    pos2 <- as.integer (r_i [[j]] +
                                            attr (r_i [[j]],
                                                  "match.length") - 1)
                                    substring (rd_i [j], pos1, pos2)     })
        desc_classes <- unique (unlist (desc_classes))

        actual_class <- retclasses [which (retclasses %in% desc_classes)]

        if (length (actual_class) > 0 & actual_class [1] != retclasses [1]) {
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
            desc_class <- substring (desc_class, 1,
                                     regexpr ("\\s+", desc_class) - 1)
            actual_class <- retclasses [which (retclasses_mod == desc_class)]

            txt <- paste0 ("Function returns an object of class [",
                           actual_class,
                           "] yet documentation describes class of value as [",
                           desc_class,
                           "]")

            if (actual_class != retclasses [1])
                txt <- c (txt, paste0 ("Function returns an object of ",
                                       "primary class [",
                                       retclasses [1],
                                       "] yet documentation says value ",
                                       "is of class [",
                                       desc_class,
                                       "]"))
        }
    }

    return (txt)
}
