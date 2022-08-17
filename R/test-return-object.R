
#' Check (1) whether return values are documented at all; and (2) If so, whether
#' they describe the class or type of return object. The latter is currently
#' only crudely tested with a simple `grep([[Cc]lass|[Oo]bject)`.
#' @noRd
autotest_return <- function (x = NULL, ...) {
    UseMethod ("autotest_return", x)
}

autotest_return.NULL <- function (x = NULL, ...) {

    # The NULL dispatch here is different from all others, because no parameters
    # are actually tested, so results are identical to test = F, and can be
    # triggered with a dummy `autotest_obj`:
    x <- autotest_obj (package = "",
                       parameters = list (),
                       parameter_types = list (),
                       fn_name = "",
                       classes = NULL,
                       test = FALSE)

    rbind (test_return_success (),
           test_return_is_described (),
           test_return_has_class (),
           test_return_primary_val_matches_desc ())
}

autotest_return.autotest_obj <- function (x, test_data = NULL) {

    rbind (test_return_success (x, test_data),
           test_return_is_described (x, test_data),
           test_return_has_class (x, test_data),
           test_return_primary_val_matches_desc (x, test_data))
}

test_return_success <- function (x = NULL, ...) {
    UseMethod ("test_return_success", x)
}

test_return_success.NULL <- function (x = NULL, ...) {

    op <- "Check that function successfully returns an object"
    report_object (type = "dummy",
                   test_name = "return_successful",
                   parameter_type = "(return object)",
                   parameter = "(return object)",
                   operation = op)
}


test_return_success.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    ret <- test_return_success.NULL ()
    ret$fn_name <- x$fn

    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, ret)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            ret$type <- "no_test"
    }

    if (x$test) {

        # pkgs with examples for internal fns have to have their namespace
        # (re-)loaded which prompts a warning via memoise
        suppressWarnings (
                          retobj <- m_capture_return_object (x)
        )

        if (methods::is (retobj, "error")) {
            ret$type <- "error"
            ret$operation <- "error from normal operation"
            ret$content <- retobj$message
        } else {
            ret <- NULL
        }
    }

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

    suppressMessages (
        o <- utils::capture.output (
            retobj <- tryCatch (do.call (x$fn, x$params, quote = TRUE),
                                warning = function (w) w,
                                error = function (e) e)
            )
        )

    return (retobj)
}
m_capture_return_object <- memoise::memoise (capture_return_object)

test_return_is_described <- function (x = NULL, ...) {
    UseMethod ("test_return_is_described", x)
}

test_return_is_described.NULL <- function (x = NULL, ...) {

    report_object (type = "dummy",
                   test_name = "return_val_described",
                   parameter = "(return object)",
                   parameter_type = "(return object)",
                   operation = "Check that description has return value")
}

test_return_is_described.autotest_obj <- function (x, test_data = NULL, ...) { # nolint

    ret <- test_return_is_described.NULL ()
    ret$fn_name <- x$fn

    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, ret)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            ret$type <- "no_test"
    }

    if (x$test) {

        aliases <- m_fns_to_topics (package = x$package_loc)
        rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == x$fn])
        Rd_value <- get_Rd_value (package = x$package_loc, fn_name = rdname) # nolint

        if (is.null (Rd_value)) {
            ret$type <- "diagnostic"
            ret$content <- paste0 ("Function [",
                                   x$fn,
                                   "] does not specify a return value.")
        } else {
            ret <- NULL
        }
    }

    return (ret)
}

test_return_has_class <- function (x = NULL, ...) {
    UseMethod ("test_return_has_class", x)
}

test_return_has_class.NULL <- function (x = NULL, ...) {

    op <- "Check whether description of return value specifies class"
    report_object (type = "dummy",
                   test_name = "return_desc_includes_class",
                   parameter = "(return object)",
                   parameter_type = "(return object)",
                   operation = op)
}

test_return_has_class.autotest_obj <- function (x, test_data = NULL) { # nolint

    ret <- test_return_has_class.NULL ()
    ret$fn_name <- x$fn

    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, ret)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            ret$type <- "no_test"
    }

    if (x$test) {

        aliases <- m_fns_to_topics (package = x$package_loc)
        rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == x$fn])
        Rd_value <- get_Rd_value (package = x$package_loc, fn_name = rdname) # nolint

        if (is.null (Rd_value)) {

            ret$type <- "diagnostic"
            ret$content <- paste0 ("Function [",
                                   x$fn,
                                   "] does not describe return value")
        } else {

            # pkgs with examples for internal fns have to have their namespace
            # (re-)loaded which prompts a warning via memoise
            suppressWarnings (
                              retobj <- m_capture_return_object (x)
                              )

            cl <- attr (retobj, "class")
            okay <- TRUE
            if (!is.null (cl)) {
                okay <- vapply (cl, function (i)
                                any (grepl (i, Rd_value)),
                                logical (1))
            }

            if (!any (okay)) { # one okay means all okay

                ret$type <- "diagnostic"
                ret$content <- paste0 ("Function [",
                                       x$fn,
                                       "] returns a value of class [",
                                       paste0 (attr (retobj, "class"),
                                               collapse = ", "),
                                       "], which differs from the value ",
                                       "provided in the description")
            } else {
                ret <- NULL
            }
        }
    }

    return (ret)
}

test_return_primary_val_matches_desc <- function (x = NULL, ...) { # nolint
    UseMethod ("test_return_primary_val_matches_desc", x)
}

test_return_primary_val_matches_desc.NULL <- function (x = NULL, ...) { # nolint

    op <- "Compare class of return value with description"
    report_object (type = "dummy",
                   test_name = "return_class_matches_desc",
                   parameter = "(return object)",
                   parameter_type = "(return object)",
                   operation = op)
}

test_return_primary_val_matches_desc.autotest_obj <- function (x, test_data = NULL) { # nolint

    ret <- test_return_primary_val_matches_desc.NULL ()
    ret$fn_name <- x$fn

    if (!is.null (test_data)) {
        test_flag <- test_these_data (test_data, ret)
        if (length (test_flag) == 1L) {
            x$test <- test_flag
        }
        if (!x$test)
            ret$type <- "no_test"
    }

    if (x$test) {

        aliases <- m_fns_to_topics (package = x$package_loc)
        rdname <- gsub ("\\.Rd$", "", aliases$name [aliases$alias == x$fn])
        Rd_value <- get_Rd_value (package = x$package_loc, fn_name = rdname) # nolint

        if (is.null (Rd_value)) {

            ret$type <- "diagnostic"
            ret$content <- paste0 ("Function [",
                                   x$fn,
                                   "] does not describe return value")
        } else {

            # pkgs with examples for internal fns have to have their namespace
            # (re-)loaded which prompts a warning via memoise
            suppressWarnings (
                              retobj <- m_capture_return_object (x)
                              )

            chk <- TRUE
            if (!is.null (attr (retobj, "class"))) {
                chk <- vapply (attr (retobj, "class"), function (i)
                               any (grepl (i, Rd_value)),
                               logical (1))
            }

            if (!any (chk)) {

                txt <- compare_return_classes (Rd_value, retobj)

                if (!is.null (txt)) {
                    ret$type <- "diagnostic"
                    ret_tmp <- NULL
                    for (i in txt) {
                        ret$content <- i
                        ret_tmp <- rbind (ret_tmp, ret)
                    }
                    ret <- ret_tmp
                } else {
                    ret <- NULL
                }
            } else {
                ret <- NULL
            }
        }
    } # end if x$test

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
            if (grepl ("\\s", desc_class))
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
