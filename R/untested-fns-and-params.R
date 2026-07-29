#' test_fns_wo_examples
#'
#' Get functions which do not have examples and return as an autotest object.
#' @noRd
test_fns_wo_example <- function (package = NULL, res, fn_names) {
    UseMethod ("test_fns_wo_example", package)
}

#' @exportS3Method
test_fns_wo_example.NULL <- function (package = NULL, res, fn_names) {

    report_object (
        type = "dummy",
        test_name = "fn_without_example",
        operation = "Identify functions without documented examples"
    )
}

#' @exportS3Method
test_fns_wo_example.character <- function (package, res, fn_names) {

    r0 <- test_fns_wo_example.NULL ()

    no_examples <- fns_without_examples (package) # in namespace-processing
    no_examples <- no_examples [which (no_examples %in% fn_names)]

    if (length (no_examples) > 0) {
        r0$type <- "warning"
        r0$content <- "This function has no documented example"
        for (i in no_examples) {
            r0$fn_name <- i
            res <- rbind (res, r0)
        }
    }

    return (res)
}

#' untested_params
#'
#' For every function represented in `example_fn_pars`, identify any formal
#' parameters (other than `...`) never demonstrated by name in any
#' example-sourced 'typetracer' trace.
#' @param package Name or path of the package being tested, used to look up
#' function formals.
#' @param example_fn_pars Result of \link{get_example_fn_pars}: a
#' `data.frame` of `fn_name`/`par_name` combinations demonstrated in
#' example-sourced traces.
#' @return Named `list`, one entry per function with untested parameters,
#' each holding the names of parameters never demonstrated by that function's
#' examples.
#' @noRd
untested_params <- function (package, example_fn_pars) {

    if (is.null (example_fn_pars) || nrow (example_fn_pars) == 0) {
        return (NULL)
    }

    pkg_name <- get_package_name (package)
    fns <- unique (example_fn_pars$fn_name)

    fmls <- lapply (fns, function (f) {
        fn <- utils::getFromNamespace (f, pkg_name)
        fn_formals <- names (formals (fn))
        these_pars <- example_fn_pars$par_name [example_fn_pars$fn_name == f]
        index <- which (!fn_formals %in% these_pars & fn_formals != "...")
        fn_formals [index]
    })
    names (fmls) <- fns

    fmls <- fmls [which (vapply (fmls, length, integer (1)) > 0)]

    return (fmls)
}

test_untested_params <- function (example_fn_pars = NULL, ...) {
    UseMethod ("test_untested_params", example_fn_pars)
}

#' @exportS3Method
test_untested_params.NULL <- function (example_fn_pars = NULL, ...) {

    report_object (
        type = "dummy",
        test_name = "par_is_demonstrated",
        content = paste0 (
            "Examples do not demonstrate ",
            "usage of this parameter"
        ),
        operation = "Check that parameter usage is demonstrated"
    )
}

#' @exportS3Method
test_untested_params.data.frame <- function (example_fn_pars = NULL,
                                             package = NULL,
                                             res_in = NULL, ...) {

    pars <- untested_params (package, example_fn_pars)

    res <- lapply (seq_along (pars), function (i) {
        ro <- test_untested_params.NULL ()
        ro <- ro [rep (1, length (pars [[i]])), ]
        ro$type <- "warning"
        ro$fn_name <- names (pars) [i]
        ro$parameter <- pars [[i]]
        return (ro)  })

    res <- do.call (rbind, res)

    return (rbind (res_in, res))
}

# no method dispatch for these
undocumented_params_NULL <- function () { # nolint

    report_object (
        type = "dummy",
        test_name = "par_is_documented",
        content = "Examples do not document this parameter",
        operation = "Check that parameter is documented"
    )
}

param_docs_match_input_NULL <- function (this_class) { # nolint

    report_object (
        type = "dummy",
        test_name = "par_matches_docs",
        operation = paste0 (
            "Check that documentation matches ",
            "class of input parameter"
        )
    )
}


test_param_documentation <- function (x = NULL, ...) {

    UseMethod ("test_param_documentation", x)
}

#' @exportS3Method
test_param_documentation.NULL <- function (x = NULL, ...) {

    rbind (
        undocumented_params_NULL (),
        param_docs_match_input_NULL ()
    )
}

#' @exportS3Method
test_param_documentation.autotest_obj <- function (x) { # nolint

    if (x$test) {
        ret <- test_param_docs_test (x)
    } else {
        ret <- test_param_docs_notest (x)
    }

    return (ret)
}

test_param_docs_notest <- function (x) {

    ret <- NULL

    for (p in seq_along (x$params)) {

        this_ret <- undocumented_params_NULL ()
        this_ret$fn_name <- x$fn
        this_ret$parameter <- names (x$params) [p]
        this_ret$content <- NA_character_
        ret <- rbind (ret, this_ret)

        this_ret <- param_docs_match_input_NULL ()
        this_ret$fn_name <- x$fn
        this_ret$parameter <- names (x$params) [p]
        ret <- rbind (ret, this_ret)
    }

    return (ret)
}

test_param_docs_test <- function (x) {

    ret <- NULL

    for (p in seq_along (x$params)) {

        x$i <- p
        rd_desc <- get_Rd_param (
            x$package_loc,
            x$fn,
            names (x$params) [x$i]
        )

        if (is.na (rd_desc)) {

            this_ret <- undocumented_params_NULL ()
            this_ret$type <- "warning"
            this_ret$fn_name <- x$fn
            this_ret$parameter <- names (x$params) [x$i]
            ret <- rbind (ret, this_ret)

        } else if (x$param_types [p] == "tabular" | is.na (x$param_types [p])) {

            this_class <- class (x$params [[p]])
            class_in_desc <- vapply (
                this_class, function (i) {
                    grepl (i, rd_desc)
                },
                logical (1)
            )
            if (!any (class_in_desc)) {

                this_ret <- param_docs_match_input_NULL ()
                this_ret$content <- paste0 (
                    "Parameter documentation does ",
                    "not describe class of [",
                    paste0 (this_class, collapse = ", "),
                    "]"
                )
                this_ret$type <- "warning"
                this_ret$fn_name <- x$fn
                this_ret$parameter <- names (x$params) [x$i]
                ret <- rbind (ret, this_ret)
            }
        }
    }

    return (ret)
}
