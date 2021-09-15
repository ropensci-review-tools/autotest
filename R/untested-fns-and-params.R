
#' test_fns_wo_examples
#'
#' Get functions which do not have examples and return as an autotest object.
#' @noRd
test_fns_wo_example <- function (package = NULL, res, fn_names) {
    UseMethod ("test_fns_wo_example", package)
}

test_fns_wo_example.NULL <- function (package = NULL, res, fn_names) {

    report_object (type = "dummy",
                   test_name = "fn_without_example",
                   operation = "Identify functions without documented examples")
}

test_fns_wo_example.character <- function (package, res, fn_names) {

    r0 <- test_fns_wo_example.NULL ()

    no_examples <- fns_without_examples (package) # in namespace-processing
    no_examples <- no_examples [which (no_examples %in% fn_names)]

    # res will be NULL if package has no examples at all
    if (!is.null (res))
        no_examples <- no_examples [no_examples %in% unique (res$fn_name)]
    if (length (no_examples) > 0) {
        r0$type <- "warning"
        r0$content <- "This function has no documented example"
        r0$yaml_hash <- NA_character_
        for (i in no_examples) {
            r0$fn_name <- i
            res <- rbind (res, r0)
        }
    }

    return (res)
}

#' untested_params
#'
#' Takes a full list of examples for a single package and identifies any
#' parameters not able to be tested from example code. This requires the full
#' list of all examples, because it's not possible to determine from any one
#' example whether or not a parameter might be tested in some other example.
#' @noRd
untested_params <- function (exs) {

    if (length (exs) == 0)
        return (NULL) # package has no examples

    pars <- list ()

    for (i in exs) {

        package <- gsub ("package:\\s?|\\s?$", "",
                         grep ("^package:", i, value = TRUE))
        l1 <- i [grep ("^functions:", i) [1] + 1]
        sp <- gregexpr ("\\s", l1) [[1]]
        nspaces <- sp [which (diff (sp) > 1) [1]]
        ptn <- paste0 ("^[[:space:]]{", nspaces, "}-")
        index <- grep (ptn, i)

        these_fns <- gsub ("\\s+-\\s?|:$", "", i [index])

        index <- cbind (index, c (index [-1] - 1, length (i)))
        these_pars <- apply (index, 1, function (j) {
                         txt <- i [j [1]:j [2]]
                         ln <- grep ("\\s+- parameters:", txt)
                         p <- vapply (txt [- (1:ln)], function (k) {
                                          res <- gsub ("\\s+-\\s?", "", k)
                                          strsplit (res, ":") [[1]] [1] },
                                          character (1),
                                          USE.NAMES = FALSE)
                         return (p) })
        if (methods::is (these_pars, "matrix")) {
            these_pars <- apply (these_pars, 2, function (j)
                                 list (as.vector (j)))
            these_pars <- lapply (these_pars, unlist)
        }
        names (these_pars) <- these_fns

        pars <- c (pars, these_pars)
    }

    fns <- unique (names (pars))

    this_env <- as.environment (paste0 ("package:", package))
    # add any internal fns which also have examples
    this_env <- add_internal_fns_to_namespace (this_env, exs)
    # and remove any internal (`:::`) namespace adresses from fns
    index <- grep ("\\:\\:\\:", fns)
    fns [index] <- unlist (regmatches (fns [index],
                               gregexpr ("(?<=\\:\\:\\:).*", fns [index],
                                         perl = TRUE)))

    pars_f <- lapply (fns, function (f) {
                          pars_f <- pars [which (names (pars) == f)]
                          unique (unlist (pars_f))    })
    names (pars_f) <- fns
    
    fmls <- lapply (fns, function (f)
                    names (formals (f, envir = this_env)))
    fmls <- lapply (seq_along (fmls), function (i) {
                        index <- which (!fmls [[i]] %in% pars_f [[i]] &
                                        !fmls [[i]] == "...")
                        fmls [[i]] [index]  })
    names (fmls) <- fns

    fmls <- fmls [which (vapply (fmls, length, integer (1)) > 0)]

    return (fmls)
}

test_untested_params <- function (exs = NULL, ...) {
    UseMethod ("test_untested_params", exs)
}

test_untested_params.NULL <- function (exs = NULL, ...) {

    report_object (type = "dummy",
                   test_name = "par_is_demonstrated",
                   content = paste0 ("Examples do not demonstrate ",
                                     "usage of this parameter"),
                   operation = "Check that parameter usage is demonstrated")
}

test_untested_params.list <- function (exs = NULL, res_in = NULL, ...) {

    pars <- untested_params (exs)
    pars <- pars [which (vapply (pars, length, integer (1)) > 0)]

    res <- lapply (seq_along (pars), function (i) {
                       ro <- test_untested_params.NULL ()
                       ro <- ro [rep (1, length (pars [[i]])), ]
                       ro$type <- "warning"
                       ro$fn_name <- names (pars) [i]
                       ro$parameter <- pars [[i]]
                       ro$yaml_hash <- NA_character_
                       return (ro)  })

    res <- do.call (rbind, res)

    return (rbind (res_in, res))
}

# no method dispatch for these
undocumented_params_NULL <- function () { # nolint

    report_object (type = "dummy",
                   test_name = "par_is_documented",
                   content = "Examples do not document this parameter",
                   operation = "Check that parameter is documented")
}

param_docs_match_input_NULL <- function (this_class) { # nolint

    report_object (type = "dummy",
                   test_name = "par_matches_docs",
                   operation = paste0 ("Check that documentation matches ",
                                       "class of input parameter"))
}


test_param_documentation <- function (x = NULL, ...) {

    UseMethod ("test_param_documentation", x)
}

test_param_documentation.NULL <- function (x = NULL, ...) {

    rbind (undocumented_params_NULL (),
           param_docs_match_input_NULL ())
}

test_param_documentation.autotest_obj <- function (x) { # nolint

    if (x$test)
        ret <- test_param_docs_test (x)
    else
        ret <- test_param_docs_notest (x)

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
        rd_desc <- get_Rd_param (x$package_loc,
                                 x$fn,
                                 names (x$params) [x$i])

        if (is.na (rd_desc)) {

            this_ret <- undocumented_params_NULL ()
            this_ret$type <- "warning"
            this_ret$fn_name <- x$fn
            this_ret$parameter <- names (x$params) [x$i]
            ret <- rbind (ret, this_ret)

        } else if (x$param_types [p] == "tabular" | is.na (x$param_types [p])) {

            this_class <- class (x$params [[p]])
            class_in_desc <- vapply (this_class, function (i)
                                     grepl (i, rd_desc),
                                     logical (1))
            if (!any (class_in_desc)) {

                this_ret <- param_docs_match_input_NULL ()
                this_ret$content <- paste0 ("Parameter documentation does ",
                                            "not describe class of [",
                                            paste0 (this_class, collapse = ", "),
                                            "]")
                this_ret$type <- "warning"
                this_ret$fn_name <- x$fn
                this_ret$parameter <- names (x$params) [x$i]
                ret <- rbind (ret, this_ret)
            }
        }
    }

    return (ret)
}

#' @param e package namespace environment
#' @noRd
add_internal_fns_to_namespace <- function (e, exs) {

    pkg <- strsplit (attr (e, "name"), ":") [[1]] [2]

    fns <- lapply (exs, function (i) {
                        y <- yaml::yaml.load (i,
                                              handlers = yaml_handlers ())
                        unique (vapply (y$functions, names, character (1)))
                          })
    fns <- unique (unlist (fns))
    fns <- grep ("\\:\\:\\:", fns, value = TRUE)
    if (length (fns) > 0) {
            fns <- vapply (fns, function (i) {
                               regmatches (i,
                                           gregexpr ("(?<=\\:\\:\\:).*", i,
                                                     perl = TRUE)) [[1]]
                          }, character (1),
                          USE.NAMES = FALSE)
            for (f in fns) {
                tmp_fn <- utils::getFromNamespace (f, pkg)
                e [[f]] <- tmp_fn
            }
    }

    return (e)
}
