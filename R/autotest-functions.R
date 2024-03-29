#' autotest_yaml
#'
#' Automatically test inputs to functions specified in a 'yaml' template.
#'
#' @param yaml A 'yaml' template as a character vector, either hand-coded or
#' potentially loaded via \link{readLines} function or similar. Should generally
#' be left at default of 'NULL', with template specified by 'filename'
#' parameter.
#' @param filename Name (potentially including path) of file containing 'yaml'
#' template. See \link{at_yaml_template} for details of template. Default uses
#' template generated by that function, and held in local './tests' directory.
#' @param test If `FALSE`, return only descriptions of tests which would be run
#' with `test = TRUE`, without actually running them.
#' @param test_data Result returned from calling either \link{autotest_types} or
#' \link{autotest_package} with `test = FALSE` that contains a list of all tests
#' which would be conducted. These tests have an additional flag, `test`, which
#' defaults to `TRUE`. Setting any tests to `FALSE` will avoid running them when
#' `test = TRUE`.
#' @param quiet If 'FALSE', provide printed output on screen.
#' @return An `autotest_pkg` object, derived from a \pkg{tibble}, detailing
#' instances of unexpected behaviour for every parameter of every function.
#' @family yaml
#'
#' @examples
#' \dontrun{
#' yaml_list <- examples_to_yaml (package = "stats", functions = "reshape")
#' res <- autotest_yaml (yaml = yaml_list)
#' }
#' @export
autotest_yaml <- function (yaml = NULL,
                           filename = NULL,
                           test = TRUE,
                           test_data = NULL,
                           quiet = FALSE) {

    if (is.null (yaml) & is.null (filename)) {
        stop ("either yaml or filename must be given")
    } else if (!is.null (filename)) {
        yaml <- readLines (filename)
        pkg <- strsplit (yaml [grep ("^package:", yaml)],
                         "^package: ") [[1]] [2]
        attr (yaml, "package") <- pkg
    }

    if (is.character (yaml) & !is.null (attr (yaml, "package"))) {
        yaml <- list (yaml)
    }

    msg <- paste0 ("yaml must be either a single character vector ",
                   "representing a yaml 'autotest' object, or a list ",
                   "of such objects")
    if (!is.list (yaml))
        stop (msg)

    # Ensure anything passed as list represents valid yaml input:
    if (!is.character (yaml [[1]]) & !is.null (attr (yaml [[1]], "package")))
        stop (msg)

    reports <- lapply (yaml, function (i)
                      autotest_single_yaml (i,
                                            filename,
                                            test,
                                            test_data,
                                            quiet))
    reports <- do.call (rbind, reports)

    if (!is.null (reports)) {
        reports <- tibble::tibble (reports)
        class (reports) <- c ("autotest_package", class (reports))
    }

    return (reports)
}

#' Test one 'yaml' input file
#'
#' @inheritParams autotest_yaml
#' @noRd
autotest_single_yaml <- function (yaml = NULL,
                                  filename = NULL,
                                  test = TRUE,
                                  test_data = NULL,
                                  quiet = FALSE) {

    # yaml templates can be preprocessing only, with no direct function calls:
    if (!any (grepl ("- parameters:$", yaml)))
        return (NULL)

    res <- parse_yaml_template (yaml = yaml, filename = filename)

    # are parameters exclusively used as single-valued, or vectors?
    par_lengths <- single_or_vec (res)
    # are numeric parameters exclusively used as integers?
    int_val <- double_or_int (res)

    reports <- NULL

    for (i in seq_along (res$parameters)) {

        this_fn <- names (res$parameters) [i]
        params <- get_params (res, i, this_fn)
        params <- params [which (!(params == "NULL" | names (params) == "..."))]
        param_types <- get_param_types (this_fn, params,
                                        par_lengths)

        param_class <- vapply (params,
                               function (i)
                                   ifelse (inherits (i, "data.frame"),
                                           "data.frame",
                                           class (i) [1]),
                               character (1))
        index <- which (!param_class %in% c (atomic_modes (),
                                             "data.frame"))
        param_class <- param_class [index]
        if (length (param_class) == 0L)
            param_class <- NULL

        test_obj <- autotest_obj (package = res$package,
                                  package_loc = attr (yaml, "package"),
                                  fn_name = names (res$parameters) [i],
                                  parameters = params,
                                  parameter_types = param_types,
                                  class = param_class,
                                  classes = res$classes [[i]],
                                  env = new.env (),
                                  test = test,
                                  quiet = quiet)

        test_obj <- add_int_attrs (test_obj, int_val)

        if (grepl ("\\:\\:\\:", test_obj$fn)) {
            test_obj$fn <- rm_internal_namespace (test_obj$fn)
        }

        if (length (params) > 0L) {

            reports <- rbind (reports, autotest_rectangular (test_obj, test_data))

            reports <- rbind (reports, autotest_vector (test_obj, test_data))

            reports <- rbind (reports, autotest_single (test_obj, test_data))

            reports <- rbind (reports, autotest_return (test_obj, test_data))
        }

        reports <- rbind (reports, test_param_documentation (test_obj))

        if (!quiet)
            message (cli::col_green (cli::symbol$tick, " ", this_fn))
    }

    if (!is.null (reports)) {

        # add hash to reports
        if (is.null (yaml) & !is.null (filename))
            yaml <- readLines (filename)
        reports$yaml_hash <- digest::digest (yaml)

        reports <- reports [which (!duplicated (reports)), ]

        # rm "no_test" tests switched off from "test_data"
        if (test) {
            no_test <- reports$type == "no_test"
            if (all (no_test)) {
                reports <- NULL
            } else {
                reports <- reports [which(!no_test), ]
                rownames (reports) <- NULL
            }
        }

    }

    return (reports)
}

#' autotest_package
#'
#' Automatically test an entire package by converting examples to `yaml` format
#' and submitting each to the \link{autotest_yaml} function.
#'
#' @param package Name of package, as either
#' \enumerate{
#'    \item Path to local package source
#'    \item Name of installed package
#'    \item Full path to location of installed package if not on \link{.libPaths}, or
#'    \item Default which presumes current directory is within package to be
#'    tested.
#' }
#' @param functions Optional character vector containing names of functions of
#' nominated package to be included in 'autotesting'.
#' @param exclude Optional character vector containing names of any functions of
#' nominated package to be excluded from 'autotesting'.
#' @inheritParams autotest_yaml
#' @return An `autotest_package` object which is derived from a \pkg{tibble}
#' `tbl_df` object. This has one row for each test, and the following nine
#' columns:
#' \enumerate{
#'    \item `type` The type of result, either "dummy" for `test = FALSE`, or one
#'    of "error", "warning", "diagnostic", or "message".
#'    \item `test_name` Name of each test
#'    \item `fn_name` Name of function being tested
#'    \item `parameter` Name of parameter being tested
#'    \item `parameter_type` Expected type of parameter as identified by
#'    `autotest`.
#'    \item `operation` Description of the test
#'    \item `content` For `test = FALSE`, the expected behaviour of the test; for
#'    `test = TRUE`, the observed discrepancy with that expected behaviour
#'    \item `test` If `FALSE` (default), list all tests without implementing them,
#'    otherwise implement all tests.
#'    \item `yaml_hash' A unique hash which may be be used to extract the `yaml`
#'    specification of each test.
#' }
#' Some columns may contain NA values, as explained in the Note.
#'
#' @note Some columns may contain NA values, including:
#' \itemize{
#'    \item `parameer` and `parameter_type`, for tests applied to entire
#'    functions, such as tests of return values.
#'    \item `test_name` for warnings or errors generated through "normal"
#'    function calls generated directly from example code, in which case `type`
#'    will be "warning" or "error", and `content` will contain the content of
#'    the corresponding message.
#' }
#' @family main_functions
#' @export
autotest_package <- function (package = ".",
                              functions = NULL,
                              exclude = NULL,
                              test = FALSE,
                              test_data = NULL,
                              quiet = FALSE) {

    package <- dot_to_package (package)
    pkg_name <- preload_package (package)

    exclude <- exclude_functions (package, functions, exclude)

    exs <- examples_to_yaml (package, exclude = exclude, quiet = quiet)

    if (!quiet) {
        txt <- paste0 ("autotesting ", get_package_name (package))
        cli::cli_h2 (cli::col_green (txt))
    }

    res <- NULL
    for (i in seq_along (exs)) {
        yaml <- exs [[i]]
        attr (yaml, "package") <- package
        fn_name <- fn_from_yaml (yaml)

        res <- rbind (res,
                      autotest_yaml (yaml = yaml,
                                     test = test,
                                     test_data = test_data,
                                     quiet = TRUE))

        if (!quiet)
            message (cli::col_green (cli::symbol$tick, " [",
                                     i, " / ", length (exs),
                                     "]: ", fn_name [1]))
    }
    res <- res [which (!duplicated (res)), ]

    res <- test_untested_params (exs, res)
    res <- test_fns_wo_example (package, res, names (exs))

    if (is.null (res))
        return (res)

    attr (res, "package") <- package

    if (pkg_is_source (package)) {

        desc <- file.path (package, "DESCRIPTION")
        attr (res, "packageName") <- read.dcf (desc, "Package")
        attr (res, "packageVersion") <- read.dcf (desc, "Version")
        attr (res, "githash") <- get_git_hash (package)

    } else {

        attr (res, "packageName") <- package
        attr (res, "packageVersion") <-
            utils::packageVersion (basename (package))

    }

    return (order_at_rows (res))
}

# Extract function name from yaml; used only to screen dump in autootest_package
fn_from_yaml <- function (yaml) {

    x <- yaml::yaml.load (yaml)
    nms <- vapply (x$functions, names, character (1))
    return (unique (nms))

}

#' autotest_types
#'
#' List all types of 'autotests' currently implemented.
#'
#' @param notest Character string of names of tests which should be switched off
#' by setting the `test` column to `FALSE`. Run this function first without this
#' parameter to get all names, then re-run with this parameter switch specified
#' tests off.
#'
#' @return An `autotest` object with each row listing one unique type of test
#' which can be applied to every parameter (of the appropriate class) of each
#' function.
#' @family main_functions
#'
#' @export
autotest_types <- function (notest = NULL) {

    res <- rbind (autotest_rectangular (),
                  autotest_vector (),
                  autotest_single (),
                  autotest_return (),
                  test_untested_params (),
                  test_fns_wo_example (),
                  test_param_documentation ())
    res <- tibble::tibble (res)

    class (res) <- c ("autotest_package", class (res))

    if (!is.null (notest)) {
        index <- match (notest, res$test_name)
        if (any (is.na (index))) {
            message ("notest = [",
                     paste0 (notest [which (is.na (index))], collapse = ", "),
                     "] does not match any test_name values")
            index <- index [which (!is.na (index))]
        }
        res$test [index] <- FALSE
    }

    return (res)
}

order_at_rows <- function (x) {

    type_order <- c ("error", "warning", "diagnostic", "message",
                     "dummy", "no_test")
    index <- data.frame (index = seq (nrow (x)),
                         type = match (x$type, type_order))
    index <- index [order (index$type), ]
    x <- x [index$index, ]
    rownames (x) <- NULL

    return (x)
}
