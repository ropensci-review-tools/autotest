
#' autotest_package
#'
#' Automatically test an entire package by converting examples to `yaml` format
#' and submitting each to the \link{autotest_single_trace} function.
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
#' @param test If `FALSE`, return only descriptions of tests which would be run
#' with `test = TRUE`, without actually running them.
#' @param test_data Result returned from calling either \link{autotest_types} or
#' \link{autotest_package} with `test = FALSE` that contains a list of all tests
#' which would be conducted. These tests have an additional flag, `test`, which
#' defaults to `TRUE`. Setting any tests to `FALSE` will avoid running them when
#' `test = TRUE`.
#' @param quiet If 'FALSE', provide printed output on screen.
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
    pkg_dir <- get_package_loc (package)

    traces <- autotest_trace_package (package, functions = functions, exclude = exclude)

    trace_files <- list.files (
        get_typetrace_dir (),
        pattern = "^typetrace\\_.*\\.Rds$",
        full.names = TRUE
    )

    fn_pars <- get_unique_fn_pars (traces)

    res <- NULL

    for (i in seq_along (trace_files)) {

        res <- rbind (res,
                      autotest_single_trace (package,
                                             pkg_dir,
                                             readRDS (trace_files [i]),
                                             fn_pars,
                                             test = test,
                                             test_data = test_data,
                                             quiet = TRUE))

        if (!quiet) {
            message (cli::col_green (cli::symbol$tick, " [",
                                     i, " / ", length (trace_files), "]"))
        }
    }

    typetracer::clear_traces ()

    res <- res [which (!duplicated (res)), ]

    #res <- test_untested_params (exs, res)
    #res <- test_fns_wo_example (package, res, names (exs))

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

get_package_loc <- function (package) {

    pkg_dir <- tryCatch (find.package (package), error = function (e) NULL)
    
    if (is.null (pkg_dir)) {
        if (!dir.exists (package)) {
            stop ("Directory ['", package, "'] does not exist", call. = FALSE)
        }
    } else {
        package <- pkg_dir
    }

    return (package)
}


#' Test one 'typetracer' trace file
#'
#' @param traces A 'typetracer' trace file of function and parameter traces.
#' @param fn_pars Reduced version of 'typetracer' traces containing only unique
#' function and parameter name combinations.
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
#' @noRd
autotest_single_trace <- function (package,
                                   pkg_dir = NULL,
                                   trace_data = NULL,
                                   fn_pars,
                                   test = TRUE,
                                   test_data = NULL,
                                   quiet = FALSE) {

    # get parameter values:
    par_index <- which (!nzchar (names (trace_data)))
    par_names_i <- vapply (trace_data [par_index], function (j) j$par, character (1L))
    par_vals_i <- lapply (trace_data [par_index], function (j) j$par_eval)
    names (par_vals_i) <- par_names_i
    index <- which (!vapply (par_vals_i, is.null, logical (1L)))
    par_vals_i <- par_vals_i [index]
    par_names_i <- par_names_i [index]

    # get parameter classes & types:
    index <- which (fn_pars$fn_name == trace_data$fn_name &
                    fn_pars$par_name %in% par_names_i)
    fn_pars_i <- fn_pars [index, ]
    fn_pars_i <- fn_pars_i [match (fn_pars_i$par_name, par_names_i), ]

    # param_types are in [single, vector, tabular]
    param_types <- rep (NA_character_, nrow (fn_pars_i))
    is_single <- vapply (fn_pars_i$length, function (j)
        all (as.integer (strsplit (j, ",") [[1]]) <= 1L),
        logical (1L))
    param_types [which (is_single)] <- "single"
    is_rect <- vapply (trace_data [par_index], function (j)
        j$typeof == "list" && length (dim (j$par_eval)) == 2,
        logical (1L))
    param_types [which (is_rect)] <- "tabular"

    # reduce class to first value only
    param_class <- gsub (",\\s.*$", "", fn_pars_i$class)
    names (param_class) <- fn_pars_i$par_name
    index <- which (!param_class %in% c (atomic_modes (), "data.frame"))
    param_class <- param_class [index]

    test_obj <- autotest_obj (package = package,
                              package_loc = pkg_dir,
                              fn_name = trace_data$fn_name,
                              parameters = par_vals_i,
                              parameter_types = param_types,
                              class = param_class,
                              classes = param_class,
                              env = new.env (),
                              test = test,
                              quiet = quiet)
    int_val <- data.frame (
        fn = trace_data$fn_name,
        par = fn_pars_i$par_name,
        int_val = fn_pars_i$storage_mode == "integer"
    )
    test_obj <- add_int_attrs (test_obj, int_val)

    reports <- NULL

    if (length (test_obj$params) > 0L) {

        reports <- rbind (reports, autotest_rectangular (test_obj, test_data))

        reports <- rbind (reports, autotest_vector (test_obj, test_data))

        reports <- rbind (reports, autotest_single (test_obj, test_data))

        reports <- rbind (reports, autotest_return (test_obj, test_data))
    }

    reports <- rbind (reports, test_param_documentation (test_obj))

    if (!quiet) {
        message (cli::col_green (cli::symbol$tick, " ", this_fn))
    }

    if (!is.null (reports)) {

        reports <- reports [which (!duplicated (reports)), ]

        # rm "no_test" tests switched off from "test_data"
        if (test) {
            reports <- reports [which (!reports$type == "no_test"), ]
        }

        rownames (reports) <- NULL
    }

    return (reports)
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
