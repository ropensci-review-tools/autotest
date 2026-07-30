#' autotest_package
#'
#' Automatically test an entire package by tracing calls made in its
#' documented examples (and, for local source packages, its test suite) with
#' \pkg{typetracer}, then testing each traced function call in turn.
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
#' @param progress Style of progress display while testing functions, one of:
#' \itemize{
#'    \item `"bar"` (default) A `cli` progress bar. Automatically falls back
#'    to `"none"` when called from within a `knitr` document, to avoid
#'    literal ANSI escape sequences leaking into the rendered output.
#'    \item `"tests"` One line per function tested, showing `[i / n]`.
#'    \item `"none"` No progress display at all.
#' }
#' @return An `autotest_package` object which is derived from a \pkg{tibble}
#' `tbl_df` object. This has one row for each test, and the following eight
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
                              progress = c ("bar", "tests", "none")) {

    progress <- match.arg (progress)
    if (progress == "bar" && isTRUE (getOption ("knitr.in.progress"))) {
        # 'cli' progress bars rely on 'isatty()' at the file-descriptor
        # level, which can still read TRUE inside a knitr chunk even though
        # knitr has redirected R-level output, leaving literal ANSI
        # clear-line sequences (e.g. "[K") baked into the rendered document.
        progress <- "none"
    }

    package <- dot_to_package (package)
    pkg_name <- preload_package (package)
    pkg_dir <- get_package_loc (package)

    traces <- autotest_trace_package (package, functions = functions, exclude = exclude)

    # 'typetracer::trace_package()' unloads the traced package's namespace
    # once tracing is done, and does not itself reload it into this session
    # (its own end-of-trace reload check runs in a separate subprocess).
    # Packages with other loaded dependents (e.g. 'stats') tend to stay
    # attached regardless, since 'unloadNamespace()' silently no-ops when
    # dependents prevent it, but a standalone package with nothing else
    # depending on it unloads cleanly, leaving the mutation-testing loop
    # below unable to find its functions. Reload it to be sure.
    preload_package (package)

    trace_files <- list.files (
        get_typetrace_dir (),
        pattern = "^typetrace\\_.*\\.Rds$",
        full.names = TRUE
    )

    fn_pars <- get_unique_fn_pars (traces)

    res <- NULL

    # Traces from the package's own test suite are used only to enrich
    # 'fn_pars' with type information (above); mutation/fuzz testing itself
    # is only ever driven by example-derived traces, because test-suite
    # calls may be deliberately designed to trigger errors, which would
    # otherwise appear here as spurious autotest failures. Reading and
    # filtering all trace files once up front (rather than skipping
    # non-example traces mid-loop) also gives an accurate total for the
    # progress display below.
    trace_data_all <- lapply (trace_files, readRDS)
    is_example <- vapply (
        trace_data_all,
        function (d) identical (d$trace_source, "examples"),
        logical (1)
    )
    trace_data_all <- trace_data_all [is_example]
    n_total <- length (trace_data_all)

    if (progress == "bar") {
        cli::cli_progress_bar (name = "Testing functions", total = n_total)
    }

    for (i in seq_along (trace_data_all)) {

        trace_data <- trace_data_all [[i]]

        res <- rbind (
            res,
            autotest_single_trace (package,
                pkg_dir,
                trace_data,
                fn_pars,
                test = test,
                test_data = test_data,
                quiet = TRUE
            )
        )

        if (progress == "bar") {
            cli::cli_progress_update ()
        } else if (progress == "tests") {
            message (cli::col_green (
                cli::symbol$tick, " [",
                i, " / ", n_total, "]"
            ))
        }
    }

    if (progress == "bar") {
        cli::cli_progress_done ()
    }

    example_fn_pars <- get_example_fn_pars (trace_files)
    fn_formals <- get_fn_formals (trace_files)
    fn_names <- include_functions (package, functions, exclude)

    typetracer::clear_traces ()

    res <- res [which (!duplicated (res)), ]

    if (!is.null (example_fn_pars) && nrow (example_fn_pars) > 0) {
        res <- test_untested_params (
            example_fn_pars,
            fn_formals = fn_formals,
            res_in = res
        )
    }
    res <- test_fns_wo_example (package, res, fn_names)

    if (is.null (res)) {
        return (res)
    }

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

    param_info <- get_param_info (trace_data, fn_pars)

    test_obj <- autotest_obj (
        package = package,
        package_loc = pkg_dir,
        fn_name = trace_data$fn_name,
        parameters = param_info$value,
        parameter_types = param_info$type,
        class = param_info$class,
        classes = param_info$class,
        env = new.env (),
        test = test,
        quiet = quiet
    )

    int_val <- data.frame (
        fn = rep (trace_data$fn_name, nrow (param_info)),
        par = param_info$name,
        int_val = param_info$storage_mode == "integer"
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
        message (cli::col_green (cli::symbol$tick, " ", trace_data$fn_name))
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

    res <- rbind (
        autotest_rectangular (),
        autotest_vector (),
        autotest_single (),
        autotest_return (),
        test_untested_params (),
        test_fns_wo_example (),
        test_param_documentation ()
    )
    res <- tibble::tibble (res)

    class (res) <- c ("autotest_package", class (res))

    if (!is.null (notest)) {
        index <- match (notest, res$test_name)
        if (any (is.na (index))) {
            message (
                "notest = [",
                paste0 (notest [which (is.na (index))], collapse = ", "),
                "] does not match any test_name values"
            )
            index <- index [which (!is.na (index))]
        }
        res$test [index] <- FALSE
    }

    return (res)
}

order_at_rows <- function (x) {

    type_order <- c (
        "error", "warning", "diagnostic", "message",
        "dummy", "no_test"
    )
    index <- data.frame (
        index = seq (nrow (x)),
        type = match (x$type, type_order)
    )
    index <- index [order (index$type), ]
    x <- x [index$index, ]
    rownames (x) <- NULL

    return (x)
}
