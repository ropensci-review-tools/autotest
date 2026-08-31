get_typetrace_dir <- function () {

    td <- getOption ("typetracedir")
    if (is.null (td)) {
        td <- tempdir ()
    }
    return (td)
}

autotest_trace_package <- function (package,
                                    functions = NULL,
                                    exclude = NULL) {


    package <- dot_to_package (package)
    pkg_name <- preload_package (package)

    functions <- include_functions (package, functions, exclude)

    Sys.setenv ("TYPETRACER_LEAVE_TRACES" = "true")
    if (pkg_name != package) {
        checkmate::assert_directory_exists (package, .var.name = "package")
        args <- list (pkg_dir = package)
    } else {
        args <- list (package = package)
    }
    if (!is.null (functions)) {
        args$functions <- functions
    }
    args$types <- c ("examples", "tests")

    # 'typetracer::trace_package()' runs every documented example in-process
    # via 'eval(parse(text = ex))' to derive traces, and only attempts its own
    # plot suppression via 'options(device = NULL)', which has no effect when
    # a device is already active (e.g. knitr's per-chunk recording device) --
    # so wrap the whole call to redirect any such plotting to a discarding
    # device regardless of what's already active.
    traces <- with_null_device (do.call (typetracer::trace_package, args))

    Sys.unsetenv ("TYPETRACER_LEAVE_TRACES") # traces are still there

    return (traces)
}

# combine lists of `functions` to include and `exclude` into single vector
include_functions <- function (package, functions = NULL, exclude = NULL) {

    fns <- m_get_pkg_functions (package)

    err_chk <- function (fn_arg, fns, package) {
        checkmate::assert_subset (
            fn_arg, fns,
            .var.name = paste0 ("functions in namespace of package: ", package)
        )
    }

    if (!is.null (functions)) {

        err_chk (functions, fns, package)
        fns <- fns [which (fns %in% functions)]

    } else if (!is.null (exclude)) {

        err_chk (exclude, fns, package)
        fns <- fns [which (!fns %in% exclude)]
    }

    return (fns)
}

#' Get all (unique) parameter names from all traced functions.
#'
#' @param traces Result of 'typetracer::trace_package()' call.
#' @return Reduced version of 'traces' with only unique parameter names.
#' @noRd
get_unique_fn_pars <- function (traces) {

    fn_pars <- unique (traces [, c ("fn_name", "par_name")])

    par_types <- lapply (seq_len (nrow (fn_pars)), function (i) {
        index <- which (traces$fn_name == fn_pars$fn_name [i] &
            traces$par_name == fn_pars$par_name [i])
        onecol <- function (traces, index, what = "classes") {
            res <- traces [[what]] [index]
            if (is.list (res)) {
                res <- do.call (c, res)
            }
            res <- unique (res)
            toString (res [which (res != "NULL")])
        }
        data.frame (
            class = onecol (traces, index, "class"),
            typeof = onecol (traces, index, "typeof"),
            mode = onecol (traces, index, "mode"),
            storage_mode = onecol (traces, index, "storage_mode"),
            length = onecol (traces, index, "length")
        )
    })

    return (cbind (fn_pars, do.call (rbind, par_types)))
}

#' Get all (unique) function/parameter name combinations demonstrated in
#' example-sourced traces only.
#'
#' Used to identify function parameters never exercised by any documented
#' example (`test_untested_params()`), as distinct from parameters only ever
#' seen in the package's own test suite. 'typetracer' records an entry for
#' every formal of a traced function, whether or not that formal was
#' actually written in the call (unspecified formals are recorded at their
#' resolved default value), so entries are only counted as "demonstrated"
#' here when their `par_uneval` is not the literal string `"NULL"` — the
#' value 'typetracer' records when a parameter was not part of the call
#' itself (see `trace_one_param()` in 'typetracer').
#'
#' @param trace_files Character vector of paths to individual 'typetracer'
#' trace '.Rds' files.
#' @return A `data.frame` with columns `fn_name` and `par_name`, one row per
#' unique combination explicitly demonstrated in an example-sourced trace.
#' @noRd
get_example_fn_pars <- function (trace_files) {

    pars <- lapply (trace_files, function (f) {

        trace_data <- readRDS (f)
        if (!identical (trace_data$trace_source, "examples")) {
            return (NULL)
        }

        par_index <- which (!nzchar (names (trace_data)))
        if (length (par_index) == 0L) {
            return (NULL)
        }

        was_demonstrated <- vapply (
            trace_data [par_index], function (j) !identical (j$par_uneval, "NULL"),
            logical (1L)
        )
        par_index <- par_index [was_demonstrated]
        if (length (par_index) == 0L) {
            return (NULL)
        }

        par_names <- vapply (
            trace_data [par_index], function (j) j$par,
            character (1L)
        )
        data.frame (fn_name = trace_data$fn_name, par_name = par_names)
    })

    pars <- do.call (rbind, pars)

    return (unique (pars))
}

#' Get the formal parameter names of every traced function.
#'
#' By the time `test_untested_params()` runs, `typetracer::trace_package()`
#' has already unloaded the traced package's namespace (and for a local
#' source package that was never actually installed, it may not be
#' reloadable at all afterwards), so formals can't reliably be looked up
#' again via `getFromNamespace()`. Each individual trace file already
#' records `par_formals`, captured directly at trace time, so those are
#' used instead.
#'
#' @param trace_files Character vector of paths to individual 'typetracer'
#' trace '.Rds' files.
#' @return Named `list`, one entry per unique traced function, each holding
#' that function's formal parameter names.
#' @noRd
get_fn_formals <- function (trace_files) {

    res <- lapply (trace_files, function (f) {
        trace_data <- readRDS (f)
        list (fn_name = trace_data$fn_name, formals = names (trace_data$par_formals))
    })

    fn_names <- vapply (res, function (x) x$fn_name, character (1L))
    fmls <- lapply (res, function (x) x$formals)
    index <- which (!duplicated (fn_names))
    fmls <- fmls [index]
    names (fmls) <- fn_names [index]

    return (fmls)
}
