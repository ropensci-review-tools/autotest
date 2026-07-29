# Trace a package with 'typetracer'

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
        if (!dir.exists (package)) {
            stop ("'package' should be a local directory.")
        }
        args <- list (pkg_dir = package)
    } else {
        args <- list (package = package)
    }
    if (!is.null (functions)) {
        args$functions <- functions
    }
    args$types <- c ("examples", "tests")

    traces <- do.call (typetracer::trace_package, args)

    Sys.unsetenv ("TYPETRACER_LEAVE_TRACES") # traces are still there

    return (traces)
}

# combine lists of `functions` to include and `exclude` into single vector
include_functions <- function (package, functions = NULL, exclude = NULL) {

    fns <- m_get_pkg_functions (package)

    err_chk <- function (fn_arg, fns, package) {
        if (!all (fn_arg %in% fns)) {
            fn_arg <- fn_arg [which (!fn_arg %in% fns)]
            stop ("The following functions are not in the namespace of ",
                "package:", package, ": [",
                paste0 (fn_arg, collapse = ", "), "]",
                call. = FALSE
            )
        }
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

    par_types <- lapply (seq (nrow (fn_pars)), function (i) {
        index <- which (traces$fn_name == fn_pars$fn_name [i] &
            traces$par_name == fn_pars$par_name [i])
        onecol <- function (traces, index, what = "classes") {
            res <- traces [[what]] [index]
            if (is.list (res)) {
                res <- do.call (c, res)
            }
            res <- unique (res)
            paste0 (res [which (!res == "NULL")], collapse = ", ")
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
#' seen in the package's own test suite.
#' @param trace_files Character vector of paths to individual 'typetracer'
#' trace '.Rds' files.
#' @return A `data.frame` with columns `fn_name` and `par_name`, one row per
#' unique combination demonstrated in an example-sourced trace.
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

        par_names <- vapply (
            trace_data [par_index], function (j) j$par,
            character (1L)
        )
        data.frame (fn_name = trace_data$fn_name, par_name = par_names)
    })

    pars <- do.call (rbind, pars)

    return (unique (pars))
}
