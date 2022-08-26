# Trace a package with 'typetracer'

get_typetrace_dir <- function () {

    td <- getOption ("typetracedir")
    if (is.null (td)) {
        td <- tempdir ()
    }
    return (td)
}

autotest_trace_package <- function (package) {

    Sys.setenv ("TYPETRACER_LEAVE_TRACES" = "true")

    package <- dot_to_package (package)
    pkg_name <- preload_package (package)
    if (pkg_name != package) {
        if (!dir.exists (package)) {
            stop ("'package' should be a local directory.")
        }
        traces <- typetracer::trace_package (pkg_dir = package)
    } else {
        traces <- typetracer::trace_package (package = package)
    }

    trace_files <- list.files (
        get_typetrace_dir (),
        pattern = "^typetrace\\_.*\\.Rds$",
        full.names = TRUE
    )

    Sys.unsetenv ("TYPETRACER_LEAVE_TRACES")

    return (trace_files)
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
