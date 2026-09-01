get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Load a package (installed or local source) into the current session, and
#' return its name.
#'
#' @inheritParams get_pkg_functions
#' @return Name of the package
#' @noRd
preload_package <- function (package) {

    if (pkg_is_source (package)) {

        pkg_name <- get_package_name (package)
        if (!paste0 ("package:", pkg_name) %in% search ()) {
            requireNamespace ("devtools")
            devtools::load_all (package, export_all = FALSE)
        }

    } else if (fs::path_file (package) != package) {

        # pkgs installed in tmp_loc via covr
        # 'library()' (not 'requireNamespace()') is required here: the
        # package must be attached to the search path so that unqualified
        # calls in traced example/test code resolve correctly.
        suppressMessages (
            library ( # nolint
                fs::path_file (package),
                lib.loc = fs::path_abs (fs::path (package, "..")),
                character.only = TRUE,
                warn.conflicts = FALSE,
                verbose = FALSE
            )
        )
        pkg_name <- fs::path_file (package)

    } else {

        fp <- tryCatch (
            find.package (package),
            error = function (e) NULL
        )
        if (is.null (fp)) {
            stop ("package [", package, "] does not appear to be installed.")
        }
        # 'library()' required here too, for the same reason as above.
        suppressMessages (
            library (package, character.only = TRUE) # nolint
        )
        pkg_name <- package
    }

    return (pkg_name)
}

#' List of atomic modes
#'
#' from ?is.atomic
#' @noRd
atomic_modes <- function (collapse = FALSE) {

    x <- c (
        "logical",
        "integer",
        "numeric",
        "complex",
        "character",
        "raw",
        "NULL"
    )

    if (collapse) {
        x <- paste (x, collapse = "|")
    }

    return (x)
}
