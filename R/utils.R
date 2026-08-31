get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Load a package (installed or local source) into the current session, and
#' return its name.
#'
#' @param package Name of locally installed package or path to local source
#' @return Name of the package
#' @noRd
preload_package <- function (package) {

    if (pkg_is_source (package)) {

        pkg_name <- get_package_name (package)
        if (!paste0 ("package:", pkg_name) %in% search ()) {
            requireNamespace ("devtools")
            devtools::load_all (package, export_all = FALSE)
        }

    } else if (!basename (package) == package) {

        # pkgs installed in tmp_loc via covr
        suppressMessages (
            library (basename (package),
                lib.loc = normalizePath (file.path (package, "..")),
                character.only = TRUE
            )
        )
        pkg_name <- basename (package)

    } else {

        fp <- tryCatch (
            find.package (package),
            error = function (e) NULL
        )
        if (is.null (fp)) {
            stop ("package [", package, "] does not appear to be installed.")
        }
        suppressMessages (
            library (package, character.only = TRUE)
        )
        pkg_name <- package
    }

    return (pkg_name)
}

rm_internal_namespace <- function (x) {

    regmatches (
        x,
        gregexpr ("(?<=\\:\\:\\:).*",
            x,
            perl = TRUE
        )
    ) [[1]]
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
        x <- paste0 (x, collapse = "|")
    }

    return (x)
}
