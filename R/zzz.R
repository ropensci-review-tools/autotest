# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    options (keep.source = TRUE) # otherwise getParseData does not work

    invisible ()
}
# nocov end


# logical test whether a report data.frame is null, or whether it does not
# contain expected type ("not_this"). A result might, for example, be expected
# to be an error, so testing `null_or_not("error")` will give `TRUE` if no error
# occurs.
null_or_not <- function (x, not_this) {
    res <- is.null (x)
    if (!res) {
        for (i in not_this) {
            res <- c (res, i %in% x$type)
        }
    }

    return (!any (res))
}

not_null_and_is <- function (x, is_this) {
    res <- FALSE
    if (!is.null (x)) {
        if (any (is_this %in% x$type)) {
            res <- TRUE
        }
    }
    return (res)
}

# Conversion of default `package = "."` to proper package path
dot_to_package <- function (package) {

    fp <- tryCatch (
        find.package (package),
        error = function (e) NULL
    )
    if (!is.null (fp)) {
        return (package)
    }

    # https://github.com/r-lib/rprojroot/blob/master/R/root.R#L115:
    .max_depth <- 10L

    files <- c ("DESCRIPTION", "NAMESPACE")

    if (package == "." | !all (files %in% list.files (package))) {

        package <- normalizePath (package)

        if (!all (files %in% list.files (package))) {

            for (i in seq_len (.max_depth)) {

                package <- normalizePath (file.path (package, ".."))

                if (all (files %in% list.files (package))) {
                    return (package)
                }
            }
        }
    }

    if (!all (files %in% list.files (package))) {
        stop ("Unable to find root directory of an R package")
    }

    return (package)
}

# same criteria as rprojroot::is_r_package, but without extra dependency.
pkg_is_source <- function (package) {

    need_these <- file.path (
        package,
        c ("DESCRIPTION", "NAMESPACE", "R", "man")
    )

    is_source <- FALSE

    if (file.exists (package)) {
        if (all (file.exists (need_these))) {
            desc <- readLines (file.path (package, "DESCRIPTION"))
            if (any (grepl ("^Package\\:", desc))) {
                is_source <- TRUE
            }
        }
    }

    return (is_source)
}

pkg_lib_path <- function (package, root = FALSE) {

    if (dir.exists (package)) {
        package <- utils::tail (strsplit (package, .Platform$file.sep) [[1]], 1)
    }

    if (!paste0 ("package:", package) %in% search ()) {
        stop ("Package [", package, "] is not loaded")
    }

    sp <- vapply (searchpaths (), function (i) {
        utils::tail (strsplit (i, .Platform$file.sep) [[1]], 1)
    },
    character (1),
    USE.NAMES = TRUE
    )

    path <- names (sp) [which (sp == package)]

    if (root) {
        path <- normalizePath (file.path (path, ".."))
    }

    return (path)
}

get_git_hash <- function (package) {

    ret <- NULL

    withr::with_dir (package, {
        if (dir.exists (file.path (package, ".git"))) {

            x <- system2 ("git", c ("log", "-1"), stdout = TRUE) [1]
            ret <- gsub ("commit\\s+", "", x)
        }
    })

    return (ret)
}
