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

    if (package == "." | !all (files %in% fs::path_file (fs::dir_ls (package, fail = FALSE)))) {

        package <- fs::path_abs (package)

        if (!all (files %in% fs::path_file (fs::dir_ls (package, fail = FALSE)))) {

            for (i in seq_len (.max_depth)) {

                package <- fs::path_abs (fs::path (package, ".."))

                if (all (files %in% fs::path_file (fs::dir_ls (package, fail = FALSE)))) {
                    return (package)
                }
            }
        }
    }

    if (!all (files %in% fs::path_file (fs::dir_ls (package, fail = FALSE)))) {
        stop ("Unable to find root directory of an R package")
    }

    return (package)
}

# same criteria as rprojroot::is_r_package, but without extra dependency.
pkg_is_source <- function (package) {

    need_these <- fs::path (
        package,
        c ("DESCRIPTION", "NAMESPACE", "R", "man")
    )

    is_source <- FALSE

    if (fs::file_exists (package)) {
        if (all (fs::file_exists (need_these))) {
            desc <- readLines (fs::path (package, "DESCRIPTION"))
            if (any (grepl ("^Package\\:", desc))) {
                is_source <- TRUE
            }
        }
    }

    return (is_source)
}

pkg_lib_path <- function (package, root = FALSE) {

    if (fs::dir_exists (package)) {
        package <- fs::path_file (package)
    }

    if (!paste0 ("package:", package) %in% search ()) {
        stop ("Package [", package, "] is not loaded")
    }

    sp <- vapply (searchpaths (), function (i) {
        fs::path_file (i)
    },
    character (1),
    USE.NAMES = TRUE
    )

    path <- names (sp) [which (sp == package)]

    if (root) {
        path <- fs::path_abs (fs::path (path, ".."))
    }

    return (path)
}

get_git_hash <- function (package) {

    ret <- NULL

    withr::with_dir (package, {
        if (fs::dir_exists (fs::path (package, ".git"))) {

            x <- system2 ("git", c ("log", "-1"), stdout = TRUE) [1]
            ret <- gsub ("commit\\s+", "", x)
        }
    })

    return (ret)
}
