# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    options (keep.source = TRUE) # otherwise getParseData does not work

    op <- options ()

    op.autotest <- list (autotest_yaml_indent = 4) # nolint

    toset <- !(names (op.autotest) %in% names (op))
    if (any (toset))
        options (op.autotest [toset])
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
    if (!is.null (x))
        if (any (is_this %in% x$type))
            res <- TRUE
    return (res)
}

# Conversion of default `package = "."` to proper package path
dot_to_package <- function (package) {

    ip <- as.data.frame (utils::installed.packages ())
    if (package %in% ip$Package)
        return (package)

    # https://github.com/r-lib/rprojroot/blob/master/R/root.R#L115:
    .max_depth <- 10L

    files <- c ("DESCRIPTION", "NAMESPACE")

    if (package == "." | !all (files %in% list.files (package))) {

        package <- normalizePath (package)

        if (!all (files %in% list.files (package))) {

            for (i in seq_len (.max_depth)) {

                package <- normalizePath (file.path (package, ".."))

                if (all (files %in% list.files (package)))
                    return (package)
            }
        }
    }

    if (!all (files %in% list.files (package)))
        stop ("Unable to find root directory of an R package")

    return (package)
}

# same criteria as rprojroot::is_r_package, but without extra dependency.
pkg_is_source <- function (package) {

    need_these <- file.path (package,
                             c ("DESCRIPTION", "NAMESPACE", "R", "man"))

    is_source <- FALSE

    if (file.exists (package)) {
        if (all (file.exists (need_these))) {
            desc <- readLines (file.path (package, "DESCRIPTION"))
            if (any (grepl ("^Package\\:", desc)))
                is_source <- TRUE
        }
    }

    return (is_source)
}

pkg_lib_path <- function (package, root = FALSE) {

    if (dir.exists (package))
        package <- utils::tail (strsplit (package, .Platform$file.sep) [[1]], 1)

    if (!paste0 ("package:", package) %in% search ())
        stop ("Package [", package, "] is not loaded")

    sp <- vapply (searchpaths (), function (i)
                  utils::tail (strsplit (i, .Platform$file.sep) [[1]], 1),
                  character (1),
                  USE.NAMES = TRUE)

    path <- names (sp) [which (sp == package)]

    if (root)
        path <- normalizePath (file.path (path, ".."))

    return (path)
}

get_git_hash <- function (package) {

    ret <- NULL

    wd <- setwd (package)

    if (dir.exists (file.path (package, ".git"))) {

        x <- system2 ("git", c ("log", "-1"), stdout = TRUE) [1]
        ret <- gsub ("commit\\s+", "", x)
    }

    setwd (wd)

    return (ret)
}

#' @param pkg Either name of locally-installed package or path to package source
#' @param suggests If `FALSE`, return just names of package Imports, if `TRUE`
#' then also include Suggests
#' @return List of package Imports (and optionally Suggests)
#' @noRd
get_pkg_deps <- function (pkg, suggests = FALSE) {

    if (pkg_is_source (pkg)) {

        desc <- file.path (pkg, "DESCRIPTION")
        get_deps <- function (desc, s = "Imports") {
            deps <- strsplit (read.dcf (desc, s), ",|,\\n") [[1]]
            ret <- NULL
            if (!all (is.na (deps))) {
                ret <- gsub ("\\(.*\\)$", "", deps)
                ret <- gsub ("^\\s*|\\s*$", "", ret)
            }
            return (ret)
        }
        deps <- get_deps (desc)
        if (suggests)
            deps <- c (deps, get_deps (desc, "Suggests"))
    } else {

        ip <- data.frame (utils::installed.packages (),
                          stringsAsFactors = FALSE)
        if (!pkg %in% ip$Package) {

            lib <- c (.libPaths (), pkg_lib_path (pkg, root = TRUE))

            ip <- data.frame (utils::installed.packages (lib.loc = lib),
                              stringsAsFactors = FALSE)
        }

        deps <- strsplit (ip$Depends [ip$Package == pkg], ", ") [[1]]
        deps <- gsub ("\\s*\\(.*$", "", deps [!is.na (deps)])
        imports <- strsplit (ip$Imports [ip$Package == pkg], ", ") [[1]]
        deps <- c (deps, gsub ("\\s*\\(.*$", "", imports [!is.na (imports)]))
        if (suggests) {
            s <- strsplit (ip$Suggests [ip$Package == pkg], ", ") [[1]]
            deps <- c (deps, gsub ("\\s*\\(.*$", "", s [!is.na (s)]))
        }
    }
    base_pkgs <- c ("stats", "graphics", "grDevices", "utils",
                    "datasets", "methods", "base")
    deps <- deps [which (!deps %in% c ("R", base_pkgs))]

    return (deps)
}
