# nocov start
.onLoad <- function (libname, pkgname)
{
    op <- options ()
	
    op.autotest <- list (autotest_yaml_indent = 4)
	
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
        if (is_this %in% x$type)
            res <- TRUE
    return (res)
}

report_object <- function (type = "diagnostic",
                           fn_name = NA_character_,
                           parameter = NA_character_,
                           operation = NA_character_,
                           content = NA_character_) {
    data.frame (type = type,
                fn_name = fn_name,
                parameter = parameter,
                operation = operation,
                content = content,
                stringsAsFactors = FALSE)
}

# same criteria as rprojroot::is_r_package, but without extra dependency.
pkg_is_source <- function (package) {
    is_source <- FALSE
    if (file.exists (package)) {
        if (file.exists (file.path (package, "DESCRIPTION"))) {
            desc <- readLines (file.path (package, "DESCRIPTION"))
            if (any (grepl ("^Package\\:", desc)))
                is_source <- TRUE
        }
    }
    return (is_source)
}

get_git_hash <- function (package) {
    wd <- setwd (package)
    x <- system2 ("git", c ("log", "-1"), stdout = TRUE) [1]
    setwd (wd)
    return (gsub ("commit\\s+", "", x))
}

#' @param pkg Either name of locally-installed package or path to package source
#' @param suggests If `FALSE`, return just names of package Imports, if `TRUE`
#' then also include Suggests
#' @return List of package Imports (and optionally Suggests)
#' @noRd
get_pkg_deps <- function (pkg, suggests = FALSE) {
    if (pkg_is_source (pkg)) {
        desc <- readLines (file.path (pkg, "DESCRIPTION"))
        get_imports <- function (desc, s = "^Imports:") {
            i_start <- grep (s, desc)
            i_end <- grep ("[[:alpha:]]$|\\)$", desc)
            i_end <- i_end [i_end >= i_start] [1]
            imports <- gsub (s, "", paste0 (desc [i_start:i_end], collapse = " "))
            imports <- strsplit (imports, ", ") [[1]]
            imports <- gsub ("\\(.*\\)$", "", imports)
            return (gsub ("^\\s*|\\s*$", "", imports))
        }
        imports <- get_imports (desc)
        if (suggests)
            imports <- c (imports, get_imports (desc, "^Suggests:"))
    } else {
        ip <- data.frame (utils::installed.packages ())
        imports <- strsplit (ip$Imports [ip$Package == pkg], ", ") [[1]]
        imports <- gsub ("\\s*\\(.*$", "", imports)
        if (suggests) {
            s <- strsplit (ip$Suggests [ip$Package == pkg], ", ") [[1]]
            imports <- c (imports, gsub ("\\s*\\(.*$", "", s))
        }
    }

    return (imports)
}
