
get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

rm_internal_namespace <- function (x) {

    regmatches (x,
                gregexpr ("(?<=\\:\\:\\:).*",
                          x,
                          perl = TRUE)) [[1]]
}

#' List of atomic modes
#'
#' from ?is.atomic
#' @noRd
atomic_modes <- function (collapse = FALSE) {
    
    x <- c ("logical",
            "integer",
            "numeric",
            "complex",
            "character",
            "raw",
            "NULL")

    if (collapse)
        x <- paste0 (x, collapse = "|")

    return (x)
}
