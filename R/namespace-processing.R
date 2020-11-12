#' @param package Name of locally installed package or path to local source
#' @return List of all functions exported by nominated package excluding any
#' re-exports from other packages
#' @noRd
get_pkg_functions <- function (package) {

    pkg <- get_package_name (package)
    e <- as.environment (paste0 ("package:", get_package_name (pkg)))

    if (pkg_is_source (package)) {
        man_dir <- list.files (file.path (package, "man"), pattern = "\\.Rd$",
                               full.names = TRUE)
        fns <- lapply (man_dir, function (i)
                       get_Rd_metadata (tools::parse_Rd (i), "alias"))
        fns <- unique (unlist (fns))
    } else {
        fns <- ls (paste0 ("package:", package))
    }

    fn_classes <- vapply (fns, function (i)
                          tryCatch (class (get (i, envir = e)) [1],
                                    error = function (err) NA_character_),
                          character (1))
    fns <- fns [grep ("[Ff]unction|standardGeneric", fn_classes)]

    other_fns <- fns_from_other_pkgs (package)

    return (fns [which (!fns %in% other_fns)])
}
m_get_pkg_functions <- memoise::memoise (get_pkg_functions)

#' @param package Name of locally installed package or path to local source
#' @note This function is really slow, but is only called one so no gain from
#' memoising.
#' @noRd
fns_without_examples <- function (package) {
    if (pkg_is_source (package)) {
        man_dir <- list.files (file.path (package, "man"), pattern = "\\.Rd$",
                               full.names = TRUE)
        ex_alias <- lapply (man_dir, function (i) {
                                rd <- tools::parse_Rd (i)
                                list (ex = get_Rd_metadata (rd, "examples"),
                                      aliases = get_Rd_metadata (rd, "alias"))
                               })
    } else {
        rd <- tools::Rd_db (package = package)
        ex_alias <- lapply (rd, function (i)
                            list (ex = get_Rd_metadata (i, "examples"),
                                  aliases = get_Rd_metadata (i, "alias")))
    }

    index <- which (vapply (ex_alias, function (i) {
                                res <- nchar (i$ex)
                                if (length (res) == 0) res <- 0L
                                return (res)    },
                                integer (1)) == 0)
    fns <- lapply (ex_alias [index], function (i) i$alias)
    fns <- unique (unlist (fns))

    fns <- fns [which (fns %in% m_get_pkg_functions (package))]

    return (fns)
}

fns_from_other_pkgs <- function (package) {
    if (pkg_is_source (package)) {
        fp <- file.path (package)
    } else {
        fp <- file.path (R.home (), "library", package)
    }
    namespace_file <- file.path (fp, "NAMESPACE")
    if (!file.exists (namespace_file))
        stop ("There is no NAMESPACE file at [", fp, "]")

    ns <- readLines (namespace_file)
    ns <- gsub ("importFrom\\(|\\)$", "", ns [grep ("^importFrom", ns)])
    fns <- vapply (ns, function (i) strsplit (i, split = ",") [[1]] [2],
                   character (1))

    index <- grep ("\"", fns)
    if (length (index) > 0) {
        fns_sub <- gsub ("\"", "", fns [index])
        fns <- c (fns, fns_sub)
    }

    return (fns)
}

#' @param x A list of function aliases
#' @return `data.frame` of function aliases, official topics as defined in the
#' corresponding ".Rd" file, and associated names of the Rd files or database
#' entries.
#' @note If `is.null(x)`, the full list of topics and aliases are returned,
#' otherwise it is reduced to only topics assocaited with the aliases given in
#' `x`.
#' @noRd
fns_to_topics <- function (x = NULL, package) {
    if (pkg_is_source (package)) {
        man_dir <- list.files (file.path (package, "man"), pattern = "\\.Rd$",
                               full.names = TRUE)
        alias_topic <- lapply (man_dir, function (i) {
                                rd <- tools::parse_Rd (i)
                                alias <- get_Rd_metadata (rd, "alias")
                                topic <- get_Rd_metadata (rd, "name")
                                name <- strsplit (i, .Platform$file.sep) [[1]]
                                name <- name [length (name)]
                                cbind (alias,
                                       rep (topic, length (alias)),
                                       rep (name, length (alias)))
                               })
    } else {
        rd <- tools::Rd_db (package = package)
        # installed packages can have different R
        alias_topic <- lapply (rd, function (i) {
                                alias <- get_Rd_metadata (i, "alias")
                                topic <- get_Rd_metadata (i, "name")
                                name <- strsplit (attr (i, "Rdfile"),
                                                  .Platform$file.sep) [[1]]
                                name <- name [length (name)]
                                cbind (alias,
                                       rep (topic, length (alias)),
                                       rep (name, length (alias)))
                               })
    }

    alias_topic <- do.call (rbind, alias_topic)
    index <- seq (nrow (alias_topic))
    if (!is.null (x))
        index <- match (x, alias_topic [, 1])
    res <- data.frame (alias_topic [index, ])
    names (res) <- c ("alias", "topic", "name")
    return (res)
}
m_fns_to_topics <- memoise::memoise (fns_to_topics)

#' @param topic A single .Rd topic
#' @return List of aliases associated with that .Rd topic
#' @noRd
topic_to_fns <- function (topic, package) {
    alias_topic <- m_fns_to_topics (package = package)
    return (alias_topic$alias [alias_topic$topic == topic])
}
m_topic_to_fns <- memoise::memoise (topic_to_fns)
