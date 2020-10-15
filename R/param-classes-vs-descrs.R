#' Match parameter descriptions to classes of objects
#'
#' @param yaml An autotest `yaml` including only pre-processing specifications,
#' and NOT including either class or parameter definitions.
#' @param pkg Either name of locally installed package or path to source
#' @return A `data.frame` with the following columns:
#'  1. "parameter" the name of each parameter of the function represented in the
#'  yaml
#'  2. "desc" The text description given for that parameter
#'  3. "class_in_desc" A character describing any object classes which are
#'  directly specified in each parameter description, otherwise `NA` indicating
#'  no class is described
#'  4. "param_classes" Classes of objects from other packages matching any
#'  elements of the textual description; otherwise `NA`.
#'  5. "fn_is_construtor" Logical variable for any non-NA `param_classes`
#'  indicating whether they functions they describe serve as class-constructors.
#' @noRd
param_classes_in_desc <- function (yaml, pkg_full) {

    i <- grep ("functions:", yaml)
    if (length (i) > 1)
        stop ("yaml must only define one single function")
    fn_name <- gsub ("\\s*-\\s*|:", "", yaml [i + 1])

    pkg <- pkg_full

    if (pkg_is_source (pkg_full)) {
        pkg <- get_package_name (pkg_full)
        f <- file.path (pkg_full, "man", paste0 (fn_name, ".Rd"))
        m <- get_Rd_metadata (tools::parse_Rd (f), "arguments")
    } else {
        r <- tools::Rd_db (pkg)
        aliases <- lapply (r, function (i) get_Rd_metadata (i, "alias"))
        i <- vapply (aliases, function (i) fn_name %in% i, logical (1))
        r <- r [[which (i) [1] ]]
        m <- get_Rd_metadata (tools::parse_Rd (r), "arguments")
    }

    m <- strsplit (m, "\\n") [[1]]
    m <- m [m != ""]

    param_names <- unlist (lapply (m, function (i)
                                   eval (parse (text = i)) [[1]] [[1]]))
    param_descs <- unlist (lapply (m, function (i)
                                   eval (parse (text = i)) [[2]] [[1]]))

    class_in_desc <- class_in_main_fn_desc (yaml, fn_name, param_descs)
    names (class_in_desc) <- param_names

    # otherwise check whether any elements of desription name functions from
    # other packages
    index <- which (is.na (class_in_desc))
    param_classes <- fn_is_constructor <- rep (NA_character_, length (param_names))
    if (length (index) > 0) {
        param_classes [index] <- param_desc_is_other_fn (pkg, param_descs [index])

        # Then check that any such functions actually serve to construct class-based
        # objects:
        index <- which (!is.na (param_classes))
        if (length (index) > 0) {
            fn_is_constructor [index] <- vapply (param_classes [index],
                                                 function (i)
                                                     is_fn_a_constructor (i),
                                                 logical (1))
        }
    }

    ret <- data.frame (parameter = param_names,
                       desc = param_descs,
                       class_in_desc = class_in_desc,
                       param_classes = param_classes,
                       fn_is_constructor = fn_is_constructor,
                       stringsAsFactors = FALSE,
                       row.names = seq_along (param_names))

    return (ret)
}

class_in_main_fn_desc <- function (yaml, fn_name, param_descs) {
    res <- parse_yaml_template (yaml)
    e <- new.env ()
    eval (parse (text = res$preprocess [[fn_name]]), envir = e)
    classes <- unique (unlist (lapply (ls (envir = e), function (i)
                                       class (get (i, envir = e)))))
    class_in_desc <- vapply (param_descs, function (i) {
                                 i_s <- gsub ("\"|\'|\`", "", strsplit (i, " ") [[1]])
                                 i_s <- gsub ("^.*\\{\\}$", "", i_s)
                                 chk <- vapply (i_s, function (j)
                                                any (grepl (j, classes)),
                                                logical (1),
                                                USE.NAMES = FALSE)
                                 return (i_s [which (chk) [1]]) },
                                 character (1),
                                 USE.NAMES = FALSE)
    return (class_in_desc)
}

#' Does a parameter description refer to a function from another package?
#'
#' @param pkg Name of installed package or path to local source
#' @param param_descs Vector of character descriptions given to each function of a
#' pacakge
#' @return Vector of same length as `param_descs` with `NA` for descriptions
#' which contain no elements matching functions from other pacakge, otherwise a
#' single
#' string of "\<package\>:\<function\>".
#' @noRd
param_desc_is_other_fn <- function (pkg, param_descs) {
    plist <- get_pkg_deps (pkg)
    for (p in plist)
        if (!paste0 ("package:", p) %in% search ())
            suppressMessages (library (p, character.only = TRUE))
    allfns <- lapply (search (), function (i) ls (envir = as.environment (i)))
    names (allfns) <- search ()

    param_classes <- vapply (param_descs, function (i) {
                                 i_s <- gsub ("\"|\'|\`|^.*\\{|\\}$", "",
                                              strsplit (i, " ") [[1]])
                                 s_in_pkg <- function (s, f) {
                                     vapply (f, function (j) s %in% j, logical (1))
                                 }
                                 res <- vapply (i_s, function (j) {
                                                    res <- s_in_pkg (j, allfns)
                                                    res <- names (res) [which (res) [1]]
                                                    return (gsub ("package:", "", res))
                                              }, character (1),
                                              USE.NAMES = FALSE)
                                 index <- which (!is.na (res))
                                 res <- res [index [1]]
                                 if (!is.na (res))
                                     res <- paste0 (res, "::", i_s [index [1]])
                                 return (res)
                                 }, character (1),
                                 USE.NAMES = FALSE)
    return (param_classes)
}

#' Does the `fn` f construct a class-based object?
#'
#' @param fn Name of a function in form "\<package\>:\<function\>"
#' @param phrases Vector of phrases to `grep` for in function title or return
#' value to determine whether it is constructing a class-based object.
#' @return `TRUE` is that function serves to construct an object with a defined
#' class
#'
#' @note The `phrases` parameter is case-insensitive, and are OR-combined, so
#' matching any one will suffice.
#' @noRd
is_fn_a_constructor <- function (fn, phrases = c ("create", "construt", "object", "class")) {
    fn <- strsplit (fn, "::") [[1]]
    this_pkg <- fn [1]
    this_fn <- fn [2]
    # get Rd for topic
    h <- tools::Rd_db (package = this_pkg)
    aliases <- lapply (h, function (i) get_Rd_metadata (i, "alias"))
    i <- vapply (aliases, function (j) this_fn %in% j, logical (1))
    h <- h [[which (i)]]

    h_title <- get_Rd_metadata (h, "title")
    h_value <- get_Rd_metadata (h, "value")

    is_constructor <- grepl ("create|construct|object|class",
                             paste0 (h_title, h_value, collapse = " "),
                             ignore.case = TRUE)

    return (is_constructor)
}
