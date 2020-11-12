#' @param p Result from `tools:::.Rd_get_metadata (., "arguments")`. Instances
#' that are not able to be parsed return NA and are subsequently removed,
#' meaning this will not necessarily catch all
#' @noRd
parse_one_params <- function (p) {
    nm_desc <- vapply (p, function (i) {
                           res <- tryCatch (eval (parse (text = i)),
                                            error = function (e) NULL)
                           # TODO: Use `formals` to at least have names of all
                           # fn parameters
                           if (is.null (res))
                               return (rep (NA_character_, 2))

                           name <- res [[1]] [[1]]
                           desc <- gsub ("\\n|\\t", "",
                                         paste0 (unlist (res [[2]]),
                                                 collapse = " "))
                           c (name, desc)
                           return (c (name, desc)) },
                           character (2), USE.NAMES = FALSE)
    index <- apply (nm_desc, 2, function (i) !all (is.na (i)))
    data.frame (param = nm_desc [1, index],
                descr = nm_desc [2, index],
                stringsAsFactors = FALSE)
}

#' @return List of data frames, one for each Rd file, containing parameter
#' names, descriptions, and name of Rd file.
#' @noRd
get_param_lists <- function (package) {

    if (pkg_is_source (package)) {
        f <- file.path (package, "man")
        flist <- list.files (f, full.names = TRUE, pattern = "*\\.Rd$")
        rdnames <- gsub ("\\.Rd$", "",
                        list.files (f, full.names = FALSE, pattern = "*\\.Rd$"))
        params <- lapply (flist, function (i)
                          get_Rd_metadata (tools::parse_Rd (i), "arguments"))
        names (params) <- rdnames
        fn_names <- vapply (flist, function (i)
                            get_Rd_metadata (tools::parse_Rd (i), "alias") [1],
                            character (1), USE.NAMES = FALSE)
    } else {
        r <- tools::Rd_db (package)
        rdnames <- gsub ("\\.Rd$", "", names (r))
        params <- lapply (r, function (i) get_Rd_metadata (i, "arguments"))
        names (params) <- rdnames
        fn_names <- unname (unlist (lapply (r, function (i)
                                            get_Rd_metadata (i, "alias") [1])))
    }

    index <- which (vapply (params, length, integer (1)) > 0)
    params <- lapply (params [index], function (i) {
                      res <- strsplit (i, "\\n") [[1]]
                      res [!grepl ("^\\s*$", res)]  })
    fn_names <- fn_names [index]

    params <- lapply (seq_along (params), function (i) {
                          res <- parse_one_params (params [[i]])
                          res$rdname <- names (params) [i]
                          return (res)  })
    for (i in seq_along (params))
        params [[i]]$fn_name <- fn_names [i]
    params <- do.call (rbind, params)

    return (params)
}
m_get_param_lists <- memoise::memoise (get_param_lists)

pkg_param_classes <- function (package) {

    objs <- m_get_example_objs (package)
    objs$package <- vapply (objs$package, function (i)
                            strsplit (i, "::") [[1]] [1],
                            character (1))

    base_pkgs <- c ("stats", "graphics", "grDevices", "utils",
                    "datasets", "methods", "base")
    objs <- objs [which (!objs$package %in% base_pkgs), ]

    return (objs)
}

package_param_descs <- function (package) {

    return (m_get_param_lists (package))

}

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

    params <- m_get_param_lists (pkg_full)
    classes <- pkg_param_classes (pkg_full)

    # classes includes all objects created in any pre-processing stages. Then
    # just need to check whether the `parameter` stage of the `yaml` creates any
    # other objects
    param_classes <- yaml_param_classes (yaml)

    fn <- gsub (":", "",
                strsplit (yaml [grep ("^functions:", yaml) + 1],
                          "- ") [[1]] [2])
    these_params <- params [params$fn_name == fn, ]
    these_classes <- classes [classes$alias == fn, ]

    class_in_desc <- NULL
    if (nrow (these_params) > 0) {
        class_in_desc <- vapply (seq_along (nrow (these_params)), function (i) {
                                     class_i <- these_classes$object
                                     these <- these_params$params [i]
                                     if (these %in% names (param_classes))
                                         class_i <- c (class_i,
                                                       param_classes [[these]])
                                     chk <- vapply (class_i, function (j)
                                            any (grepl (j,
                                                        these_params$descr [i],
                                                        ignore.case = TRUE)),
                                                    logical (1))
                                     ret <- NA_character_
                                     if (any (chk))
                                         ret <- names (chk) [which (chk) [1]]
                                     return (ret)
                }, character (1), USE.NAMES = FALSE)
    }

    ret <- data.frame (parameter = these_params$param,
                       desc = these_params$descr,
                       class_in_desc = class_in_desc,
                       stringsAsFactors = FALSE)

    return (ret)
}

yaml_param_classes <- function (yaml) {
    # reduce to only preprocess and parameters clauses
    yaml <- yaml [-grep ("^[a-z]", yaml)]
    n <- nchar (yaml_indent (1))
    fndef <- paste0 (c ("^", rep ("\\s", n), "\\-"), collapse = "")
    yaml <- yaml [-grep (fndef, yaml)]

    pre_start <- grep ("\\- preprocess:", yaml) + 1L
    par_start <- grep ("\\- parameters:", yaml) + 1L
    classes <- grep ("\\- classes:", yaml)
    index <- sort (c (par_start - 1L, classes))
    pre_end <- vapply (pre_start, function (i) {
                       res <- index [which (index > i) [1]] - 1L
                       if (is.na (res))
                           res <- length (yaml)
                       return (res) },
                       integer (1))
    index <- sort (c (pre_start - 1L, classes))
    par_end <- vapply (par_start, function (i) {
                       res <- index [which (index > i) [1]] - 1L
                       if (is.na (res))
                           res <- length (yaml)
                       return (res) },
                       integer (1))

    pre_index <- unlist (lapply (seq_along (pre_start), function (i)
                                 pre_start [i]:pre_end [i]))
    par_index <- unlist (lapply (seq_along (par_start), function (i)
                                 par_start [i]:par_end [i]))

    yaml_pre <- gsub ("^\\s*\\-\\s*", "", yaml [pre_index])
    index <- which (grepl ("^\'.*\'$", yaml_pre) | grepl ("^\".*\"$", yaml_pre))
    if (length (index) > 0)
        yaml_pre [index] <- gsub ("^\'|^\"|\"$|\'$", "", yaml_pre [index])
    e <- new.env ()
    x <- tryCatch (eval (parse (text = yaml_pre), envir = e)) # nolint

    yaml_pars <- gsub ("^\\s*\\-\\s*", "", yaml [par_index])
    objs <- vapply (yaml_pars, function (i)
                    strsplit (i, ": ") [[1]] [2],
                    character (1), USE.NAMES = FALSE)
    params <- vapply (yaml_pars, function (i)
                      strsplit (i, ": ") [[1]] [1],
                      character (1), USE.NAMES = FALSE)
    params <- gsub ("\\s*-\\s?", "", params)

    classes <- lapply (seq_along (objs), function (i) {
                           res <- tryCatch (
                                     class (eval (parse (text = objs [i]),
                                                  envir = e)),
                                     error = function (z) "error")
                           if (i > 1 & "error" %in% res) {
                               iprev <- i - 1
                               while (res == "error") {
                   res <- tryCatch (class (eval (parse (text = objs [i]),
                                                 envir = get (objs [iprev]))),
                                    error = function (z) "error")
                               }
                           }
                           return (res)
                           })
    names (classes) <- params

    return (classes)
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
is_fn_a_constructor <- function (fn,
                                 phrases = c ("create",
                                              "construt",
                                              "object",
                                              "class")) {
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


#' Does a parameter description refer to a function from another package?
#'
#' @param pkg Name of installed package or path to local source
#' @param param_descs Vector of character descriptions given to each function
#' of a pacakge
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
    s <- search ()
    s <- s [grep ("^package:", s)]
    allfns <- lapply (s, function (i) ls (envir = as.environment (i)))
    names (allfns) <- s

    # then also get all Rd topics
    topics <- lapply (s, function (i) {
                          rd <- tools::Rd_db (gsub ("^package:", "", i))
                          vapply (rd, function (j) get_Rd_metadata (j, "name"),
                                  character (1), USE.NAMES = FALSE)
                             })
    for (i in seq_along (allfns)) {
        allfns [[i]] <- unique (c (allfns [[i]], topics [[i]]))
    }

    match_txt_to_pkg <- function (txt, allfns) {
        pkg_index <- vapply (allfns, function (i) any (grepl (txt, i)),
                             logical (1))
        pkgs <- names (pkg_index) [which (pkg_index)]
        if (length (pkgs) > 0) {
            # check whether any topics are "class" descriptions:
            nms <- lapply (pkgs, function (i)
                           allfns [[i]] [grep (txt, allfns [[i]])])
            has_class <- vapply (nms, function (i)
                                 any (grepl ("class|as\\.", i)),
                                 logical (1))
            if (any (has_class))
                pkgs <- pkgs [which (has_class)]
        }
        # otherwise just default to first-listed package
        return (pkgs [1])
    }

    param_classes <- vapply (param_descs, function (i) {
                                 i_s <- gsub ("\"|\'|\`|^.*\\{|\\}$", "",
                                              strsplit (i, " ") [[1]])
                                 res <- vapply (i_s, function (txt) {
                                                    match_txt_to_pkg (txt,
                                                                      allfns)
                                              }, character (1),
                                              USE.NAMES = FALSE)
                                 index <- which (!is.na (res))
                                 res <- res [index [1]]
                                 if (!is.na (res))
                                     res <- paste0 (res, "::", i_s [index [1]])
                                 return (res)
                                 }, character (1),
                                 USE.NAMES = FALSE)
    return (gsub ("^package:", "", param_classes))
}

#' @param classes A single line of the return value of `param_classes_in_desc`,
#' containing a parameter value and associated class in `class_in_desc`.
#' @return `yaml` with additional line restricting class of the specified object
#' to the specified value.
#' @noRd
add_class_restriction <- function (yaml, classes) {
    if (!any (grepl ("- class:", yaml)))
        yaml <- c (yaml, paste0 (yaml_indent (2), "- class:"))

    yaml <- c (yaml,
               paste0 (yaml_indent (3),
                       "- ",
                       classes$parameter [1],
                       ": ",
                       classes$class_in_desc [1]))
    return (yaml)
}
