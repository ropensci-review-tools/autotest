
parse_yaml_template <- function (yaml = NULL, filename = NULL) {

    if (is.null (yaml) & is.null (filename)) {
        stop ("either yaml or filename must be given")
    } else if (!is.null (filename)) {
        yaml <- readLines (filename)
    }

    load_libraries (yaml)

    x <- yaml::yaml.load (yaml, handlers = yaml_handlers ())

    x <- rm_no_param_fns (x)

    parameters <- preprocess <- classes <- list ()
    fn_names <- NULL

    for (f in seq (x$functions)) {
        fn_names <- c (fn_names, names (x$functions [[f]]))

        pars <- NA_character_
        if (any (grepl ("- parameters:$", yaml)))
            pars <- parse_one_fn (x, f, yaml)
        parameters [[length (parameters) + 1]] <- pars

        preprocess <- add_yaml_prepro (preprocess, x, f)

        classes <- add_yaml_classes (classes, x, f)
    }
    names (parameters) <- names (preprocess) <- names (classes) <- fn_names

    parameters <- set_seq_storage_mode (parameters)
    parameters <- param_had_dot (yaml, parameters)

    list (package = x$package,
          parameters = parameters,
          preprocess = preprocess,
          classes = classes)
}

#' handlers for yaml parsing
#'
#' YAML spec dictates "y", "yes", "Y", and so on are converted to boolean.
#' These handlers prevent that
#' see https://github.com/viking/r-yaml/issues/5
#' @noRd
yaml_handlers <- function () {

    bool_yes <- function (x) {
        if (substr (tolower (x), 1, 1) == "y")
            return (x)
        else
            return (TRUE)
    }

    bool_no <- function (x) {
        if (substr (tolower (x), 1, 1) == "n")
            return (x)
        else
            return (FALSE)
    }

    int_handler <- function (x) {
        if (substring (x, nchar (x), nchar (x)) == "L")
            return (as.integer (x))
        else
            return (as.double (x))
    }

    str_handler <- function (x) {
        index <- as.integer (gregexpr ("[0-9]", x) [[1]])
        if (identical (index, seq_len (nchar (x) - 1)) &
            substring (x, nchar (x), nchar (x)) == "L")
            return (as.integer (substring (x, 1, nchar (x) - 1)))
        else
            return (x)
    }

    handlers <- list("bool#yes" = bool_yes,
                     "bool#no" = bool_no,
                     "int" = int_handler,
                     "str" = str_handler)

    return (handlers)
}

#' Remove any functions with no parameters
#'
#' rm any functions with parameters == "(none)", as set at end of
#' `one_ex_to_yaml()`:
#' @param x yaml function definition loaded by yaml.load
#' @return Modified version of input, `x`, after removal of any functions with
#' no parameters.
#' @noRd
rm_no_param_fns <- function (x) {

    no_params <- vapply (x$functions, function (i) {
                             fi <- i [[1]] [[1]] # (name, empty [[1]] item)
                             res <- FALSE
                             if ("parameters" %in% names (fi)) {
                                 if (length (fi$parameters) == 1 &
                                     all (fi$parameters == "(none)"))
                                     res <- TRUE
                             }
                             return (res) },
                             logical (1))

    x$functions <- x$functions [which (!no_params)]

    return (x)
}

parse_one_fn <- function (x, f, yaml) {

    i <- x$functions [[f]] [[1]]
    nms <- get_fn_names (x, f)

    # rm all other fns from yaml except 'f':
    yfns <- grep (paste0 ("^", yaml_indent (1), "-\\s[[:alpha:]]"), yaml)
    yfns_end <- c (yfns [-1] - 1, length (yaml))
    index <- yfns [f]:yfns_end [f]
    yaml <- c (yaml [1:(yfns [1] - 1)],
               yaml [index])

    # check whether character variables are quoted:
    pars <- i [[which (nms == "parameters") [1]]]$parameters
    is_char <- which (vapply (pars, function (j)
                              is.character (j [[1]]), logical (1)))
    # then check whether yaml vals are quoted:
    index <- grep ("- parameters:$", yaml)
    if (length (index) > 0) {

        yaml2 <- yaml [(index [1] + 1):length (yaml)] # the parameters

        for (p in is_char) {
            ystr <- paste0 ("- ", names (pars [[p]]), ":")
            # specific processing because yaml itself reserves "null"
            if (ystr == "- null:")
                ystr <- "- \"null\":"
            yaml_version <- gsub ("^\\s+", "",
                                  strsplit (yaml2 [grep (ystr, yaml2)],
                                            ystr) [[1]] [2])

            if (!grepl ("\"|\'", yaml_version)) {

                is_formula <- grepl ("~", paste0 (pars [[p]]))
                if (is_formula) {
                    f <- tempfile ()
                    is_formula <- is.null (catch_all_msgs (tempfile (),
                                                           "as.formula",
                                                           unname (pars [[p]])))
                }

                if (is_formula) {
                    pars [[p]] [[1]] <- as.formula (pars [[p]] [[1]])
                } else {
                    pars [[p]] [[1]] <- as.name (pars [[p]] [[1]])
                }
            }
        }
    }

    return (pars)
}

#' Get names of functions from parsed yaml
#'
#' @param x yaml version of examples from one package
#' @param f integer index of one entry in f
#' @return Names of all functions included in the f'th entry of x
#' @noRd
get_fn_names <- function (x, f) {
    vapply (x$functions [[f]] [[1]], function (j)
            names (j), character (1))
}

add_yaml_prepro <- function (preprocess, x, f) {

    this_fn <- x$functions [[f]] [[1]]
    nms <- get_fn_names (x, f)

    if ("preprocess" %in% nms)
        preprocess [[length (preprocess) + 1]] <-
            this_fn [[which (nms == "preprocess")]]$preprocess
    else
        preprocess [[length (preprocess) + 1]] <- NA_character_

    return (preprocess)
}

add_yaml_classes <- function (classes, x, f) {

    this_fn <- x$functions [[f]] [[1]]
    nms <- get_fn_names (x, f)

    if ("class" %in% nms)
        classes [[length (classes) + 1]] <-
            this_fn [[which (nms == "class")]]$class [[1]]
        else
            classes [[length (classes) + 1]] <- NA_character_

    return (classes)
}

# x is raw yaml from 'readLines' NOT parsed from yaml.load
load_libraries <- function (x, quiet = FALSE) {
    libraries <- vapply (x [grep ("::", x)], function (i) {
                             first_bit <- strsplit (i, "::") [[1]] [1]
                             # then remove everything before space
                             utils::tail (strsplit (first_bit, "\\s+") [[1]], 1)
          },
          character (1),
          USE.NAMES = FALSE)
    # then main package
    this_lib <- gsub ("package:\\s", "", x [grep ("package:", x)])
    libraries <- unique (c (libraries, this_lib))
    # these can pre pre-pended with a single `'` when part of pre-processing,
    # so:
    libraries <- gsub ("^\\'", "", libraries)
    libraries <- libraries [which (!libraries %in% loadedNamespaces ())]
    if (!quiet & length (libraries) > 0) {
        message (cli::col_green (cli::symbol$star,
                                 " Loading the following libraries:"))
        cli::cli_ul (libraries)
        suppressMessages (
                          chk <- lapply (libraries, function (i)
                                         do.call (library, as.list (i)))
        )
    }
}

#' at_yaml_template
#'
#' Generate a 'yaml' template for an 'autotest'.
#' @param loc Location to generate template file. Append with filename and
#' '.yaml' suffix to overwrite default name of 'autotest.yaml', otherwise this
#' parameter will be used to specify directory only.
#' @family yaml
#' @export
at_yaml_template <- function (loc = tempdir ()) {

    if (!grepl ("\\.yaml$", loc [1])) {
        if (!file.exists (loc))
            stop ("Directory [", loc, "] does not exist")
        loc <- file.path (loc, "autotest.yaml")
    }

    if (file.exists (loc)) {
        message ("yaml template [", loc, "] already exists")
    } else {
        con <- file (loc, "w")
        writeLines (yaml_template (), con)
        message ("template written to [", loc, "]")
        close (con)
    }
}

yaml_template <- function () {
    c ("package: <package_name>",
       "functions:",
       "    - <name of function>:",
       "        - preprocess:",
       "            - '<R code required for pre-processing exlosed in quotation marks>'", # nolint
       "            - '<second line of pre-processing code>'",
       "            - '<more code>'",
       "        - parameters:",
       "            - <param_name>: <value>",
       "            - <another_param>: <value>",
       "    - <name of same or different function>::",
       "        - parameters:",
       "            - <param_name>: <value>")
}

#' any YAML sequences like `[1 2 3]` are converted to "double" storage mode.
#' This function reverts to "int" so that test_double_is_int is not triggered
#' @param p parameters part of `parse_yaml_template` result
#' @return Same thing but with modified storage.mode
#' @noRd
set_seq_storage_mode <- function (p) {

    p <- lapply (p, function (i) {
                 i <- lapply (i, function (j) {
                              if (storage.mode (j [[1]]) == "double" &
                                  length (j [[1]]) > 1) {
                                  if (all (abs (j [[1]] - round (j [[1]])) <
                                           .Machine$double.eps))
                                      storage.mode (j [[1]]) <- "integer"
                              }
                              return (j)
                                 })
                 return (i)
       })

    return (p)
}

#' Parameters intended to be double should be specified as `1.` or `1.0`. This
#' routine checks for such as sets `attr(., "is_int") <- FALSE`, to then switch
#' off `test_double_is_int`.
#' @param p parameters part of `parse_yaml_template` result
#' @noRd
param_had_dot <- function (yaml, p) {

    p <- lapply (p, function (i) {
                     lapply (i, function (j) {
                                 nm <- names (j)
                                 ptn <- paste0 ("-\\s?", nm, ":")
                                 ln <- strsplit (grep (ptn, yaml, value = TRUE),
                                                 ptn)
                                 if (storage.mode (j [[1]]) == "double" &
                                     length (j [[1]]) == 1 &
                                     grepl ("\\.", ln [[1]] [2]))
                                     attr (j [[1]], "is_int") <- FALSE
                                 return (j)
                                 })
       })

    return (p)
}
