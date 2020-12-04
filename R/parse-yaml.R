
parse_yaml_template <- function (yaml = NULL, filename = NULL) {

    if (is.null (yaml) & is.null (filename)) {
        stop ("either yaml or filename must be given")
    } else if (!is.null (filename))
        yaml <- readLines (filename)

    load_libraries (yaml)

    x <- yaml::yaml.load (yaml, handlers = yaml_handlers ())

    x <- rm_no_param_fns (x)

    parameters <- preprocess <- classes <- list ()
    fn_names <- NULL

    for (f in seq (x$functions)) {
        fn_names <- c (fn_names, names (x$functions [[f]]))

        i <- x$functions [[f]] [[1]]
        nms <- vapply (i, function (i) names (i), character (1))

        # check whether character variables are quoted:
        pars <- i [[which (nms == "parameters") [1]]]$parameters
        is_char <- which (vapply (pars, function (i)
                                  is.character (i [[1]]), logical (1)))
        # then check whether yaml vals are quoted:
        index <- grep ("- parameters:$", yaml)
        if (length (index) > 0) {
            if (f < length (x$functions)) {
                index <- index [f]:(index [f + 1] - 2)
            } else {
                index <- index [f]:length (yaml)
            }

            yaml2 <- yaml [index]
            for (p in is_char) {
                ystr <- paste0 ("- ", names (pars [[p]]), ":")
                # specific processing because yaml itself reserves "null"
                if (ystr == "- null:")
                    ystr <- "- \"null\":"
                yaml_version <- gsub ("^\\s+", "",
                                      strsplit (yaml2 [grep (ystr, yaml2)],
                                                ystr) [[1]] [2])
                if (!grepl ("\"|\'", yaml_version)) {
                    if (grepl ("formula", names (pars [[p]]),
                               ignore.case = TRUE)) {
                        pars [[p]] [[1]] <- stats::formula (pars [[p]] [[1]])
                        attr (pars [[p]] [[1]], ".Environment") <- NULL
                    } else
                        pars [[p]] [[1]] <- as.name (pars [[p]] [[1]])
                }
            }

            parameters [[length (parameters) + 1]] <- pars
        } else
            parameters [[length (parameters) + 1]] <- NA_character_

        if ("preprocess" %in% nms)
            preprocess [[length (preprocess) + 1]] <-
                i [[which (nms == "preprocess")]]$preprocess
        else
            preprocess [[length (preprocess) + 1]] <- NA_character_

        if ("class" %in% nms)
            classes [[length (classes) + 1]] <-
                i [[which (nms == "class")]]$class [[1]]
        else
            classes [[length (classes) + 1]] <- NA_character_
    }
    names (parameters) <- names (preprocess) <- names (classes) <- fn_names

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
    handlers <- list("bool#yes" = function(x) {
                         if (substr (tolower (x), 1, 1) == "y")
                             x
                         else
                             TRUE   },
                      "bool#no" = function(x) {
                          if (substr (tolower (x), 1, 1) == "n")
                              x
                          else
                              TRUE  })

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

# x is raw yaml from 'readLines' NOY parsed from yaml.load
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
