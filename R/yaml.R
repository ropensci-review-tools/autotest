
parse_yaml_template <- function (yaml = NULL, filename = NULL) {
    if (is.null (yaml) & is.null (filename)) {
        d <- file.path (here::here(), "tests")
        filename <- file.path (d, "autotest.yaml")
        yaml <- readLines (filename)
    } else if (!is.null (filename))
        yaml <- readLines (filename)

    load_libraries (yaml)

    # YAML spec dictates "y", "yes", "Y", and so on are converted to boolean.
    # These handlers prevent that
    # see https://github.com/viking/r-yaml/issues/5
    handlers <- list('bool#yes' = function(x) {
                         if (substr (tolower (x), 1, 1) == "y")
                             x
                         else
                             TRUE   },
                      "bool#no" = function(x) {
                          if (substr (tolower (x), 1, 1) == "n")
                              x
                          else
                              TRUE  })

    x <- yaml::yaml.load (yaml, handlers = handlers)

    parameters <- preprocess <- list ()
    fn_names <- NULL

    for (f in x$functions) {
        fn_names <- c (fn_names, names (f))

        i <- f [[1]]
        nms <- vapply (i, function (i) names (i), character (1))

        parameters [[length (parameters) + 1]] <-
            i [[which (nms == "parameters")]]$parameters

        if ("preprocess" %in% nms)
            preprocess [[length (preprocess) + 1]] <-
                i [[which (nms == "preprocess")]]$preprocess
        else
            preprocess [[length (preprocess) + 1]] <- NA_character_

    }
    names (parameters) <- names (preprocess) <- fn_names

    list (package = x$package,
          parameters = parameters,
          preprocess = preprocess)
}

# x is raw yaml from 'readLines' NOY parsed from yaml.load
load_libraries <- function (x, quiet = FALSE) {
    libraries <- vapply (x [grep ("::", x)], function (i) {
                             first_bit <- strsplit (i, "::") [[1]] [1]
                             # then remove everything before space
                             utils::tail (strsplit (first_bit, "\\s+") [[1]], 1) },
                             character (1), USE.NAMES = FALSE)
    # then main package
    this_lib <- gsub ("package:\\s", "", x [grep ("package:", x)])
    libraries <- unique (c (libraries, this_lib))
    if (!quiet) {
        message (cli::col_green (cli::symbol$star, " Loading the following libraries:"))
        cli::cli_ul (libraries)
    }
    suppressMessages (
        chk <- lapply (libraries, function (i)
                       do.call (library, as.list (i)))
        )
}

#' at_yaml_template
#'
#' Generate a 'yaml' template for an 'autotest' in the 'test' directory of
#' current package directory.
#' @export
at_yaml_template <- function () {

    if (!"DESCRIPTION" %in% list.files (here::here ()))
        stop ("You do not appear to be in any directory of a current R package")

    d <- file.path (here::here(), "tests")
    if (!file.exists (d)) {
        dir.create (d)
    }

    f <- file.path (d, "autotest.yaml")
    if (file.exists (f)) {
        message ("yaml template [", f, "] already exists")
    } else {
        con <- file (f, "w")
        writeLines (yaml_template (), con)
        message ("template written to [", f, "]")
        close (con)
    }
}

yaml_template <- function () {
    c ("package: <package_name>",
       "functions:",
       "    - <name of function>:",
       "        - preprocess:",
       "            - '<R code required for pre-processing exlosed in quotation marks>'",
       "            - '<second line of pre-processing code>'",
       "            - '<more code>'",
       "        - parameters:",
       "            - <param_name>: <value>",
       "            - <another_param>: <value>",
       "    - <name of same or different function>::",
       "        - parameters:",
       "            - <param_name>: <value>")
}
