
parse_yaml_template <- function (yaml = NULL, filename = NULL) {
    if (is.null (yaml) & is.null (filename)) {
        d <- file.path (here::here(), "tests")
        filename <- file.path (d, "autotest.yaml")
        yaml <- readLines (filename)
    } else if (!is.null (filename))
        yaml <- readLines (filename)

    load_libraries (yaml)

    x <- yaml::yaml.load (yaml)

    datasets <- preprocess <- list ()
    for (f in x$functions) {
        this_fn <- names (f)
        i <- f [[1]]
        datasets [[length (datasets) + 1]] <-
            vapply (i, function (j)
                    j$data [[1]]$name,
                    character (1))
        preprocess [[length (preprocess) + 1]] <-
            lapply (i, function (j)
                    j$data [[2]]$preprocess)
    }

    list (datasets = datasets,
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
    chk <- lapply (libraries, function (i)
                   do.call (library, as.list (i)))
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
       "libraries:",
       "    - <required library 1>",
       "    - <required library 2>",
       "functions:",
       "    - <name of first function>:",
       "        - data:",
       "            - name: <name of a data set, in pkg::name format when from another package>",
       "            - preprocess:",
       "                - '<R code required for pre-processing enclosed in quotation marks>'",
       "                - '<second line of pre-processing code>'",
       "                - '<more code>'",
       "        - data:",
       "            - name: <name of another data set which may be submitted to function>",
       "            - preprocess:",
       "                - '<more lines of pre-processing code>'",
       "    - <name of second function>:",
       "        - data: <single data set>",
       "        - preprocess: '<single line of pre-processing code>'")
}
