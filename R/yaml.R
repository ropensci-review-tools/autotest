
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
        x <- yaml_template ()
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
