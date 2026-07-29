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
        if (!file.exists (loc)) {
            stop ("Directory [", loc, "] does not exist")
        }
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
    c (
        "package: <package_name>",
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
        "            - <param_name>: <value>"
    )
}
