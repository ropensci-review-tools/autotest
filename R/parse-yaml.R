#' handlers for yaml parsing
#'
#' YAML spec dictates "y", "yes", "Y", and so on are converted to boolean.
#' These handlers prevent that
#' see https://github.com/viking/r-yaml/issues/5
#' @noRd
yaml_handlers <- function () {

    bool_yes <- function (x) {
        if (substr (tolower (x), 1, 1) == "y") {
            return (x)
        } else {
            return (TRUE)
        }
    }

    bool_no <- function (x) {
        if (substr (tolower (x), 1, 1) == "n") {
            return (x)
        } else {
            return (FALSE)
        }
    }

    int_handler <- function (x) {
        if (substring (x, nchar (x), nchar (x)) == "L") {
            return (as.integer (x))
        } else {
            return (as.double (x))
        }
    }

    str_handler <- function (x) {
        index <- as.integer (gregexpr ("[0-9]", x) [[1]])
        if (length (index) <= 1) {
            return (x)
        }

        if (identical (index, seq_len (nchar (x) - 1)) &
            substring (x, nchar (x), nchar (x)) == "L") {
            return (as.integer (substring (x, 1, nchar (x) - 1)))
        } else {
            return (x)
        }
    }

    handlers <- list (
        "bool#yes" = bool_yes,
        "bool#no" = bool_no,
        "int" = int_handler,
        "str" = str_handler
    )

    return (handlers)
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
