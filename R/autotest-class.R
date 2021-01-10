#' autotest_obj class definition
#'
#' @param package Name of package for which object is to be constructed.
#' @param fn_name Name of function to be tested.
#' @param parameters Names of all parameters for that function.
#' @param parameter_types Types of input parameters.
#' @param class Class of an individual parameter.
#' @param classes Classes of all parameters.
#' @param env Environment in which tests are to be run.
#' @param test If `FALSE`, return only descriptions of tests which would be run
#' with `test = TRUE`, without actually running them.
#' @param quiet If `FALSE`, issue progress and other messages during testing of
#' object.
#'
#' @export
autotest_obj <- function (package,
                          fn_name,
                          parameters,
                          parameter_types,
                          class = NULL,
                          classes,
                          env = new.env (),
                          test = FALSE,
                          quiet = FALSE) {

    x <- list (package = package,
               params = parameters,
               param_types = parameter_types,
               fn = fn_name,
               class = class,
               classes = classes,
               env = env,
               test = test,
               quiet = quiet)

    class (x) <- "autotest_obj"

    return (x)
}
