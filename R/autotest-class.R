#' autotest_obj class definition
#'
#' @param package Name of package for which object is to be constructed.
#' @param package_loc Location of package on local system (for source packages
#' only)
#' @param test_name Name of test (use \link{autotest_types} to get all test
#' names).
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
autotest_obj <- function (package = NA_character_,
                          package_loc = NULL,
                          test_name = NA_character_,
                          fn_name = NA_character_,
                          parameters = list (),
                          parameter_types = NA_character_,
                          class = NULL,
                          classes = NULL,
                          env = new.env (),
                          test = FALSE,
                          quiet = FALSE) {

    x <- list (package = package,
               package_loc = package_loc,
               test_name = test_name,
               params = parameters,
               param_types = parameter_types,
               fn = fn_name,
               class = class,
               classes = classes,
               env = env,
               test = test,
               quiet = quiet)

    if (is.null (x$package_loc))
        x$package_loc <- x$package

    class (x) <- "autotest_obj"

    return (x)
}
