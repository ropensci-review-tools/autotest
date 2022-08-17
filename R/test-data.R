
#' Do `test_data` indicate whether a particular test should be conducted?
#'
#' Extract flag of test_data$test corresponding to the `autotest_obj` object,
#' `obj`, which contains function and parameter names.
#' @return A zero-length parameter if "test_data$test_name" is not in
#' "obj$test_name"
#' @noRd
test_these_data <- function (test_data, obj) {

    test_data <- test_data [which (test_data$test_name %in% obj$test_name), ]

    if (nrow (test_data) > 1 & any (obj$fn_name %in% test_data$fn_name))
        test_data <- test_data [test_data$fn_name %in% obj$fn_name, ]

    if (nrow (test_data) > 1 & any (obj$parameter %in% test_data$parameter))
        test_data <- test_data [test_data$parameter %in% obj$parameter, ]

    if (length (unique (test_data$test)) > 1)
        stop ("Cannot determine single 'test' flag from 'test_data' for\n",
              "   function = ", obj$fn_name, "\n",
              "   parameter = ", obj$parameter)

    return (unique (test_data$test))
}
