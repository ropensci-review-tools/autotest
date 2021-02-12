
#' Do `test_data` indicate whether a particular test should be conducted?
#'
#' Extract flag of test_data$test corresponding to the `autotest_obj` object,
#' `obj`, which contains function and parameter names.
#' @noRd
test_these_data <- function (test_data, obj) {

    test_data <- test_data [which (test_data$test_name == obj$test_name), ]

    if (nrow (test_data) > 1 & obj$fn_name %in% test_data$fn_name)
        test_data <- test_data [test_data$fn_name == obj$fn_name, ]

    if (nrow (test_data) > 1 & obj$parameter %in% test_data$parameter)
        test_data <- test_data [test_data$parameter == obj$parameter, ]

    if (length (unique (test_data$test)) > 1)
        stop ("Cannot determine single 'test' flag from 'test_data' for\n",
              "   function = ", obj$fn_name, "\n",
              "   parameter = ", obj$parameter)

    return (unique (test_data$test))
}
