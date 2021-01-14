
report_object <- function (type = "diagnostic",
                           test_name = NA_character_,
                           fn_name = NA_character_,
                           parameter = NA_character_,
                           parameter_type = NA_character_,
                           operation = NA_character_,
                           content = NA_character_,
                           test = TRUE) {

    res <- tibble::tibble (type = type,
                           test_name = test_name,
                           fn_name = fn_name,
                           parameter = parameter,
                           parameter_type = parameter_type,
                           operation = operation,
                           content = content,
                           test = test)

    class (res) <- c ("autotest_package", class (res))

    return (res)
}
