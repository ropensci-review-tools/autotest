#' Get names, values, types and classes of parameters
#'
#' @param trace_data Result of a single 'typetracer' trace.
#' @param fn_pars Result of \link{get_unique_fn_pars} applied to a single trace.
#' @return A `list` of 4 item of "value", "type" and "class", and "storage_mode"
#' of each parameter, where "type" is one of "single", "vector", or "tabular"
#' (or otherwise NA).
#' @noRd
get_param_info <- function (trace_data, fn_pars) {

    # get parameter values:
    par_index <- which (!nzchar (names (trace_data)))
    par_names_i <- vapply (trace_data [par_index], function (j) j$par, character (1L))
    par_vals_i <- lapply (trace_data [par_index], function (j) j$par_eval)
    names (par_vals_i) <- par_names_i
    index <- which (!vapply (par_vals_i, is.null, logical (1L)))
    par_vals_i <- par_vals_i [index]
    par_names_i <- par_names_i [index]

    # get parameter classes & types:
    index <- which (fn_pars$fn_name == trace_data$fn_name &
                    fn_pars$par_name %in% par_names_i)
    fn_pars_i <- fn_pars [index, ]
    fn_pars_i <- fn_pars_i [match (fn_pars_i$par_name, par_names_i), ]

    index <- which (par_names_i %in% fn_pars_i$par_name)
    par_vals_i <- par_vals_i [index]
    par_names_i <- par_names_i [index]

    # param_types are in [single, vector, tabular]
    param_types <- rep (NA_character_, nrow (fn_pars_i))

    is_single <- vapply (fn_pars_i$length, function (j)
        all (as.integer (strsplit (j, ",") [[1]]) <= 1L),
        logical (1L))
    param_types [which (is_single)] <- "single"

    is_vector <- vapply (fn_pars_i$length, function (j)
        any (as.integer (strsplit (j, ",") [[1]]) > 1L),
        logical (1L))
    param_types [which (is_vector)] <- "vector"

    is_rect <- vapply (trace_data [par_index], function (j)
        j$typeof == "list" && length (dim (j$par_eval)) == 2,
        logical (1L))
    param_types [which (is_rect)] <- "tabular"

    # reduce class to first non-generic value only
    # start by removing generic classes, which may be first of several items, so
    # first remove all ", " versions.
    atomics <- paste0 (atomic_modes (), collapse = "|")
    atomics <- paste0 (atomics, "|matrix|array|data\\.frame")
    ptn <- paste0 ("(", atomics, "),\\s*")
    param_class <- gsub (ptn, "", fn_pars_i$class)
    param_class <- gsub (atomics, "", param_class)
    param_class <- gsub ("^\\,\\s+", "", param_class)

    param_class [which (!nzchar (param_class))] <- NA_character_

    data.frame (
        name = par_names_i,
        value = I (par_vals_i),
        type = param_types,
        class = param_class,
        storage_mode = fn_pars_i$storage_mode
    )
}


# add attributes to elements of `autotest_object` `x` identifying any parameters
# which are exclusively used as `int`, but not explicitly specified as such
add_int_attrs <- function (x, int_val) {

    int_val <- int_val [int_val$fn == x$fn & int_val$int_val, , drop = FALSE]

    if (nrow (int_val) > 0) {
        for (p in int_val$par) {
            if (is.numeric (x$params [[p]]))
                attr (x$params [[p]], "is_int") <- TRUE
        }
    }

    return (x)
}
