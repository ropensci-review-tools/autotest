
get_param_types <- function (fn, params, par_lengths) {

    if (any (params == "NULL")) {
        params <- params [params != "NULL"]
    }

    single_index <- single_params (params)
    vec_index <- vector_params (params)
    rect_index <- tabular_params (params)

    param_types <- rep (NA_character_, length (params))
    param_types [vec_index] <- "vector"
    param_types [single_index] <- "single"
    param_types [rect_index] <- "tabular"

    # use par_lengths to set any parameters identified as single through usage
    # in present example to vector
    index <- which (par_lengths$fn == fn &
                    par_lengths$par %in% names (params))
    if (length (index) > 0) {
        par_lengths <- par_lengths [index, ]
        index <- match (names (params), par_lengths$par)
        is_vec <- !par_lengths$single
        param_types [is_vec] <- "vector"
    }

    return (param_types)
}

single_params <- function (params) {

        is_single <- function (j) {
            chk <- FALSE
            if (is.null (dim (j)) && length (j) == 1) {
                if (methods::is (j, "name")) {
                    val <- tryCatch (eval (parse (text = j)),
                                     error = function (e) NULL)
                    if (!is.null (val))
                        chk <- length (val) == 1
                } else if (!isS4 (j)) {
                    # single objects can still be almost anything, so only
                    # consider as truly single those objects which have
                    # attribute lists each element of which have at most two
                    # elements. This is entirely arbitrary, and should be
                    # modified once more is known about the kinds of things
                    # thrown at this function.
                    lens <- vapply (attributes (j), length, integer (1))
                    chk <- !any (lens > 2)
                }
            } else if (methods::is (j, "formula")) {
                chk <- TRUE
            }
            return (chk)
        }

    return (which (vapply (params, function (j)
                           is_single (j),
                           logical (1))))
}

vector_params <- function (params) {

    return (which (vapply (params, function (i)
                           length (i) > 1 &&
                               is.null (dim (i)) &&
                               is.atomic (i) &&
                               !class (i) %in% c ("call", "formula"),
                           logical (1))))
}

tabular_params <- function (params) {

    return (which (vapply (params, function (i)
                           length (dim (i)) == 2 &
                               !(inherits (i, "Matrix") |
                                 inherits (i, "matrix")),
                           logical (1))))
}

#' single_or_vec
#'
#' Do different usages within a single yaml indicate whether a parameter is
#' restricted to length one, or whether it can be a vector with length > 1?
#' @param res The parsed yaml returned from `parse_yaml_template`.
#' @noRd
single_or_vec <- function (res) {

    fns <- unique (names (res$parameters))

    pars <- lapply (fns, function (f) {

        pars <- res$parameters [names (res$parameters) == f]
        pars <- lapply (pars, function (i) {
                            nms <- names (i [[1]])
                            lens <- vapply (nms, function (j)
                                            length (i [[1]] [[j]]),
                                            integer (1))
                            data.frame (name = nms,
                                        len = lens) })

        pars <- data.frame (do.call (rbind, unname (pars)))
        pars <- lapply (split (pars, f = as.factor (pars$name)),
                        function (i)
                            i [which.max (i$len), , drop = FALSE])

        pars <- do.call (rbind, pars)

        data.frame (fn = f,
                    par = pars$name,
                    single = pars$len == 1,
                    stringsAsFactors = FALSE)
                     })

    return (do.call (rbind, pars))
}
