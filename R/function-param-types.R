
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
    index <- which (par_lengths$par %in% names (params) & !par_lengths$single)
    if (length (index) > 0) {
        par_lengths <- par_lengths [index, , drop = FALSE]
        param_types [match (par_lengths$par, names (params))] <- "vector"
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
                               length (class (i) <= 1L) &&
                               any (grepl (atomic_modes (collapse = TRUE),
                                           class (i))),
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

    pkg_namespace <- paste0 ("package:", res$package)
    pkg_env <- new.env (parent = as.environment (pkg_namespace))

    pars <- lapply (fns, function (f) {

        pars <- res$parameters [names (res$parameters) == f]
        pars <- lapply (pars, function (i) {
                    nms <- names (unlist (i))
                    lens <- vapply (nms, function (j) {
                                ij <- unlist (i) [[j]]
                                out <- length (ij)
                                if (methods::is (ij, "name")) {
                                    tmp <- tryCatch (
                                            eval (parse (text = ij),
                                                  envir = pkg_env),
                                            error = function (e) NULL)
                                    if (!is.null (tmp))
                                        out <- length (tmp)
                                }
                                return (out)
                            },
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

#' double_or_int
#'
#' Do different usages within a single yaml indicate whether a single-length
#' parameter is intended to be an integer, yet without `L`, or whether it is
#' indeed a double?
#' @param res The parsed yaml returned from `parse_yaml_template`.
#' @noRd
double_or_int <- function (res) {

    fns <- unique (names (res$parameters))

    is_par_int <- function (p) {
        ret <- FALSE
        if (is.numeric (p))
            ret <- all (abs (p - round (p)) < .Machine$double.eps)
        if (!is.null (attr (p, "is_int")))
            if (!attr (p, "is_int"))
                ret <- FALSE
        return (ret)
    }

    pars <- lapply (fns, function (f) {

        pars <- res$parameters [names (res$parameters) == f] [[1]]
        nms <- vapply (pars, names, character (1))
        pars <- lapply (pars, function (i) i [[1]])
        names (pars) <- nms

        pars <- lapply (seq_along (pars), function (i) {
                            nms <- names (pars) [i]
                            int_val <- is_par_int (pars [[i]])
                            data.frame (name = nms,
                                        int_val = int_val)
                    })

        pars <- data.frame (do.call (rbind, unname (pars)))
        pars <- lapply (split (pars, f = as.factor (pars$name)),
                        function (i) {
                            int_val <- all (i$int_val)
                            i <- i [1, ]
                            i$int_val <- int_val
                            return (i)
                        })

        pars <- do.call (rbind, pars)

        data.frame (fn = f,
                    par = pars$name,
                    int_val = pars$int_val,
                    stringsAsFactors = FALSE)
                     })

    return (do.call (rbind, pars))
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
