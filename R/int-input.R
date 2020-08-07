# Test int inputs to functions to determine accepted range of inputs.
get_int_range <- function (this_fn, params, i) {

    # Return 0 if fn call returns NULL
    #        1 if fn call errors
    #        2 if fn call warns
    #        3 if fn call silently returns
    get_fn_response <- function (this_fn, params) {
        val <- tryCatch (
                        do.call (this_fn, params),
                        error = function (e) "error",
                        warning = function (w) "warning")
        if (is.null (val))
            val <- 0
        else if (is.character (val) & length (val) == 1) {
            if (val == "error")
                val <- 1
            else if (val == "warning")
                val <- 2
        } else
            val <- 3
        return (val)
    }

    if (get_fn_response (this_fn, params) != 3)
    {
        warning ("Function [", this_fn, "] does not respond appropriately for ",
              "specified/default input [", names (params) [i], " = ", params [[i]], "]")
        return (NULL)
    }

    # log-space search by 'step_factor':
    stepdown <- function (this_fn, params, i, maxval, step_factor = 10) {
        val <- maxval
        while (val == maxval && abs (params [[i]]) > 1) {
            params [[i]] <- ceiling (params [[i]] / step_factor)
            val <- get_fn_response (this_fn, params)
        }
        return (params [[i]])
    }

    params [[i]] <- .Machine$integer.max
    maxval <- get_fn_response (this_fn, params)
    if (maxval == 3) {
        p_i_max <- params [[i]]
    } else {
        p_i <- stepdown (this_fn, params, i, maxval, step_factor = 10)
        # then step back up factor and 10 and zoom in
        params [[i]] <- floor (p_i * 10)
        maxval <- get_fn_response (this_fn, params)
        p_i_max <- stepdown (this_fn, params, i, maxval, step_factor = 2)
    }

    params [[i]] <- -.Machine$integer.max
    maxval <- get_fn_response (this_fn, params)
    if (maxval == 3) {
        p_i <- params [[i]]
    } else {
        p_i <- stepdown (this_fn, params, i, maxval, step_factor = 10)

        fn_resp_is_3 <- function (this_fn, params, i, val) {
            params [[i]] <- val
            return (get_fn_response (this_fn, params) == 3)
        }

        if (p_i == 0) {
            if (fn_resp_is_3 (this_fn, params, i, 0))
                p_i <- 0
            else if (fn_resp_is_3 (this_fn, params, i, 1))
                p_i <- 1
        } else { # p_i must be < 0
            params [[i]] <- -floor (p_i * 10)
            maxval <- get_fn_response (this_fn, params)
            p_i <- stepdown (this_fn, params, i, maxval, step_factor = 2)
        }
    }

    return (c (p_i, p_i_max))
}
