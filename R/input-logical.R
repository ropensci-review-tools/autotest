
test_single_logical <- function (pkg, this_fn, params, i) {

    chk <- TRUE

    get1 <- function (val) {
        params [[i]] <- val
        tryCatch (
                  utils::capture.output (do.call (this_fn, params)),
                  error = function (e) e,
                  warning = function (w) w)
    }
    val0 <- get1 (params [[i]])
    val1 <- get1 (!params [[i]])

    if (any (methods::is (val0, "error")) | methods::is (val0, "warning") |
        any (methods::is (val1, "error")) | methods::is (val1, "warning")) {

        msg <- paste0 ("Function [", this_fn, "] does not respond appropriately ",
                       "for specified/default input [", names (params) [i],
                       " = ", params [[i]], "]\n")

        this_val <- params [[i]]
        msg_out <- val0
        if (any (methods::is (val1, "error")) | any (methods::is (val1, "warning"))) {
            this_val <- !params [[i]]
            msg_out <- val1
        }

        this_type <- "an error"
        if (any (methods::is (val0, "warning")) | any (methods::is (val1, "warning")))
            this_type <- "a warning"

        msg <- paste0 (msg, "  value of [", names (params) [i], " = ",
                       this_val, "] generates ", this_type, "\n ",
                       msg_out)

        warning (msg)

        return (FALSE)
    }

    val0 <- get1 (0L)
    val1 <- get1 (1L)
    val2 <- get1 (2L)
    if (!(any (methods::is (val2, "error")) | methods::is (val2, "warning"))) {
        message ("Parameter ", names (params) [i], " of function [",
                 this_fn, "] is assumed to be logical, but responds to ",
                 "general integer values.")
        # chk remains TRUE
    }

    vala <- get1 ("a")
    if (!(any (methods::is (vala, "error")) | methods::is (vala, "warning"))) {
        message ("Parameter ", names (params) [i], " of function [",
                 this_fn, "] is assumed to be logical, but responds to ",
                 "character input")
        chk <- FALSE
    }

    val_len <- get1 (c (TRUE, FALSE))
    if (!(any (methods::is (val_len, "error")) | methods::is (val_len, "warning"))) {
        message ("Parameter ", names (params) [i], " of function [",
                 this_fn, "] is assumed to be logical of length 1, ",
                 "but responds to vectors of length > 1")
        chk <- FALSE
    }

    return (chk)
}
