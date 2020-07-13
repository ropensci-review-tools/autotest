
# extracts the i-th list of complete parameters from the result of a parsed yaml
get_params <- function (res, i, this_fn) {
    p <- unlist (res$parameters [[i]])
    p_keys <- names (p)
    p_vals <- unname (p)

    . <- NULL # suppress no visible binding note
    pre <- res$preprocess [[i]]
    e <- new.env ()
    for (p in pre) {
        gsub ("`", "", p) %>%
            parse (text = .) %>%
            eval (envir = e)
    }

    params <- list ()
    for (p in seq (p_keys)) {
        this_val <- p_vals [p]
        if (grepl ("::", this_val)) {
            this_pkg <- strsplit (p_vals [p], "::") [[1]] [1]
            if (!this_pkg %in% search ())
                suppressMessages (
                    library (this_pkg, character.only = TRUE)
                    )
            this_val <- parse (text = this_val) %>%
                eval (envir = as.environment (paste0 ("package:", this_pkg)))
        } else if (this_val %in% names (e))
            this_val <- parse (text = this_val) %>%
                eval (envir = e)

            params [[length (params) + 1]] <- this_val
            names (params) [length (params)] <- p_keys [p]
    }

    # Parse fn definition to get list of all parameters:
    #pars <- at_get_fn_params (fn_name = this_fn, pkg_name = res$package)
    if (!res$package %in% search ())
        suppressMessages (
            library (res$package, character.only = TRUE)
            )
    pkg_env <- as.environment (paste0 ("package:", res$package))
    pars <- formals (fun = this_fn, envir = pkg_env)
    nms <- names (pars)

    # If fn includes ... AND any submitted params are not named, then remove the
    # ... from returned list
    if ("..." %in% nms & any (!names (params) %in% nms)) {
        pars <- pars [which (!nms == "...")]
        nms <- nms [which (!nms == "...")]
    }

    # parameters in formals with no default values are returned as empty
    # 'symbol' expressions - this converts these to "MISSING":
    pars <- lapply (pars, function (i) {
                if (typeof (i) == "symbol" & deparse (i) == "")
                    return ("MISSING")
                else if (is.null (i))
                    return ("NULL")
                else
                    return (i)
            })
    # That can then be used to check that any with non-default values have been
    # provided:
    if (any (pars == "MISSING")) {
        no_defaults <- nms [which (pars == "MISSING")]
        no_defaults <- no_defaults [which (!no_defaults %in% names (params))]
        if (length (no_defaults) > 0)
            stop ("function includes the following parameters which require ",
                  "non-default values:\n   [", paste0 (no_defaults, collapse = ", "),
                  "]")
        # The rest must be in params, so the default "MISSING" entries can be
        # removed here:
        pars <- pars [which (pars != "MISSING")]
    }

    # Add all resultant params from fn definition yet not directly specified to
    # the return list.
    for (p in seq (pars)) {
        params [[length (params) + 1]] <- pars [[p]]
        names (params) [length (params)] <- names (pars) [p]
    }

    return (params)
}