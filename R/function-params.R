
#' at_get_fn_params
#'
#' Get parameters of a specified function
#' @param fn_name The function for which parameters are to be extracted, either
#' as a simply function name along with 'package' parameter, or in double-colon
#' form as 'fn_name::pkg_name', in which case the 'pkg_name' parameter need not
#' be provided. 
#' @param pkg_name The package defining the named function.
#' @export
at_get_fn_params <- function (fn_name = NULL, pkg_name = NULL) {
    if (is.null (fn_name))
        stop ("function must be specified")
    else if (is.null (pkg_name)) {
        if (grepl ("::", fn_name)) {
            x <- strsplit (fn_name, "::") [[1]]
            pkg_name <- x [1]
            fn_name <- x [2]
        } else
            stop ("If pkg_name not specified, fn_name must be in double-colon format, ",
                  "'fn_name::pkg_name'")
    }

    if (!pkg_name %in% search ())
        suppressMessages (
            library (pkg_name, character.only = TRUE)
            )

    e <- as.environment (paste0 ("package:", pkg_name))
    . <- NULL # suppress no visible binding note
    x <- get (fn_name, envir = e) %>%
        deparse () %>%
        parse (text = ., keep.source = TRUE) %>%
        utils::getParseData ()
    n <- which (x$token == "'{'") [1]

    i1 <- which (x$token == "'('") [1]
    i2 <- which (x$token == "')'") [1]
    xhead <- x [(i1 + 1):(i2 - 1), ]
    xfn <- split (xhead, cumsum (xhead$token == "','"))
    xfn <- lapply (xfn, function (i)
                   i [!i$token %in% c ("','", "expr"), ])
    xfn <- lapply (xfn, function (i) {
                   sname <- i$text [i$token == "SYMBOL_FORMALS"]
                   sval <- NA
                   if ("EQ_FORMALS" %in% i$token) {
                       i2 <- split (i, cumsum (i$token == "EQ_FORMALS")) [[2]] [2, ]
                       if (nrow (i2) > 2)
                           stop ("whooops")
                       sval <- i2$text
                       if (i2$token == "NUM_CONST") {
                           if (regexpr ("L", sval) > 0)
                               sval <- as.integer (gsub ("L", "", sval))
                           else if (sval %in% c ("TRUE", "FALSE"))
                               sval <- eval (parse (text = sval, keep.source = TRUE))
                           else
                               sval <- as.numeric (sval)
                       }

                   }
                   list (name = sname, val = sval)
                   })
    return (xfn)
}
