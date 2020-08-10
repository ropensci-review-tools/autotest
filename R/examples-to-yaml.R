#' examples_to_yaml
#'
#' Convert examples for a specified package to an 'autotest' 'yaml' to use to
#' automatically test package
#'
#' @param package Name of package for which 'yaml' is to be generated.
#' @export
examples_to_yaml <- function (package = NULL) {
}

get_fn_exs <- function (pkg, fn, rm_seed = TRUE) {
    ex <- example (eval (substitute (fn)), package = pkg,
                   character.only = TRUE, give.lines = TRUE)

    ex <- ex [-(1:grep ("^### \\*\\* Examples", ex))]
    if (ex [1] == "")
        ex <- ex [-1]

    # find all points of function calls:
    fns <- ls (paste0 ("package:", pkg))
    fn_calls <- do.call (c, lapply (fns, function (i) grep (i, ex)))
    fn_calls <- sort (unique (fn_calls))
    # reduce to only final calls in a sequence
    fn_calls <- fn_calls [-(which (c (2, diff (fn_calls)) == 1) - 1)]
    # remove any plot or summary calls
    fn_calls <- fn_calls [-grep ("^plot|^summary", ex [fn_calls])]

    index <- rep (seq (length (fn_calls)),
                  times = c (fn_calls [1], diff (fn_calls)))
    exs <- split (ex [seq (length (index))], f = as.factor (index))
    # rm extraneous lines
    ret <- lapply (exs, function (i) {
                       i <- i [which (!i == "")]
                       i [!grepl ("^\\#|^plot|^summary", i)] })
    
    if (rm_seed) {
        ret <- lapply (ret, function (i) {
                           i [!grepl ("^set.seed", i)]  })
    }

    return (ret)
}

# convert one example from get_fn_exs to yaml output
one_ex_to_yaml <- function (pkg, fn, x) {

    i1 <- paste0 (rep (" ", 4), collapse = "")
    i2 <- paste0 (rep (" ", 8), collapse = "")
    i3 <- paste0 (rep (" ", 12), collapse = "")

    yaml <- c (paste0 ("package: ", pkg),
               "functions:",
               paste0 (i1, "- ", fn, ":"))

    if (length (x) > 1) {
        yaml <- c (yaml,
                   paste0 (i2, "- preprocess:"))
        for (i in x [-length (x)])
            yaml <- c (yaml,
                       paste0 (i3, "- '", i, "'"))
    }
    yaml <- c (yaml,
               paste0 (i2, "- parameters:"))

    # get fn formals:
    pkg_env <- as.environment (paste0 ("package:", pkg))
    pars <- formals (fun = fn, envir = pkg_env)
    nms <- names (pars)

    # capture content between parentheses:
    xlast <- x [length (x)]
    ex <- regmatches (xlast, gregexpr("(?=\\().*?(?<=\\))", xlast, perl=T)) [[1]]
    ex <- strsplit (substr (ex, 2, nchar (ex) - 1), ",") [[1]]
    ex <- lapply (ex, function (i) {
                      if (!grepl ("=", i))
                          c (NA_character_, i)
                      else
                          strsplit (i, "=") [[1]]   })
    ex <- do.call (rbind, ex)

    # assign names to any unnamed parameters:
    if (any (is.na (ex [, 1]))) {
        other_nms <- nms [which (!nms %in% ex [, 1])]
        index <- which (is.na (ex [, 1]))
        ex [index, 1] <- other_nms [seq (index)]
    }

    # add to parameters list of yaml:
    for (i in seq (nrow (ex))) {
        yaml <- c (yaml,
                   paste0 (i3, "- ", ex [i, 1], ": ", ex [i, 2]))
    }

    return (yaml)
}
