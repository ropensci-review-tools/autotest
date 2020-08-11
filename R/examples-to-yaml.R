#' examples_to_yaml
#'
#' Convert examples for a specified package to an 'autotest' 'yaml' to use to
#' automatically test package
#'
#' @param package Name of package for which 'yaml' is to be generated.
#' @export
examples_to_yaml <- function (package = NULL) {
    if (!package %in% search ())
        suppressMessages (
                          library (package, character.only = TRUE)
        )
    h <- help (package = eval (substitute (package)), help_type = "text")
    # first info is description stuff;
    # second info is help files;
    # third info is vignettes
    fns <- h$info [[2]]
    fns <- gsub ("\\s.*", "", fns [which (!grepl ("^\\s", fns))])

    exs <- list ()
    for (i in seq (fns)) {
        exs [[length (exs) + 1]] <- get_fn_exs (package, fn = fns [i])
    }
    not_null <- vapply (exs, function (i) length (i) > 0, logical (1))
    exs <- exs [not_null]
}

get_fn_exs <- function (pkg, fn, rm_seed = TRUE, exclude_not_run = TRUE) {
    ex <- example (eval (substitute (fn)), package = pkg,
                   character.only = TRUE, give.lines = TRUE)
    if (length (ex) == 0)
        return (NULL)

    ex <- ex [-(1:grep ("^### \\*\\* Examples", ex))]
    if (ex [1] == "")
        ex <- ex [-1]

    if (exclude_not_run) {
        nr <- grep ("^## Not run:", ex)
        if (length (nr) > 0) {
            nr_end <- grep ("^## End\\(Not run\\)", ex)
            ex <- ex [-(nr:nr_end)]
        }
    }

    ex <- ex [!grepl ("^\\#", ex)]
    ex <- ex [ex != ""]

    # find all points of function calls:
    fns <- ls (paste0 ("package:", pkg))
    fn_calls <- do.call (c, lapply (fns, function (i) grep (i, ex)))
    fn_calls <- sort (unique (fn_calls))
    # reduce to only final calls in a sequence
    fn_calls <- fn_calls [-(which (c (2, diff (fn_calls)) == 1) - 1)]
    # remove any plot or summary calls
    index <- grepl ("^plot|^summary", ex [fn_calls])
    if (any (index))
        fn_calls <- fn_calls [-(which (index))]

    if (length (fn_calls) == 0)
        return (NULL)

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
one_ex_to_yaml <- function (pkg, fn, x, prev_fns = NULL) {

    i1 <- paste0 (rep (" ", 4), collapse = "")
    i2 <- paste0 (rep (" ", 8), collapse = "")
    i3 <- paste0 (rep (" ", 12), collapse = "")

    fns <- ls (paste0 ("package:", pkg))
    fn_calls <- do.call (c, lapply (fns, function (i) grep (i, x)))
    fn_calls <- sort (unique (fn_calls))

    yaml <- c (paste0 ("package: ", pkg),
               "functions:",
               paste0 (i1, "- ", fn, ":"))

    if (fn_calls [1] > 1) {
        yaml <- c (yaml,
                   paste0 (i2, "- preprocess:"))
        for (i in x [seq (fn_calls [1]) - 1])
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
    x <- x [fn_calls [1]:length (x)]
    ex <- regmatches (x, gregexpr("(?=\\().*?(?<=\\))", x, perl=T)) [[1]]
    ex <- strsplit (substr (ex, 2, nchar (ex) - 1), ",") [[1]]
    ex <- lapply (ex, function (i) {
                      if (!grepl ("=", i))
                          c (NA_character_, i)
                      else
                          strsplit (i, "=") [[1]]   })
    ex <- do.call (rbind, ex)

    # check whether any objects have been constructed in previous examples =
    # previous pre-processing steps:
    pp <- prev_preprocess (prev_fns, fn)
    po <- prev_objects (pp)
    if (any (po %in% ex [, 2])) {
        # add those pre-processing steps
        iend <- grep ("parameters:$", yaml) - 1
        if (any (grepl ("preprocess:$", yaml))) {
            istart <- grep ("preprocess:$", yaml)
            prepro <- yaml [istart:iend]
            yaml_top <- yaml [1:(istart - 1)]
        } else {
            prepro <- paste0 (i2, "- preprocess:")
            yaml_top <- yaml [1:iend]
        }
        yaml_bot <- yaml [(iend + 1):length (yaml)]

        # TODO: The following is not correct because it only grabs one line, but
        # there may be cases with multiple lines
        this_pp <- vapply (pp [[which (po %in% ex [, 2])]], function (i)
                           paste0 (i3, "- '", i, "'"), character (1),
                           USE.NAMES = FALSE)
        this_prepro <- c (prepro [1], this_pp)
        if (length (prepro) > 1)
            this_prepro <- c (this_prepro, prepro [2:length (prepro)])

        # stick those preprocessing lines in the yaml
        yaml <- c (yaml_top, this_prepro, yaml_bot)
    }

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

# Get preprocessing steps from previously constructed yaml representations of
# examples
prev_preprocess <- function (prev_fns, fn) {
    lapply (prev_fns, function (i)
            yaml::yaml.load (i)$functions [[1]] [[fn]] [[1]]$preprocess)
}

get_assign_position <- function (x) {
    index1 <- regexpr ("<-", x)
    index2 <- regexpr ("=", x)
    if (index1 < 0)
        index1 <- Inf
    if (index2 < 0)
        index2 <- Inf
   min (c (index1, index2))
}

# Get names of previous objects assigned by prev_preprocess steps
prev_objects <- function (prev_preprocesses) {
    vapply (prev_preprocesses, function (i) {
                ap <- get_assign_position (i)
                substring (i, 1, ap - 1)
            }, character (1))
}
