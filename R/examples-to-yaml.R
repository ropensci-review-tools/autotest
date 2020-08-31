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

    exs <- get_all_examples (package)
}

get_all_examples <- function (package) {

    h <- utils::help (package = eval (substitute (package)), help_type = "text")
    # first info is description stuff;
    # second info is help files;
    # third info is vignettes
    fns <- h$info [[2]]
    fns <- gsub ("\\s.*", "", fns [which (!grepl ("^\\s", fns))])
    # reduce only to exported functions, methods, or data sets
    fns <- fns [fns %in% ls (paste0 ("package:", package))]
    # Then reduce only to functions:
    fn_classes <- vapply (fns, function (i) class (get (i)), character (1))
    fns <- fns [which (fn_classes == "function")]

    exs <- list ()
    for (i in seq (fns)) {
        fn <- fns [i]
        exi <- get_fn_exs (package, fn)
        if (!is.null (exi)) {
            exs [[length (exs) + 1]] <- exi
            names (exs) [length (exs)] <- fns [i]
        }
    }

    not_null <- vapply (exs, function (i) length (i) > 0, logical (1))
    ret <- exs [not_null]

    # remove any enclosing brackets from any example lines
    ret <- lapply (ret, function (i)
                   gsub ("\\)$", "", gsub ("^\\(", "", i)))

    return (ret)
}

get_fn_exs <- function (pkg, fn, rm_seed = TRUE, exclude_not_run = TRUE) {
    ex <- utils::example (eval (substitute (fn)), package = pkg,
                          character.only = TRUE, give.lines = TRUE)
    if (length (ex) == 0)
        return (NULL)

    ex <- ex [-(1:grep ("^### \\*\\* Examples", ex))]
    if (ex [1] == "")
        ex <- ex [-1]

    if (exclude_not_run) {
        nr <- grep ("^## Not run:", ex)
        while (length (nr) > 0) {
            nr_end <- grep ("^## End\\(Not run\\)", ex)
            ex <- ex [-(nr [1]:nr_end [1])]
            nr <- grep ("^## Not run:", ex)
        }
    }

    ex <- ex [!grepl ("^\\#", ex)]
    ex <- ex [ex != ""]

    ex <- match_brackets (ex)
    ex <- merge_piped_lines (ex)

    # find all points of function calls:
    fns <- ls (paste0 ("package:", pkg))
    fn_calls <- do.call (c, lapply (fns, function (i) grep (i, ex)))
    fn_calls <- sort (unique (fn_calls))
    # reduce to only final calls in a sequence
    index <- which (c (2, diff (fn_calls)) == 1)
    if (length (index) > 0)
        fn_calls <- fn_calls [-(index - 1)]
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

    # rm any example items which do not call the actual function
    index <- vapply (ret, function (i) any (grepl (fn, i)), logical (1))

    return (ret [index])
}

# convert one example from get_fn_exs to yaml output
one_ex_to_yaml <- function (pkg, fn, x, prev_fns = NULL) {

    i1 <- paste0 (rep (" ", 4), collapse = "")
    i2 <- paste0 (rep (" ", 8), collapse = "")
    i3 <- paste0 (rep (" ", 12), collapse = "")


    yaml <- c (paste0 ("package: ", pkg),
               "functions:",
               paste0 (i1, "- ", fn, ":"))

    fn_calls <- grep (fn, x)
    # rm all lines after final fn_calls
    x <- x [1:max (fn_calls)]

    if (fn_calls [1] > 1) {
        yaml <- c (yaml,
                   paste0 (i2, "- preprocess:"))
        for (i in x [seq (fn_calls [1]) - 1])
            yaml <- c (yaml,
                       paste0 (i3, "- '", i, "'"))
    }

    # get fn formals:
    pkg_env <- as.environment (paste0 ("package:", pkg))
    pars <- formals (fun = fn, envir = pkg_env)
    nms <- names (pars)

    # capture content between parentheses:
    x <- x [fn_calls [1]:length (x)]
    # remove comments at end of lines:
    x <- gsub ("\\s$", "", gsub ("\\#.*", "", x))

    ex <- regmatches (x, gregexpr("(?=\\().*?(?<=\\))$", x, perl=T))
    ex <- lapply (ex, function (i) strsplit (substr (i, 2, nchar (i) - 1), ",") [[1]])
    ex <- lapply (ex, function (i) {
                      res <- lapply (i, function (j) {
                                         if (!grepl ("=", j))
                                             c (NA_character_, j)
                                         else
                                             strsplit (j, "=") [[1]]   })
                      do.call (rbind, res)  })

    # check whether any objects have been constructed in previous examples =
    # previous pre-processing steps:
    pp <- prev_preprocess (prev_fns, fn)
    po <- prev_objects (pp)
    for (i in seq (ex)) {
        if (any (po %in% ex [[i]] [, 2])) {
            # add those pre-processing steps
            #iend <- grep ("parameters:$", yaml) - 1
            iend <- length (yaml)
            if (any (grepl ("preprocess:$", yaml))) {
                istart <- grep ("preprocess:$", yaml)
                prepro <- yaml [istart:iend]
                yaml_top <- yaml [1:(istart - 1)]
            } else {
                prepro <- paste0 (i2, "- preprocess:")
                yaml_top <- yaml [1:iend]
            }

            # TODO: The following is not correct because it only grabs one line, but
            # there may be cases with multiple lines
            this_pp <- vapply (pp [[which (po %in% ex [, 2])]], function (i)
                               paste0 (i3, "- '", i, "'"), character (1),
                               USE.NAMES = FALSE)
            this_prepro <- c (prepro [1], this_pp)
            if (length (prepro) > 1)
                this_prepro <- c (this_prepro, prepro [2:length (prepro)])

            # stick those preprocessing lines in the yaml
            yaml <- c (yaml_top, this_prepro)
        }
    }

    # assign names to any unnamed parameters:
    for (i in seq (ex)) {
        if (any (is.na (ex [[i]] [, 1]))) {
            other_nms <- nms [which (!nms %in% ex [[i]] [, 1])]
            index <- which (is.na (ex [[i]] [, 1]))
            ex [[i]] [index, 1] <- other_nms [seq (index)]
        }
    }

    # add to parameters list of yaml, duplicating fn name and preprocessing
    # stages each time:
    fn_start <- grep (fn, yaml)
    pre <- yaml [fn_start:length (yaml)]
    yaml <- yaml [1:(fn_start - 1)]
    for (i in seq (ex)) {
        yaml <- c (yaml,
                   pre,
                   paste0 (i2, "- parameters:"))
        for (j in seq (nrow (ex [[i]]))) {
            yaml <- c (yaml,
                       paste0 (i3, "- ", ex [[i]] [j, 1], ": ", ex [[i]] [j, 2]))
        }
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

# merge multi-line expressions to single line:
match_brackets <- function (x) {
    # get rid of any comments:
    x <- vapply (x, function (i) gsub ("\\#.*", "", i),
                 character (1),
                 USE.NAMES = FALSE)

    br_open <- lapply (gregexpr ("\\(", x), function (i) as.integer (i))
    br_closed <- lapply (gregexpr ("\\)", x), function (i) as.integer (i))
    br_both <- lapply (gregexpr ("\\((.+)?\\)", x), function (i) as.integer (i))
    for (i in seq (x)) {
        # the above `gregexpr` return -1 for no match, and these need to be
        # removed
        br_open [[i]] <- br_open [[i]] [which (!br_open [[i]] <= 0)]
        br_closed [[i]] <- br_closed [[i]] [which (!br_closed [[i]] <= 0)]
        br_both [[i]] <- br_both [[i]] [which (!br_both [[i]] <= 0)]
        # Then remove all instances of matched brackets on one line
        while (any (br_both [[i]] > 0)) {
            index <- which (br_open [[i]] == br_both [[i]])
            index2 <- which (br_closed [[i]] > br_open [[i]] [index]) [1]
            br_closed [[i]] <- br_closed [[i]] [-index2]
            br_open [[i]] <- br_open [[i]] [-index]
            br_both [[i]] <- br_both [[i]] [-1]

            if (length (br_open [[i]]) > 0 & length (br_closed [[i]]) > 0)
                br_both [[i]] <- br_open [[i]] [1]
        }
    }
    # convert to sequences of line numbers where brackets close, noting that
    # there may be multiple matched closing brackets on one line, hence the
    # `length` function here. There may also be values of -1 from the initial
    # `gregexpr` above; these need to be ignored here
    br_open2 <- br_closed2 <- NULL
    for (i in seq (br_open))
        br_open2 <- c (br_open2, rep (i, length (br_open [[i]])))
    for (i in seq (br_closed))
        br_closed2 <- c (br_closed2, rep (i, length (br_closed [[i]])))

    br_open <- rev (br_open2)
    br_closed <- rev (br_closed2)
    for (i in seq_along (br_open)) {
        x <- c (x [seq (br_open [i] - 1)],
                paste0 (x [br_open [i]:br_closed [i]], collapse = " "),
                x [(br_closed [i] + 1):length (x)])
    }
    
    gsub ("\\s+", " ", x)
}

merge_piped_lines <- function (x) {
    x <- gsub ("\\s$", "", x)
    index <- rev (grep ("%>%$", x))
    for (i in index) {
        x [i] <- gsub ("\\s+", " ", paste0 (x [i:(i+1)], collapse = " "))
        x <- x [-(i + 1)]
    }
    return (x)
}
