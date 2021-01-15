
#' report_fns_wo_examples
#'
#' Get functions which do not have examples and return as an autotest object.
#' @noRd
report_fns_wo_example <- function (package, res) {

    no_examples <- fns_without_examples (package)
    no_examples <- no_examples [no_examples %in% unique (res$fn_name)]
    if (length (no_examples) > 0) {
        cnt <- "This function has no documented example"
        for (i in no_examples) {
            rtemp <- report_object (type = "warning",
                                    fn_name = i,
                                    operation = "<see content>",
                                    content = cnt)
            rtemp$yaml_hash <- NA_character_
            res <- rbind (res, rtemp)

        }
    }

    return (res)
}

#' untested_params
#'
#' Takes a full list of examples for a single package and identifies any
#' parameters not able to be tested from example code. This requires the full
#' list of all examples, because it's not possible to determine from any one
#' example whether or not a parameter might be tested in some other example.
#' @noRd
untested_params <- function (exs) {

    pars <- list ()

    for (i in exs) {

        package <- gsub ("package:\\s?|\\s?$", "",
                         grep ("^package:", i, value = TRUE))
        l1 <- i [grep ("^functions:", i) [1] + 1]
        sp <- gregexpr ("\\s", l1) [[1]]
        nspaces <- sp [which (diff (sp) > 1) [1]]
        ptn <- paste0 ("^[[:space:]]{", nspaces, "}-")
        index <- grep (ptn, i)

        these_fns <- gsub ("\\s+-\\s?|:$", "", i [index])

        index <- cbind (index, c (index [-1] - 1, length (i)))
        these_pars <- apply (index, 1, function (j) {
                         txt <- i [j [1]:j [2]]
                         ln <- grep ("\\s+- parameters:", txt)
                         p <- vapply (txt [- (1:ln)], function (k) {
                                          res <- gsub ("\\s+-\\s?", "", k)
                                          strsplit (res, ":") [[1]] [1] },
                                          character (1),
                                          USE.NAMES = FALSE)
                         return (p) })
        if (methods::is (these_pars, "matrix")) {
            these_pars <- apply (these_pars, 2, function (j)
                                 list (as.vector (j)))
            these_pars <- lapply (these_pars, unlist)
        }
        names (these_pars) <- these_fns

        pars <- c (pars, these_pars)
    }

    fns <- unique (names (pars))
    pars_f <- lapply (fns, function (f) {
                          pars_f <- pars [which (names (pars) == f)]
                          unique (unlist (pars_f))    })
    names (pars_f) <- fns

    this_env <- as.environment (paste0 ("package:", package))
    fmls <- lapply (fns, function (f)
                    names (formals (f, env = this_env)))
    fmls <- lapply (seq_along (fmls), function (i) {
                        index <- which (!fmls [[i]] %in% pars_f [[i]] &
                                        !fmls [[i]] == "...")
                        fmls [[i]] [index]  })
    names (fmls) <- fns

    fmls <- fmls [which (vapply (fmls, length, integer (1)) > 0)]

    return (fmls)
}

test_untested_params <- function (exs = NULL, ...) {
    UseMethod ("test_untested_params", exs)
}

test_untested_params.NULL <- function (exs = NULL, ...) {

    report_object (type = "dummy",
                   test_name = "par_is_demonstrated",
                   content = paste0 ("Examples do not demonstrate ",
                                     "usage of this parameter"),
                   operation = "check parameter usage is demonstrated")
}

test_untested_params.list <- function (exs = NULL, res_in = NULL, ...) {

    pars <- untested_params (exs)
    pars <- pars [which (vapply (pars, length, integer (1)) > 0)]

    res <- lapply (seq_along (pars), function (i) {
                       ro <- test_untested_params.NULL ()
                       ro <- ro [rep (1, length (pars [[i]])), ]
                       ro$type <- "warning"
                       ro$fn_name <- names (pars) [i]
                       ro$parameter <- pars [[i]]
                       ro$yaml_hash <- NA_character_
                       return (ro)  })

    res <- do.call (rbind, res)

    return (rbind (res_in, res))
}
