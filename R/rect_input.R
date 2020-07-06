
autotest_rectangular <- function (yaml = NULL, filename = NULL) {
    res <- parse_yaml_template (yaml = yaml, filename = filename)

    . <- NULL # suppress no visible binding note

    for (i in seq (res$datasets)) {
        this_fn <- names (res$datasets) [i]
             for (j in seq (res$datasets [[i]])) {
                 this_dat <- strsplit (res$datasets [[i]] [j], "::") [[1]]
                 epkg <- as.environment (paste0 ("package:", this_dat [1]))
                 dat_name <- this_dat [2]
                 e <- new.env ()
                 e$this_dat <- get (dat_name, envir = epkg)

                 for (p in res$preprocess [[i]] [[j]]) {
                     gsub ("`", "", p) %>%
                         gsub (dat_name, "this_dat", .) %>%
                         parse (text = .) %>%
                         eval (envir = e)
                 }

                 pars <- at_get_fn_params (fn_name = this_fn, pkg_name = res$package)

                 out <- do.call (this_fn, list (e$this_dat))
             }
    }
}
