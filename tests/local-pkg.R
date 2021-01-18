
make_pkg <- function () {

    desc <- c ('Package: demo',
               'Title: What the Package Does (One Line, Title Case)',
               'Version: 0.0.0.9000',
               'Authors@R: ',
               '  person(given = "First",',
               '         family = "Last",',
               '         role = c("aut", "cre"),',
               '         email = "first.last@example.com")',
               'Description: What the package does (one paragraph).',
               'License: GPL-3',
               'Encoding: UTF-8')

    d <- file.path (tempdir (), "demo")
    if (!file.exists (d))
        dir.create (d)
    writeLines (desc, con = file.path (d, "DESCRIPTION"))

    rfile <- c ("#' test",
                "#' A test funtion",
                "#' @param x input",
                "#' @return return value",
                "#' @examples",
                "#' test(1)",
                "#' @export",
                "test <- function(x = 1) {",
                "  x ^ 2 }")
    dr <- file.path (d, "R")
    if (!file.exists (dr))
        dir.create (dr)
    writeLines (rfile, con = file.path (dr, "test.R"))

    rdfile <- c ("\\name{test}",
                 "\\alias{test}",
                 "\\title{test,A test funtion}",
                 "\\usage{test(x = 1)}",
                 "\\arguments{\\item{x}{input}}",
                 "\\value{return value}",
                 "\\description{test A test funtion}",
                 "\\examples{",
                 "test(1)",
                 "}")
    dm <- file.path (d, "man")
    if (!file.exists (dm))
        dir.create (dm)
    writeLines (rdfile, con = file.path (dm, "test.Rd"))

    nfile <- "export(test)"
    writeLines (nfile, con = file.path (d, "NAMESPACE"))

    return (d)
}
