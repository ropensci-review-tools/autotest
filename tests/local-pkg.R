
make_pkg <- function () {

    desc <- c ("Package: demo",
               "Title: What the Package Does (One Line, Title Case)",
               "Version: 0.0.0.9000",
               "Authors@R: ",
               "  person(given = \"First\",",
               "         family = \"Last\",",
               "         role = c(\"aut\", \"cre\"),",
               "         email = \"first.last@example.com\")",
               "Description: What the package does (one paragraph).",
               "Imports: methods",
               "License: GPL-3",
               "Encoding: UTF-8")

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

    rfile <- c ("#' test_rect",
                "#' A test retangular funtion",
                "#' @param x rectangular input",
                "#' @return return value",
                "#' @examples",
                "#' test_rect(iris)",
                "#' @export",
                "test_rect <- function(x = datasets::iris) {",
                "  ret <- x",
                "  if (methods::is (x, \"tbl_df\"))",
                "    ret <- x [-1, -ncol (x)]",
                "  else if (methods::is (x, \"data.table\"))",
                "    ret <- x [-(1:2), -\"Petal.Width\"]",
                "return (ret)  }")
    writeLines (rfile, con = file.path (dr, "test-rect.R"))

    rdfile <- c ("\\name{test}",
                 "\\alias{test}",
                 "\\title{test",
                 "A test funtion}",
                 "\\usage{test(x = 1)}",
                 "\\arguments{",
                 "\\item{x}{input}",
                 "}",
                 "\\value{return value}",
                 "\\description{test A test funtion}",
                 "\\examples{",
                 "test(1)",
                 "}")
    dm <- file.path (d, "man")
    if (!file.exists (dm))
        dir.create (dm)
    writeLines (rdfile, con = file.path (dm, "test.Rd"))

    rdfile <- c ("\\name{test_rect}",
                 "\\alias{test_rect}",
                 "\\title{test_rect",
                 "A test retangular funtion}",
                 "\\usage{test_rect(x = datasets::iris)}",
                 "\\arguments{",
                 "\\item{x}{rectangular input}",
                 "}",
                 "\\value{return value}",
                 "\\description{test_rect A test retangular funtion}",
                 "\\examples{",
                 "test_rect(datasets::iris)",
                 "}")
    writeLines (rdfile, con = file.path (dm, "test_rect.Rd"))

    nfile <- c ("export(test)",
                "export(test_rect)")
    writeLines (nfile, con = file.path (d, "NAMESPACE"))

    return (d)
}
