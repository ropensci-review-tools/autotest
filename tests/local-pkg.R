
make_pkg_path <- function () {
    d <- file.path (tempdir (), "demo")
    if (!file.exists (d))
        dir.create (d)

    return (d)
}

make_desc <- function (d) {
    desc <- c ("Package: demo",
               "Title: What the Package Does (One Line, Title Case)",
               "Version: 0.0.0.9000",
               "Authors@R: ",
               "  person(given = \"First\",",
               "         family = \"Last\",",
               "         role = c(\"aut\", \"cre\"),",
               "         email = \"first.last@example.com\")",
               "Description: What the package does (one paragraph).",
               "Imports:",
               "  data.table,",
               "  methods",
               "License: GPL-3",
               "Encoding: UTF-8")

    writeLines (desc, con = file.path (d, "DESCRIPTION"))
}

make_test_int <- function (d) {

    rfile <- c ("#' test_int",
                "#' An integer test funtion",
                "#' @param x integer input",
                "#' @return return value",
                "#' @examples",
                "#' test_int(1)",
                "#' @export",
                "test_int <- function(x = 1) {",
                "  if (x > 1e3)",
                "    stop (\"upper limit\")",
                "  x ^ 2 }")
    dr <- file.path (d, "R")
    if (!file.exists (dr))
        dir.create (dr)
    writeLines (rfile, con = file.path (dr, "test.R"))

    rdfile <- c ("\\name{test_int}",
                 "\\alias{test_int}",
                 "\\title{test_int",
                 "An integer test funtion}",
                 "\\usage{test_int(x = 1)}",
                 "\\arguments{",
                 "\\item{x}{integer input}",
                 "}",
                 "\\value{return value}",
                 "\\description{test An integer test funtion}",
                 "\\examples{",
                 "test_int(1)",
                 "}")
    dm <- file.path (d, "man")
    if (!file.exists (dm))
        dir.create (dm)
    writeLines (rdfile, con = file.path (dm, "test_int.Rd"))
}

make_test_rect <- function (d) {

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
                "  else if (methods::is (x, \"data.table\")) {",
                "    nm = names (x) [ncol (x)]",
                "    ret <- x [, (nm):=NULL]",
                "    ret <- ret [-(1:2),]}",
                "return (ret)  }")

    dr <- file.path (d, "R")
    if (!file.exists (dr))
        dir.create (dr)
    writeLines (rfile, con = file.path (dr, "test-rect.R"))

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

    dm <- file.path (d, "man")
    if (!file.exists (dm))
        dir.create (dm)
    writeLines (rdfile, con = file.path (dm, "test_rect.Rd"))
}

make_namespace <- function (d) {

    nfile <- c ("importFrom(data.table,`:=`)",
                "export(test_int)",
                "export(test_rect)")
    writeLines (nfile, con = file.path (d, "NAMESPACE"))
}

make_pkg <- function () {

    d <- make_pkg_path ()
    make_desc (d)
    make_test_int (d)
    make_test_rect (d)
    make_namespace (d)

    return (d)
}
