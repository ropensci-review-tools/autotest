make_pkg_path <- function () {
    d <- file.path (tempdir (), "demo")
    if (!file.exists (d)) {
        dir.create (d)
    }

    return (d)
}

make_desc <- function (d) {
    desc <- c (
        "Package: demo",
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
        "Encoding: UTF-8"
    )

    writeLines (desc, con = file.path (d, "DESCRIPTION"))
}

make_test_int <- function (d) {

    rfile <- c (
        "#' test_int",
        "#' An integer test funtion",
        "#' @param x integer input",
        "#' @return return value",
        "#' @examples",
        "#' test_int(1)",
        "#' @export",
        "test_int <- function(x = 1) {",
        "  if (x > 1e3)",
        "    stop (\"upper limit\")",
        "  x ^ 2 }"
    )
    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }
    writeLines (rfile, con = file.path (dr, "test.R"))

    rdfile <- c (
        "\\name{test_int}",
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
        "}"
    )
    dm <- file.path (d, "man")
    if (!file.exists (dm)) {
        dir.create (dm)
    }
    writeLines (rdfile, con = file.path (dm, "test_int.Rd"))

    rfile2 <- c (
        "#' test_int_range",
        "#' An integer test funtion with a documented range",
        "#' @param x integer input, must be greater than 0 and less than 100",
        "#' @return return value",
        "#' @examples",
        "#' test_int_range(50L)",
        "#' @export",
        "test_int_range <- function(x = 50L) {",
        "  x }"
    )
    writeLines (rfile2, con = file.path (dr, "test-int-range.R"))

    rdfile2 <- c (
        "\\name{test_int_range}",
        "\\alias{test_int_range}",
        "\\title{test_int_range",
        "An integer test funtion with a documented range}",
        "\\usage{test_int_range(x = 50L)}",
        "\\arguments{",
        "\\item{x}{integer input, must be greater than 0 and less than 100}",
        "}",
        "\\value{return value}",
        "\\description{test_int_range An integer test funtion with a documented range}",
        "\\examples{",
        "test_int_range(50L)",
        "}"
    )
    writeLines (rdfile2, con = file.path (dm, "test_int_range.Rd"))

    rfile3 <- c (
        "#' test_int_negrange",
        "#' An integer test funtion restricted to negative values",
        "#' @param x must be a negative integer",
        "#' @return return value",
        "#' @examples",
        "#' test_int_negrange(-5L)",
        "#' @export",
        "test_int_negrange <- function(x = -5L) {",
        "  if (x > 0)",
        "    stop (\"must be negative\")",
        "  x }"
    )
    writeLines (rfile3, con = file.path (dr, "test-int-negrange.R"))

    rdfile3 <- c (
        "\\name{test_int_negrange}",
        "\\alias{test_int_negrange}",
        "\\title{test_int_negrange",
        "An integer test funtion restricted to negative values}",
        "\\usage{test_int_negrange(x = -5L)}",
        "\\arguments{",
        "\\item{x}{must be a negative integer}",
        "}",
        "\\value{return value}",
        "\\description{test_int_negrange An integer test funtion restricted to negative values}",
        "\\examples{",
        "test_int_negrange(-5L)",
        "}"
    )
    writeLines (rdfile3, con = file.path (dm, "test_int_negrange.Rd"))
}

make_test_rect <- function (d) {

    rfile <- c (
        "#' test_rect",
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
        "return (ret)  }"
    )

    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }
    writeLines (rfile, con = file.path (dr, "test-rect.R"))

    rdfile <- c (
        "\\name{test_rect}",
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
        "}"
    )

    dm <- file.path (d, "man")
    if (!file.exists (dm)) {
        dir.create (dm)
    }
    writeLines (rdfile, con = file.path (dm, "test_rect.Rd"))
}

make_test_name <- function (d) {

    rfile <- c (
        "#' test_name",
        "#' A name test funtion",
        "#' @param nm name input",
        "#' @return return value",
        "#' @examples",
        "#' test_name(as.name(\"y\"))",
        "#' @export",
        "test_name <- function(nm = as.name(\"x\")) {",
        "  if (!is.name (nm))",
        "    stop (\"nm must be a name\")",
        "  deparse (nm) }"
    )
    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }
    writeLines (rfile, con = file.path (dr, "test-name.R"))

    rdfile <- c (
        "\\name{test_name}",
        "\\alias{test_name}",
        "\\title{test_name",
        "A name test funtion}",
        "\\usage{test_name(nm = as.name(\"x\"))}",
        "\\arguments{",
        "\\item{nm}{name input}",
        "}",
        "\\value{return value}",
        "\\description{test_name A name test funtion}",
        "\\examples{",
        "test_name(as.name(\"y\"))",
        "}"
    )
    dm <- file.path (d, "man")
    if (!file.exists (dm)) {
        dir.create (dm)
    }
    writeLines (rdfile, con = file.path (dm, "test_name.Rd"))
}

make_test_logical <- function (d) {

    rfile <- c (
        "#' test_logical",
        "#' A logical test funtion",
        "#' @param flag logical input",
        "#' @return return value",
        "#' @examples",
        "#' test_logical(TRUE)",
        "#' @export",
        "test_logical <- function(flag = TRUE) {",
        "  if (isTRUE (flag)) 1 else 0 }"
    )
    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }
    writeLines (rfile, con = file.path (dr, "test-logical.R"))

    rdfile <- c (
        "\\name{test_logical}",
        "\\alias{test_logical}",
        "\\title{test_logical",
        "A logical test funtion}",
        "\\usage{test_logical(flag = TRUE)}",
        "\\arguments{",
        "\\item{flag}{logical input}",
        "}",
        "\\value{return value}",
        "\\description{test_logical A logical test funtion}",
        "\\examples{",
        "test_logical(TRUE)",
        "}"
    )
    dm <- file.path (d, "man")
    if (!file.exists (dm)) {
        dir.create (dm)
    }
    writeLines (rdfile, con = file.path (dm, "test_logical.Rd"))
}

make_namespace <- function (d) {

    nfile <- c (
        "importFrom(data.table,`:=`)",
        "export(test_int)",
        "export(test_int_range)",
        "export(test_int_negrange)",
        "export(test_rect)",
        "export(test_name)",
        "export(test_logical)"
    )
    writeLines (nfile, con = file.path (d, "NAMESPACE"))
}

make_pkg <- function () {

    d <- make_pkg_path ()
    make_desc (d)
    make_test_int (d)
    make_test_rect (d)
    make_test_name (d)
    make_test_logical (d)
    make_namespace (d)

    return (d)
}
