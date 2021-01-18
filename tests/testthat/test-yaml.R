context("yaml")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TEST_ALL"), "true"))

test_that("main function errors", {
              expect_error (autotest_yaml (),
                            "either yaml or filename must be given")
              expect_error (autotest_yaml (1),
                            "yaml must be either a single character vector")
              x <- 1
              attr (x, "package") <- 1
              x <- list (x)
              expect_error (autotest_yaml (x),
                            "yaml must be either a single character vector")

              expect_null (autotest_single_yaml ())
             })

test_that("yaml test", {

    expect_message (
                    at_yaml_template (),
                    "template written to")

    expect_message (
                    at_yaml_template (),
                    "yaml template") # already exists

    f <- tempfile (fileext = ".yaml")
    expect_message (
                    at_yaml_template (f),
                    "template written to")
    expect_true (file.exists (f))

    x <- readLines (f)
    expect_equal (length (x), 13)
    expect_silent (y1 <- yaml::yaml.load (x))
    expect_silent (y2 <- yaml::yaml.load_file (f))
    expect_identical (y1, y2)

    td <- tempdir ()
    s <- "A"
    if (substr (td, nchar (td), nchar (td)) == s)
        s <- "B"
    td <- paste0 (substr (td, 1, nchar (td) - 1), s)
    if (test_all)
        expect_error (at_yaml_template (loc = td), "Directory") # does not exist

             })

test_that ("yaml filename", {
              #exs <- examples_to_yaml (package = "stats", functions = "var")
              yaml <- c ("package: stats",
                         "functions:",
                          "    - var:",
                          "        - parameters:",
                          "            - x: 1:10")
              attr (yaml, "package") <- "stats"

              f <- tempfile (fileext = ".yaml")
              writeLines (yaml, con = f)

              res1 <- autotest_yaml (filename = f)
              res2 <- autotest_yaml (yaml = yaml)
              expect_identical (res1, res2)
             })

test_that("yaml internal", {
              expect_error (parse_yaml_template (),
                            "either yaml or filename must be given")
             })
