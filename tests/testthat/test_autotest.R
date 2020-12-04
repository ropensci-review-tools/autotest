context("autotest")

test_that("autotest", {

    ex <- examples_to_yaml (package = "stats",
                            functions = "reshape")

    expect_message (
        x <- autotest (ex [[1]])
        )
    expect_is (x, "data.frame")
    expect_equal (ncol (x), 6)
    expect_identical (names (x), c ("type",
                                    "fn_name",
                                    "parameter",
                                    "operation",
                                    "content",
                                    "yaml_hash"))

    f <- file.path (tempdir (), "junk2.yaml")
    con <- file (f)
    writeLines (ex [[1]], con = con)
    close (con)
    expect_true (file.exists (f))

    expect_silent (
        x2 <- autotest (filename = f, quiet = TRUE)
        )
    expect_identical (x, x2)
             })
