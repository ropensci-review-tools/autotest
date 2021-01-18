context("test types")

test_that("autotest_types", {

    expect_silent (x <- autotest_types ())
    expect_s3_class (x, "autotest_package")
    expect_equal (ncol (x), 8)
    expect_true (all (x$test))

    t1 <- x$test_name [1]
    expect_silent (x2 <- autotest_types (notest = t1))
    expect_equal (nrow (x), nrow (x2))
    expect_true (!all (x2$test))
    tests <- x2$test_name [which (!x2$test)]
    expect_true (all (tests == t1))
})
