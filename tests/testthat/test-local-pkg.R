
context("local source package")

source ("../local-pkg.R")
# functions to make local package, including
# - make_pkg(), which itself calls
# - d <- make_pkg_path ()
# - make_desc (d)
# - make_test_int (d)
# - make_test_rect (d)
# - make_namespace (d)

test_that("pkg", {

              package <- make_pkg ()
              xf <- autotest_package (package = package)
              expect_true (all (xf$test))
              expect_true (all (xf$type == "dummy"))

              xt <- autotest_package (package = package, test = TRUE)
              expect_true (all (xt$test))
              expect_true (all (xt$type == "diagnostic"))

              n <- test_rect_compare_outputs.NULL ()
              expect_true (all (n$test_name %in% xt$test_name))
})
