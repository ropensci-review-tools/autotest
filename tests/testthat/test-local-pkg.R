
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
              types <- table (xt$type)
              expect_true (all (names (types) %in% c ("diagnostic", "warning")))
              expect_equal (as.integer (types [names (types) == "diagnostic"]), 3L)
})
