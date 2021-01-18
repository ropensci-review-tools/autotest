
context("local source package")

source ("../local-pkg.R")
# functions to make local package, including
# - make_pkg()

test_that("pkg", {

              package <- make_pkg ()
              x <- autotest_package (package = package)
})
