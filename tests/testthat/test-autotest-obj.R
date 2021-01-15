
context("autotest object")

test_that("object construction", {
              obj <- autotest_obj ()
              expect_is (obj, "autotest_obj")
              expect_is (obj, "list")
              expect_length (obj, 11)
              expect_identical (names (obj),
                                c ("package", "package_loc", "test_name",
                                   "params", "param_types", "fn", "class",
                                   "classes", "env", "test", "quiet"))
              expect_type (obj$env, "environment")
              expect_type (obj$params, "list")
})

test_that("objet defaults", {
              obj <- autotest_obj ()
              expect_equal (obj$package, NA_character_)
              expect_equal (obj$package_loc, NA_character_)

              obj <- autotest_obj (package = "p")
              expect_equal (obj$package, "p")
              expect_equal (obj$package_loc, "p")

              obj <- autotest_obj (package = "p", package_loc = "q")
              expect_equal (obj$package, "p")
              expect_equal (obj$package_loc, "q")
})
