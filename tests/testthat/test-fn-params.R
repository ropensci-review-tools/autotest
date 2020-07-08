context("fn-params")

test_that("yaml test", {

              expect_error (at_get_fn_params (),
                            "function must be specified")
              expect_error (at_get_fn_params (fn_name = "blah"),
                            "If pkg_name not specified, fn_name must be")

              expect_silent (x <- at_get_fn_params (fn_name = "base::mean"))
              expect_is (x, "list")
              expect_equal (length (x), 2)

             })
