context("yaml")

test_that("yaml test", {

    expect_message (
                    at_yaml_template (),
                    "template written to")

    expect_message (
                    at_yaml_template (),
                    "yaml template") # already exists
             })
