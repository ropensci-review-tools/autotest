context ("local source package")

source ("../local-pkg.R")
# functions to make local package, including
# - make_pkg(), which itself calls
# - d <- make_pkg_path ()
# - make_desc (d)
# - make_test_int (d), which also adds test_int_range() and
#   test_int_negrange()
# - make_test_rect (d)
# - make_test_name (d)
# - make_test_logical (d)
# - make_namespace (d)

test_that ("pkg", {

    package <- make_pkg ()
    xf <- autotest_package (package = package)
    expect_true (all (xf$test))
    expect_true (all (xf$type == "dummy"))

    xt <- autotest_package (package = package, test = TRUE)
    expect_true (all (xt$test))
    types <- table (xt$type)
    expect_true (all (
        names (types) %in% c ("diagnostic", "warning", "error")
    ))
    # 22, not the 3 the old yaml-driven pipeline found: the trace-based
    # pipeline compares test_rect()'s output pairwise across all three
    # synthetic rectangular-class conversions (tbl_df/data.table/newclass)
    # it generates, rather than just a subset; 3 of those 22 come from
    # test_logical()'s int/char-substitution and length-2 diagnostics, and
    # 1 from test_int_range()'s length-2 diagnostic (test_int_negrange()
    # contributes no visible row: its "negative integer" documentation
    # skips test_single_int_range's numeric-mismatch check entirely).
    expect_identical (as.integer (types [names (types) == "diagnostic"]), 22L)
    # test_name()'s traced "name" parameter is deliberately rejected when
    # mutated to a character, exercising test_single_name.autotest_obj's
    # error-reporting path.
    expect_identical (as.integer (types [names (types) == "error"]), 1L)
    err <- xt [xt$type == "error", ]
    expect_identical (err$fn_name, "test_name")
    expect_identical (err$test_name, "name_or_formula_as_char")

    diag <- xt [xt$type == "diagnostic" & xt$fn_name == "test_logical", ]
    expect_identical (
        sort (diag$test_name),
        sort (c (
            "subst_int_for_logical",
            "subst_char_for_logical",
            "single_par_as_length_2"
        ))
    )
})

test_that ("pkg with test_data", {

    package <- make_pkg ()
    xf <- autotest_package (package = package)

    # Switch a single test off via 'test_data', exercising the
    # 'test_data'-gated branch present in every 'input-*.R'/'test-*.R'
    # mutation-test function (and 'test_these_data()' itself), which is
    # otherwise never reached when 'test_data' is left as its default NULL.
    idx <- which (
        xf$test_name == "negate_logical" & xf$fn_name == "test_logical"
    )
    expect_identical (length (idx), 1L)
    xf$test [idx] <- FALSE

    xt <- autotest_package (package = package, test = TRUE, test_data = xf)

    # the switched-off test produces no row at all: "no_test" rows are
    # filtered out once 'test = TRUE', but the code path that assigns
    # 'type <- "no_test"' still executes on the way there.
    expect_false ("negate_logical" %in% xt$test_name [
        xt$fn_name == "test_logical"
    ])
})
