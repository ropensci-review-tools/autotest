context ("text parsing")

test_that ("match_brackets", {
    x <- c (
        "some(stuff and",
        "some) more"
    )
    res <- match_brackets (x)
    expect_length (res, 1)

    x <- c (
        "some (stuff '(in brackets and)'",
        "close that one) done"
    )
    res <- match_brackets (x)
    expect_length (res, 1)

    x <- c (
        "some (stuff '(in brackets ",
        "and)' close that one) done"
    )
    res <- match_brackets (x)
    expect_length (res, 1)
})

test_that ("unmatched brackets", {
    x <- c (
        "some (stuff \"with(unmatched\" bracket) done",
        "and (another) line)"
    )
    open_sym <- "\\("
    close_sym <- "\\)"
    both_sym <- "\\((.+)?\\)"
    res <- bracket_sequences (x, open_sym, close_sym, both_sym)
    expect_true (all (is.na (unlist (res))))

    expect_null (match_brackets (x))
})
