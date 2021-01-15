
context("text parsing")

test_that("match_brackets", {
              x <- c ("some(stuff and",
                      "some) more")
              res <- match_brackets (x)
              expect_length (res, 1)

              x <- c ("some (stuff '(in brackets and)'",
                      "close that one) done")
              res <- match_brackets (x)
              expect_length (res, 1)

              x <- c ("some (stuff '(in brackets ",
                      "and)' close that one) done")
              expect_error (match_brackets (x),
                            "NA/NaN argument")

})

test_that ("unmatched brackets", {
              x <- c ("some (stuff \"with(unmatched\" bracket) done",
                      "and (another) line)")
              open_sym <- "\\("
              close_sym <- "\\)"
              both_sym <- "\\((.+)?\\)"
              res <- bracket_sequences (x, open_sym, close_sym, both_sym)
              expect_true (all (is.na (unlist (res))))

              expect_null (match_brackets (x))
})

test_that ("parse_expressions", {
               # example from ?stats::approx with an expression within "{".
               # parse_brackets presumes each bit to represent a separate line
               # and appends them with ";", while parse_expressions does not
               # insert the extra ";".
               x <- c ("xn <- 1:4",
                       "yn <- c(1,NA,3:4)",
                       "xout <- (1:9)/2",
                       "rules <- list(N=1, C=2, NC=1:2, CN=2:1)",
                       "methods <- c(\"constant\",\"linear\")",
                       "ry <- sapply(rules, function(R) {",
                       "sapply(methods, function(M)",
                       "sapply(setNames(,c(TRUE,FALSE)), function(na.)",
                       "approx(xn, yn, xout=xout, method=M, rule=R, na.rm=na.)$y),", # nolint
                       "simplify=\"array\")",
                       "}, simplify=\"array\")")

               res1 <- match_brackets (x, curly = TRUE)
               res2 <- parse_expressions (x)
               expect_true (!identical (res1, res2))
               # The final `ry <- ...` is [6] in both:
               expect_true (grepl (";", res1 [6]))
               expect_true (!grepl (";", res2 [6]))

               expect_error (eval (parse (text = res1)),
                             "<text>:6:62: unexpected ';'")
               expect_silent (eval (parse (text = res2)))
})

test_that ("strip_if_cond", {
               x <- "if(x==1)y<-0"
               res <- strip_if_cond (x)
               expect_identical (x, res)

               x <- "if(x==1) {y<-0}"
               res <- strip_if_cond (x)
               expect_true (!identical (x, res))
               expect_identical (res [1], "y<-0")

               x <- "if(x==1) {y<-0} else {y<-1}"
               res <- strip_if_cond (x)
               expect_true (!identical (x, res))
               expect_identical (res, c ("y<-0", "y<-1"))
})

test_that ("unpipe", {
               x <- "iris %>% as.list()"
               res <- unpipe (x)
               # res should be:
               # var1 <- iris
               # var2 <- as.list(var1)
               expect_length (res, 2)
               # grepl because lines can have additional spaces
               expect_true (grepl ("var1 <- iris", res [1]))
               expect_true (grepl ("var2\\s+<-\\s+as.list\\(var1\\)", res [2]))

               x <- "iris %>% as.list() %>% magrittr::extract2(\"Species\")"
               res <- unpipe (x)
               # res [3] == "var3 <- magrittr::extract2(var2, "Species")"
               expect_true (grepl ("var3", res [3]))
               expect_true (grepl ("var2", res [3]))
})
