context("dummy")

yaml <- c ("package: SmartEDA",
"functions:",
"   - ExpData:",
"       - preprocess:",
"           - 'x <- ISLR::Carseats'",
"           - 'x$Sales <- sqrt (x$Sales)'",
"           - 'x$CompPrice <- as.integer (x$CompPrice)'",
"           - 'x$Sales [ceiling (runif (10) * nrow (x))] <- NA'",
"       - parameters:",
"           - data: x",
"   - ExpData:",
"       - parameters:",
"           - data: ISLR::College",
"   - ExpStat:",
"       - parameters:",
"           - X: ISLR::Carseats$Sales",
"           - Y: ISLR::Carseats$Urban",
"           - valueOfGood: 'Yes'")

test_that("dummy test", {

    expect_true (TRUE)
    expect_message (
    autotest_rectangular (yaml)
    )

             })
