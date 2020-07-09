context("rect")

# pre-process variable is 'n' so yaml handlers are activated
# R/yaml.R#13-22
yaml <- c ("package: SmartEDA",
"functions:",
"   - ExpData:",
"       - preprocess:",
"           - 'n <- ISLR::Carseats'",
"           - 'n$Sales <- sqrt (n$Sales)'",
"           - 'n$CompPrice <- as.integer (n$CompPrice)'",
"           - 'n$Sales [ceiling (runif (10) * nrow (n))] <- NA'",
"       - parameters:",
"           - data: n",
"   - ExpData:",
"       - parameters:",
"           - data: ISLR::College",
"   - ExpStat:",
"       - parameters:",
"           - X: ISLR::Carseats$Sales",
"           - Y: ISLR::Carseats$Urban",
"           - valueOfGood: 'Yes'")

test_that("rect test", {

    expect_message (
        autotest_rectangular (yaml)
    )

    f <- file.path (tempdir (), "junk2.yaml")
    con <- file (f)
    writeLines (yaml, con = con)
    close (con)
    expect_true (file.exists (f))

    expect_message (
        autotest_rectangular (filename = f)
    )
             })
