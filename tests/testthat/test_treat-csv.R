library(lgbktestr)
context("I/O: treat_csv()")

test_that("treat_csv() returns data.frame", {
  df <- data.frame(a = letters)
  expect_message(
    treat_csv(df),
    "`treat_csv\\(\\)` is under construction. Please wait until release."
    )
})
