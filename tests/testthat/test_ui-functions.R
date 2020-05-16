library(lgbktestr)
context("I/O: check_logbook()")

test_that("check_logbook.excel() returns data.frame", {
  expect_warning(
    expect_is(
      check_logbook("excel/PS-dummy-1.xlsx"), "data.frame"
      ),
    'Removed column\\(s\\): "...44", "...45"'
  )
})

test_that("check_logbook.csv() returns data.frame", {
  df <- data.frame(a = letters)
  expect_message(
    check_logbook.csv(df),
    "CSV file can't be treated for now. Please wait until release."
  )
})
