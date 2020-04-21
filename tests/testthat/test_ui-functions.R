library(lgbktestr)
context("I/O: check_logbook()")

test_that("check_logbook() returns data.frame", {
  expect_warning(
    expect_is(
      check_logbook("excel/PS-dummy-1.xlsx"), "data.frame"
      ),
    'Removed column\\(s\\): "...44", "...45"'
  )
})
