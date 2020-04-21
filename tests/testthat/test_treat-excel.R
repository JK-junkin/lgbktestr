library(lgbktestr)
context("I/O: treat_excel()")

test_that("treat_excel() returns data.frame", {
  expect_warning(
    expect_is(
      treat_excel("excel/PS-dummy-1.xlsx"), "data.frame"
      ),
    'Removed column\\(s\\): "...44", "...45"'
  )
})
