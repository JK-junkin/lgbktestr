library(lgbktestr)
context("I/O: check_logbkkk()")

test_that("check_logbook() returns data.frame", {
  expect_warning(
    expect_is(
      check_logbook(file = "excel/PS-dummy-1.xlsx"), "data.frame"
      ),
    'Removed column\\(s\\): "...44", "...45"'
  )
})
