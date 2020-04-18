library(lgbktestr)
context("Read and Write (I/O)")

test_that("check_logbook() returns data.frame", {
  expect_warning(
    expect_is(check_logbook("excel/PS-dummy-1.xlsx"), "data.frame"),
    'Removed column\\(s\\): "...44", "...45"'
  )
})
