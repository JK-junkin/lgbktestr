library(lgbktestr)
context("Read and Write (I/O)")

test_that("test_excel_logbook returns data.frame", {
  expect_is(test_excel_logbook("excel/PS-dummy-1.xlsx"), "data.frame")
})