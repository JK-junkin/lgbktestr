library(lgbktestr)
context("Test ui-inner functions")

test_that("test_sum() treats `...` properly like dplyr::select()", {
  input <- data.frame(a = 1:3, b = 4:6, c = 7:9, 
                      ab = 1:3 + 4:6,
                      sum = 1:3 + 4:6 + 7:9)
  
  expect_true(test_sum(input, total_col = "sum", c("a", "b", "c")))
  expect_true(test_sum(input, total_col = "sum", c(1, 2, 3)))
  expect_true(test_sum(input, total_col = "sum", 1:3))
  expect_true(test_sum(input, total_col = "sum", "a", "b", "c"))
  expect_true(test_sum(input, total_col = "sum", a:c))
  expect_true(test_sum(input, total_col = "sum", `a`:`c`))
  expect_true(test_sum(input, total_col = "sum", `a`:`b`, c))
  expect_true(test_sum(input, total_col = "ab", -c, -ab, -sum))
  expect_true(test_sum(input, total_col = "ab", -c(c, sum, ab)))
  expect_true(test_sum(input, total_col = "ab", -c("c", "sum", "ab")))
  expect_false(test_sum(input, total_col = "ab", -c, -sum))
})
