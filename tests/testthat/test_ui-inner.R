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

test_that("test_unique_length() returns T/F correctly", {
  df <- data.frame(a = letters, b = LETTERS) # strings are factorized (default)
  tbl <- tibble::tibble(a = letters, b = LETTERS)

  expect_true(test_unique_length(df,  "a", 26L))
  expect_true(test_unique_length(df,  "a", 26))
  expect_false(test_unique_length(df, "a", 1))
  expect_true(test_unique_length(df,  "b", 26))
  expect_error(test_unique_length(df, c("a", "b"), 26 * 2))
  expect_error(test_unique_length(df, b, 26))

  expect_true(test_unique_length(tbl,  "a", 26L))
  expect_true(test_unique_length(tbl,  "a", 26))
  expect_false(test_unique_length(tbl, "a", 1))
  expect_true(test_unique_length(tbl,  "b", 26))
  expect_error(test_unique_length(tbl, c("a", "b"), 26 * 2))
  expect_error(test_unique_length(tbl, b, 26))
})

test_that("test_unique_values() returns T/F correctly", {
  df <- data.frame(a = letters, b = LETTERS) # strings are factorized (default)
  tbl <- tibble::tibble(a = letters, b = LETTERS)

  expect_true(test_unique_values(df,  "a", letters))
  expect_false(test_unique_values(df, "a", letters[1:13]))
  expect_true(test_unique_values(df,  "b", LETTERS))
  expect_error(test_unique_values(df, c("a", "b"), c(letters, LETTERS)))
  expect_error(test_unique_values(df, b, LETTERS))

  expect_true(test_unique_values(tbl,  "a", letters))
  expect_false(test_unique_values(tbl, "a", letters[1:13]))
  expect_true(test_unique_values(tbl,  "b", LETTERS))
  expect_error(test_unique_values(tbl, c("a", "b"), c(letters, LETTERS)))
  expect_error(test_unique_values(tbl, b, LETTERS))
})

test_that("test_all_uniq_vals_in() returns T/F correctly", {
  df <- data.frame(a = letters[1:13], b = LETTERS) # factorized (default)
  tbl <- tibble::tibble(a = letters[1:13], b = LETTERS[1:13])

  expect_true(test_all_uniq_vals_in(df,  "a", letters))
  expect_true(test_all_uniq_vals_in(df,  "b", LETTERS))
  expect_false(test_all_uniq_vals_in(df, "a", letters[2:26]))
  expect_false(test_all_uniq_vals_in(df, "b", LETTERS[2:26]))
  expect_error(test_all_uniq_vals_in(df, c("a", "b"), c(letters, LETTERS)))
  expect_error(test_all_uniq_vals_in(df, a, letters))

  expect_true(test_all_uniq_vals_in(tbl,  "a", letters))
  expect_false(test_all_uniq_vals_in(tbl, "b", LETTERS[2:26]))
  expect_error(test_all_uniq_vals_in(tbl, c("a", "b"), c(letters, LETTERS)))
  expect_error(test_all_uniq_vals_in(tbl, a, letters))
})
