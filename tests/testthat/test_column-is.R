library(lgbktestr)
context("Column test functions")

test_that("is_sum() treats `...` properly like dplyr::select()", {
  input <- data.frame(a = 1:3, b = 4:6, c = 7:9,
                      ab = 1:3 + 4:6,
                      sum = 1:3 + 4:6 + 7:9)

  expect_true(is_sum(input, column = "sum", c("a", "b", "c")))
  expect_true(is_sum(input, column = "sum", c(1, 2, 3)))
  expect_true(is_sum(input, column = "sum", 1:3))
  expect_true(is_sum(input, column = "sum", "a", "b", "c"))
  expect_true(is_sum(input, column = "sum", a:c))
  expect_true(is_sum(input, column = "sum", `a`:`c`))
  expect_true(is_sum(input, column = "sum", `a`:`b`, c))
  expect_true(is_sum(input, column = "ab", -c, -ab, -sum))
  expect_true(is_sum(input, column = "ab", -c(c, sum, ab)))
  expect_true(is_sum(input, column = "ab", -c("c", "sum", "ab")))
  expect_false(is_sum(input, column = "ab", -c, -sum))
})

test_that("is_unique_length() returns T/F correctly", {
  df <- data.frame(a = letters, b = LETTERS) # strings are factorized (default)
  tbl <- tibble::tibble(a = letters, b = LETTERS)

  expect_true(is_unique_length(df,  "a", 26L))
  expect_true(is_unique_length(df,  "a", 26))
  expect_false(is_unique_length(df, "a", 1))
  expect_true(is_unique_length(df,  "b", 26))
  expect_error(is_unique_length(df, c("a", "b"), 26 * 2))
  expect_error(is_unique_length(df, b, 26))

  expect_true(is_unique_length(tbl,  "a", 26L))
  expect_true(is_unique_length(tbl,  "a", 26))
  expect_false(is_unique_length(tbl, "a", 1))
  expect_true(is_unique_length(tbl,  "b", 26))
  expect_error(is_unique_length(tbl, c("a", "b"), 26 * 2))
  expect_error(is_unique_length(tbl, b, 26))
})

test_that("is_unique_values() returns T/F correctly", {
  df <- data.frame(a = letters, b = LETTERS) # strings are factorized (default)
  tbl <- tibble::tibble(a = letters, b = LETTERS)

  expect_true(is_unique_values(df,  "a", letters))
  expect_false(is_unique_values(df, "a", letters[1:13]))
  expect_true(is_unique_values(df,  "b", LETTERS))
  expect_error(is_unique_values(df, c("a", "b"), c(letters, LETTERS)))
  expect_error(is_unique_values(df, b, LETTERS))

  expect_true(is_unique_values(tbl,  "a", letters))
  expect_false(is_unique_values(tbl, "a", letters[1:13]))
  expect_true(is_unique_values(tbl,  "b", LETTERS))
  expect_error(is_unique_values(tbl, c("a", "b"), c(letters, LETTERS)))
  expect_error(is_unique_values(tbl, b, LETTERS))
})

test_that("is_all_uniq_vals_in() returns T/F correctly", {
  df <- data.frame(a = letters[1:13], b = LETTERS) # factorized (default)
  tbl <- tibble::tibble(a = letters[1:13], b = LETTERS[1:13])

  expect_true(is_all_uniq_vals_in(df,  "a", letters))
  expect_true(is_all_uniq_vals_in(df,  "b", LETTERS))
  expect_false(is_all_uniq_vals_in(df, "a", letters[2:26]))
  expect_false(is_all_uniq_vals_in(df, "b", LETTERS[2:26]))
  expect_error(is_all_uniq_vals_in(df, c("a", "b"), c(letters, LETTERS)))
  expect_error(is_all_uniq_vals_in(df, a, letters))

  expect_true(is_all_uniq_vals_in(tbl,  "a", letters))
  expect_false(is_all_uniq_vals_in(tbl, "b", LETTERS[2:26]))
  expect_error(is_all_uniq_vals_in(tbl, c("a", "b"), c(letters, LETTERS)))
  expect_error(is_all_uniq_vals_in(tbl, a, letters))
})
