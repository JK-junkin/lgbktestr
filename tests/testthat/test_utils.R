library(lgbktestr)
context("Column names")

test_that("uniform_df standardize data.frame properly", {
  expect_warning(
    df1 <- data.frame(A = logical(10L), B = integer(10L)) %>%
      uniform_df(),
    'Removed column\\(s\\): "A", "B"'
  )
  expect_equal(colnames(df1), fetch_ideal_colnames(fishery = "purse seine"))
})


context("Clearance strings")

test_that("str_rm_newline_code treats a string properly", {
  expect_equal(str_rm_newline_code("A\n"),   "A") # LF: Line Feed. 
  expect_equal(str_rm_newline_code("A\r"),   "A") # CR: Carriage Return.
  expect_equal(str_rm_newline_code("A\r\n"), "A") # CRLF: Mixture of CR and LF.
  expect_equal(str_rm_newline_code("A\nB"),   "AB") # intermediation
  expect_equal(str_rm_newline_code("A\rB"),   "AB")
  expect_equal(str_rm_newline_code("A\r\nB"), "AB")
  expect_equal(str_rm_newline_code("A\nB\n"), "AB")     # twice
  expect_equal(str_rm_newline_code("A\nB\r\nC"), "ABC") # twice intermediation
  expect_equal(str_rm_newline_code("A\r\n\n\rB\n\r\r\nC"), "ABC") # complex
  expect_equal(str_rm_newline_code("ABC"), "ABC")       # none
})

test_that("str_rm_newline_code treats a string vector properly", {
  expect_equal(str_rm_newline_code(c("A\n", "B\n")),         c("A", "B"))
  expect_equal(str_rm_newline_code(c("A\r", "B\r")),         c("A", "B"))
  expect_equal(str_rm_newline_code(c("A\r\n", "B\r\n")),     c("A", "B"))
  expect_equal(str_rm_newline_code(c("A\nB", "C\nD")),       c("AB", "CD"))
  expect_equal(str_rm_newline_code(c("A\rB", "C\rD")),       c("AB", "CD"))
  expect_equal(str_rm_newline_code(c("A\r\nB", "C\r\nD")),   c("AB", "CD"))
  expect_equal(str_rm_newline_code(c("A\nB\n", "C\nD\n")),   c("AB", "CD"))
  expect_equal(str_rm_newline_code(c("A\r\n\n\rB", "C\n\r\rD")), c("AB", "CD"))
  expect_equal(str_rm_newline_code(c("ABC", "ABC")), c("ABC", "ABC"))
})
