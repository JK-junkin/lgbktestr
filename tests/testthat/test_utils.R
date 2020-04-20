library(lgbktestr)
context("Column names")

test_that("fetch_ideal_colnames() returns a colnames vector", {
  expect_is(fetch_ideal_colnames(fishery = "purse_seine"), "character")
  expect_null(fetch_ideal_colnames(fishery = "long_line"))
  expect_null(fetch_ideal_colnames(fishery = "pole_and_line"))
})

test_that("uniform_df() standardize data.frame properly", {
  expect_warning(
    df1 <- data.frame(A = logical(10L), B = integer(10L)) %>%
      uniform_df(),
    'Removed column\\(s\\): "A", "B"'
  )
  expect_equal(colnames(df1), fetch_ideal_colnames(fishery = "purse_seine"))
})


context("Clearance strings")

test_that("str_rm_newline_code() treats a string properly", {
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

test_that("str_rm_newline_code() treats stiring vector properly", {
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


context("Miscellaneous functions")

test_that("plus() treats more than two vectors properly", {
  a <- 1:3; b <- 4:6; c <- 7:9; d <- -(7:9)
  expect_equal(plus(a, b, c), 1:3 + 4:6 + 7:9)
  expect_equal(plus(a, b, d), 1:3 + 4:6 - 7:9)

  e <- complex(real = 1:3, imaginary = 4:6)
  f <- complex(real = 4:6, imaginary = 7:9)
  g <- complex(real = 7:9, imaginary = 1:3)
  expect_equal(plus(e, f, g), complex(real = 3 * 4:6, imaginary = 3 * 4:6))
})

test_that("`%all_in%` works properly", {
  expect_true(letters %all_in% letters)
  expect_true(letters[1:3] %all_in% letters)
  expect_false(letters %all_in% letters[1:3])
})
