library(lgbktestr)
context("Column names")

test_that("uniform_df handles data frame", {
  expect_warning(
    df1 <- data.frame(A = logical(10L), B = integer(10L)) %>%
      uniform_df(),
    'Removed column\\(s\\): "A", "B"'
  )
  expect_equal(colnames(df1), fetch_ideal_colnames(fishery = "purse seine"))
})
