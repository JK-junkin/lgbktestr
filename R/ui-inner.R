#' Test region column not to be intermingled
#'
#' @param dat data frame including column to be scaned
#' @param column a column name to be scaned
#'
#' @return any warning messages
#' 
#' @importFrom testthat test_that expect_identical
#'
#' @examples
#' \dontrun{test_region_coexistence(dat, column = "foo")}
test_region_coexistence <- function(dat, column) {
  test_that("is operation region mixed?", {
    expect_identical(length(unique(dat[, column])), 1L)
  })
}

#' Test year column to be corresponded with ideal year
#'
#' @param dat data frame including column to be scaned
#' @param column a column name of year to be inte
#' @param year an ideal year (integer) to be matched against
#'
#' @return any warning messages
#'
#' @importFrom testthat test_that expect_identical
#' 
#' @examples
#' \dontrun{test_year(dat, column = "foo")}
test_year <- function(dat, column, year) {
  test_that("year", {
    expect_identical(unique(as.integer(dat[, column])), year)
  })
}

#' Test cruise length column to be corresponded with sum of operation and search
#'
#' @param dat data frame including column to be inspected
#' @param total_col column name of trip length
#' @param partial_cols columns to sum up
#'
#' @return an warning message
#' 
#' @importFrom testthat test_that expect_equal
#'
#' @examples
#' \dontrun{
#' 
#' }
test_cruise_length <- function(dat, total_col, partial_cols = list()) {
  test_that("cruise length", {
    expect_equal(dat[, total_col],
                 with(dat, plus(partial_cols, na.rm = T)))
  })
}

# 3. 操業海域
test_that("is operation region correct?", {
  expect_true(unique(as.integer(test_xl$操業海域)) %all_in% 1:2)
})

# 4. 漁業種類
test_that("is fishery type correct?", {
  expect_true(unique(as.integer(test_xl$漁業種類コード)) %all_in% 13)
})

# 5. 漁法
test_that("is gear type correct?", {
  expect_true(unique(as.integer(test_xl$漁法コード)) %all_in% 251:252)
})

# 6. 漁獲量
test_that("is total catch equal to the sum of each catch?", {
  expect_equal(test_xl$合計,
               test_xl %>%
                 dplyr::select(`まいわし(小中)`:`その他`) %>%
                 dplyr::mutate(sum = rowSums(., na.rm = T)) %>%
                 dplyr::pull(sum))
})
