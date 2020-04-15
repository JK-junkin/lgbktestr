#' Test length to be correctyly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name to be scaned
#' @param u_len an unique value (ideal length) to be matched against
#'
#' @return any warning messages
#'
#' @importFrom testthat test_that expect_identical
#'
#' @examples
#' \dontrun{
#' test_length_identical(dat, column = "foo", u_len = 1L)
#' }
test_length_identical <- function(dat, column, u_len) {
  test_that("is operation region mixed?", {
    expect_identical(length(unique(dat[, column])), u_len)
  })
}

#' Test a number to be correctly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name to be scaned
#' @param u_value an unique value (ideal number) to be matched against
#'
#' @return any warning messages
#'
#' @importFrom testthat test_that expect_identical
#'
#' @examples
#' \dontrun{test_year(dat, column = "foo", u_value = 2020)}
test_value_identical <- function(dat, column, u_value) {
  test_that("is ", {
    expect_identical(unique(as.integer(dat[, column])), u_value)
  })
}

#' Test numbers to be correctly digitalized
#'
#' @param dat data frame including column to be inspected
#' @param column a column name to be scaned
#' @param u_values unique values (ideal numbers) to be matched against
#'
#' @return an warning message
#'
#' @importFrom testthat test_that expect_true
#'
#' @examples
#' \dontrun{
#' test_region_numcode(dat, column = "foo", ideal_code_numbers = 1:2)
#' }
test_value_equals <- function(dat, column, u_values) {
  test_that("is code number correct?", {
    expect_true(unique(as.integer(dat[, column])) %all_in% u_values)
  })
}

#' Test total value corresponded with the sum of partial values
#'
#' @param dat data frame including column to be inspected
#' @param total_col a column name of total value digitized
#' @param partial_cols a list of column names to sum up
#'
#' @return an warning message
#'
#' @importFrom testthat test_that expect_equal
#'
#' @examples
#' \dontrun{
#' test_sum(dat, total_col = "foobar", partial_cols = list("foo", "bar"))
#' }
test_sum <- function(dat, total_col, partial_cols = list()) {
  test_that("is total value equal to each partial values?", {
    expect_equal(dat[, total_col],
                 with(dat, plus(partial_cols, na.rm = T)))
  })
}
# test_that("is total catch equal to the sum of each catch?", {
#   expect_equal(test_xl$合計,
#                test_xl %>%
#                  dplyr::select(`まいわし(小中)`:`その他`) %>%
#                  dplyr::mutate(sum = rowSums(., na.rm = T)) %>%
#                  dplyr::pull(sum))
# })
