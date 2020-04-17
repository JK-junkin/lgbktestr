#' Test length to be correctyly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name to be scaned
#' @param u_len an unique value (ideal length) to be matched against
#'
#' @return any warning messages
#'
#' @importFrom testthat test_that expect_identical
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' test_length_identical(dat, column = "foo", u_len = 1L)
#' }
#' @export
test_length_identical <- function(dat, column, u_len) {
  target <- dat[, column] %>%
    unique() %>%
    na.omit() %>%
    length()

  test_that("is operation region mixed?", {
    expect_identical(target, u_len)
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
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{test_year(dat, column = "foo", u_value = 2020)}
#' @export
test_value_identical <- function(dat, column, u_value) {
  target <- dat[, column] %>%
    unique() %>%
    na.omit() %>%
    as.integer()

  test_that("is a value equal to?", {
    expect_identical(target, u_value)
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
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' test_region_numcode(dat, column = "foo", ideal_code_numbers = 1:2)
#' }
#' @export
test_value_equals <- function(dat, column, u_values) {
  target <- dat[, column] %>%
    unique() %>%
    na.omit() %>%
    as.integer()

  test_that("is code number correct?", {
    expect_true(target %all_in% u_values)
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
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' test_sum(dat, total_col = "foobar", partial_cols = c("foo", "bar"))
#' }
#' @export
test_sum <- function(dat, total_col, partial_cols = c()) {
  target <- dat[, total_col] %>%
    na.omit()

  against <- dat %>%
    # dplyr::select(`A`:`G`) %>%
    dplyr::select_at(.vars = dplyr::vars(partial_cols)) %>%
    rowSums(na.rm = F) %>%
    na.omit()

  test_that("is total value equal to each partial values?", {
    expect_equal(target, against)
  })
}
