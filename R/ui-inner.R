#' Test length to be correctyly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name ("string") to be scaned
#' @param u_len an unique value (ideal length) to be matched against
#'
#' @return any warning messages
#'
#' @importFrom testthat test_that expect_identical
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' test_length_identical(dat, column = "foo", u_len = 1L)
#' }
#' @export
test_length_identical <- function(dat, column, u_len) {
  target <- unique(dat[, column]) %>%
    stats::na.omit() %>%
    length()

  test_that("is operation region mixed?", {
    expect_identical(target, u_len)
  })
}

#' Test a number to be correctly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name ("string") to be scaned
#' @param u_value an unique value (ideal number) to be matched against
#'
#' @return any warning messages
#'
#' @importFrom testthat test_that expect_identical
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{test_year(dat, column = "foo", u_value = 2020)}
#' @export
test_value_identical <- function(dat, column, u_value) {
  target <- unique(dat[, column]) %>%
    stats::na.omit() %>%
    as.integer()

  test_that("is a value equal to?", {
    expect_identical(target, u_value)
  })
}

#' Test numbers to be correctly digitalized
#'
#' @param dat data frame including column to be inspected
#' @param column a column name ("string") to be scaned
#' @param u_values unique values (ideal numbers) to be matched against
#'
#' @return an warning message
#'
#' @importFrom testthat test_that expect_true
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' test_region_numcode(dat, column = "foo", ideal_code_numbers = 1:2)
#' }
#' @export
test_value_equals <- function(dat, column, u_values) {
  target <- unique(dat[, column]) %>%
    stats::na.omit() %>%
    as.integer()

  test_that("is code number correct?", {
    expect_true(target %all_in% u_values)
  })
}

#' Test total value corresponded with the sum of partial values
#'
#' @param dat data frame including column to be inspected
#' @param total_col a column name ("string") of total value digitized
#' @param ... a sequence of column names to be sumed up
#'
#' @return an warning message
#'
#' @importFrom testthat test_that expect_equal
#' @importFrom magrittr %>%
#'
#' @examples
#' data <- data.frame(a = 1:3, b = 4:6, c = 7:9, sum = 1:3 + 4:6 + 7:9)
#' test_sum(data, total_col = "sum", "a", "b", "c"))
#' test_sum(data, total_col = "sum", 1:3)
#' test_sum(data, total_col = "sum", c("a", "b", "c")))
#' test_sum(data, total_col = "sum", `a`:`c`))
#' 
#' @export
test_sum <- function(dat, total_col, ...) {
  target <- stats::na.omit(dat[, total_col])

  # Referred frome dplyr::select.data.frame
  loc <- tidyselect::eval_select(rlang::expr(c(...)), dat)
  loc <- ensure_group_vars(loc, dat, notify = TRUE)

  against <- magrittr::set_names(dat[loc], names(loc)) %>%
    rowSums(na.rm = F) %>%
    na.omit()

  test_that("is total value equal to each partial values?", {
    expect_equal(target, against)
  })
}

# Helpers -----------------------------------------------------------------

# Referred from dplyr's R/select.R
ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(dplyr::group_vars(data), names(data))
  missing <- dplyr::setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names(data)[missing]
    if (notify) {
      rlang::inform(glue::glue(
        "Adding missing grouping variables: ",
        paste0("`", names(data)[missing], "`", collapse = ", ")
      ))
    }
    loc <- c(magrittr::set_names(missing, vars), loc)
  }

  loc
}
