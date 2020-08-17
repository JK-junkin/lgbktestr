#' Test length to be correctyly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name ("string") to be scaned
#' @param u_len an unique value (ideal length) to be matched against
#' @return any warning messages
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' is_unique_length(dat, column = "foo", u_len = 1L)
#' }
#' @export
is_unique_length <- function(dat, column, u_len) {
  target <- dat %>%
    labdsv::defactorize() %>%
    dplyr::pull(column) %>%
    stats::na.omit() %>%
    unique() %>%
    length()

  isTRUE(all(target, u_len, scale = 1))
}

#' Test a number to be correctly digitalized
#'
#' @param dat data frame including column to be scaned
#' @param column a column name ("string") to be scaned
#' @param u_vals unique values to be matched against
#' @return any warning messages
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' is_unique_values(dat, column = "foo", u_vals = 2020)
#' }
#' @export
is_unique_values <- function(dat, column, u_vals) {
  target <- dat %>%
    labdsv::defactorize() %>%
    dplyr::pull(column) %>%
    stats::na.omit() %>%
    unique()

  isTRUE(all.equal(target, u_vals, scale = 1))
}

#' Test numbers to be correctly digitalized
#'
#' @param dat data frame including column to be inspected
#' @param column a column name ("string") to be scaned
#' @param u_vals unique values being expected to include all target values
#' @return an warning message
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' is_all_uniq_vals_in(dat, column = "foo", u_vals = 1:2)
#' }
#' @export
is_all_uniq_vals_in <- function(dat, column, u_vals) {
  target <- dat %>%
    labdsv::defactorize() %>%
    dplyr::pull(column) %>%
    stats::na.omit() %>%
    unique()

  target %all_in% u_vals
}

#' Test total value corresponded with the sum of partial values
#'
#' @param dat data frame including column to be inspected
#' @param column a column name ("string") of total value digitized
#' @param ... a sequence of column names to be sumed up
#' @return an warning message
#' @importFrom magrittr %>%
#' @examples
#' data <- data.frame(a = 1:3, b = 4:6, c = 7:9, sum = 1:3 + 4:6 + 7:9)
#' is_sum(data, column = "sum", "a", "b", "c")
#' is_sum(data, column = "sum", 1:3)
#' is_sum(data, column = "sum", c("a", "b", "c"))
#' is_sum(data, column = "sum", `a`:`c`)
#' @export
is_sum <- function(dat, column, ...) {
  dd <- labdsv::defactorize(dat)

  target <- dd %>%
    dplyr::pull(column) %>%
    stats::na.omit()

  # # Referred frome dplyr::select.data.frame
  # loc <- tidyselect::eval_select(rlang::expr(c(...)), dd, env = environment())
  # loc <- ensure_group_vars(loc, dd, notify = TRUE)
  # against <- magrittr::set_names(dd[loc], names(loc)) %>%
  against <- dat %>%
    dplyr::select(...) %>%
    rowSums(na.rm = F) %>%
    stats::na.omit()

  isTRUE(all.equal.numeric(target, against, scale = 1))
}

# Helpers -----------------------------------------------------------------

# Referred from dplyr's R/select.R
# ensure_group_vars <- function(loc, data, notify = TRUE) {
#   group_loc <- match(dplyr::group_vars(data), names(data))
#   missing <- dplyr::setdiff(group_loc, loc)
#
#   if (length(missing) > 0) {
#     vars <- names(data)[missing]
#     if (notify) {
#       rlang::inform(glue::glue(
#         "Adding missing grouping variables: ",
#         paste0("`", names(data)[missing], "`", collapse = ", ")
#       ))
#     }
#     loc <- c(magrittr::set_names(missing, vars), loc)
#   }
#
#   loc
# }
