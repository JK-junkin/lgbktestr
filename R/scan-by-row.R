#' Detect row(s) which is/are suspected
#'
#' @param .d a data.frame
#' @return a list of
#' @importFrom magrittr %>%
#' @importFrom rlang quo parse_expr
#' @examples
#' \dontrun{
#' scan_by_row(df)
#' }
#' @export
scan_by_row <- function(.d) {
 .d
}

# Helper ------------
#' Give the \code{FALSE} indices of a logical object, allowing for array indices
#'
#' @param x a logical vector or array. NAs
#' @return indices (usually number)
#' @examples
#' which_false(c(T, T, F, F))
#' # [1] 3 4
which_false <- function(x) { which(!x) }

#' this function fails, why?
# eval_quosure <- function(q) {
#   !!rlang::quo(!!rlang::parse_expr(q))
# }
#
