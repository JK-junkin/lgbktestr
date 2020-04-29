#' Remove all newline codes in any strings
#'
#' @param vector A vector to be processed.
#' @return Cleaned up string(s).
#' @importFrom magrittr %>%
#' @examples
#' x <- c("Main:\rSub", "Main2:\nSub", "Main3:\r\nSub")
#' str_rm_newline_code(x)
str_rm_newline_code <- function(vector) {
  vector %>%
    stringr::str_remove_all("\\r") %>%
    stringr::str_remove_all("\\n")
}

#' Genarater of ideal column names
#'
#' @param fishery fishery type of logbook
#' @return a sequence of name strings having to be equipped
#' @examples
#' fetch_ideal_colnames(fishery = "purse seine")
fetch_ideal_colnames <- function(fishery) {
  list_colnames[[fishery]]
}

#' Uniform (Standardize) data frame.
#'
#' @param .dat data frame to be processed
#' @param .fishery a fishery type string
#' @return A data.frame ncol reduced if excess columns exists.
#' @importFrom magrittr %>%
#' @examples
#' df <- data.frame(A = 1:3,
#'                  B = rep(2, 3),
#'                  C = rep(3, 3))
#' uniform_df(.dat = df, .fishery = "purse_seine")
uniform_df <- function(.dat, .fishery) {
  proc_df <- make_empty_df(colnames = fetch_ideal_colnames(.fishery)) %>%
    dplyr::bind_rows(
      magrittr::set_colnames(.dat, str_rm_newline_code(colnames(.dat)))
     )

  drop <- !(colnames(proc_df) %in% fetch_ideal_colnames(.fishery))
  if (sum(drop > 0)) {
    warning('Removed column(s): "',
            paste0(colnames(proc_df)[drop], collapse = '", "'),
            '"')
  }

  dplyr::select(proc_df, fetch_ideal_colnames(.fishery))
}

#' Vectorized addition with na.rm
#'
#' \code{plus} adds together multiple vectors in element-wise fashion, so
#' \code{plus(a, b, c)} would be similar to \code{a + b +c}. Unlike \code{+},
#' \code{plus} takes an \code{na.rm} argument to handle NA behavior.
#' @inherit EDAWR::plus
#' @inheritParams EDAWR::plus
#' @param na.rm if \code{TRUE} (default) ignore NA values when to calculate
#' @examples
#' a <- c(NA, 2, 3)
#' b <- c(1, NA, 3)
#' c <- c(1, 2, NA)
#' plus(a, b, c)
#' plus(a, b, c, na.rm = FALSE)
plus <- function(..., na.rm = TRUE) { # nolint
  rowSums(as.data.frame(list(...)), na.rm = na.rm)
}

#' All value matching
#'
#' @param x vector or NULL: the values to be matched.
#' @param X vector or NULL: the values to be matched against.
#' @return TRUE or FALSE
#' @examples
#' letters[1:3] %all_in% letters[1:5]
#' letters[1:3] %all_in% letters[2:5]
`%all_in%` <- function(x, X) { # nolint
  all(x %in% X)
}

#' Classify is_functions
#'
#' @param type a string or a number. Must be one of 1 to 4, or
#'   \code{c("uniq_len", "uniq_val", "all_in", "sum")}; then, it calls
#'   \code{is_unique_length}, \code{is_unique_values},
#'   \code{is_all_uniq_vals_in}, and \code{is_sum}, respectively.
#' @param ... arguments of each called function.
#' @importFrom magrittr %>%
#' @return TRUE of FALSE
#' @examples
#' \dontrun{
#' post_isFunc(type = "uniq_len")
#' post_isFunc(type = 1)
#' }
post_isFunc <- function(type = 1, ...) {
  switch(type,
         "uniq_len" = is_unique_length(...),
         "uniq_val" = is_unique_values(...),
         "all_in" = is_all_uniq_vals_in(...),
         "sum" = is_sum(...))
}

# Helpers -------
make_empty_df <- function(colnames) {
  purrr::map_dfr(colnames, ~ tibble::tibble(!!.x := logical())) 
}
