#' Detect row(s) which is/are suspected
#'
#' @param .d a data.frame
#' @return a list of 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' scan_by_row(df)
#' }
#' @export
scan_by_row <- function(.d) {
  readxl::read_excel(path = "tests/testthat/excel/PS-dummy-1.xlsx", sheet = 1) %>%
    uniform_df(.fishery = "purse_seine") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter_at(2, dplyr::all_vars(!is.na(.))) %>%
    tibble::column_to_rownames() %>%
    is_sum(dat = ., column = check_list$purse_seine$column[6],
           rlang::quo(parse(text = check_list$purse_seine$values[6]))
           )
    post_isFunc(type = 4,
                dat = .,
                column = check_list$purse_seine$column[6],
                rlang::quo_get_env(rlang::quo(check_list$purse_seine$values[6]))
                # rlang::expr_interp(check_list$purse_seine$values[6], "R_GlobalEnv")]
                )
                # rlang::expr_interp(rlang::parse_quosure(check_list$purse_seine$values[6])))
    # hoge_filter() %>%
    print
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
