#' Inspect data contents
#'
#' Scan input data (\code{data.frame}-like object) and return suggestions that
#' records might be problematic.
#'
#' @param .dat A data.frame or tibble standardized by \code{uniform_df()}.
#' @param species Species such as "not_tuna".
#' @param dictionaries Lookup tables referred to.
#' @param .file File name.
#' @param .sheet Sheet name when excel file read.
#' @param .fishery Fishery such as "purse_seine".
#' @return A data.frame.
#' @importFrom magrittr %>%
#' @importFrom rlang parse_expr quo
#' @importFrom foreach %do% foreach
#' @examples
#' \dontrun{
#' scan_contents()
#' }
#' @importFrom magrittr %>%
#' @export
scan_contents <- function(.dat, species, dictionaries = NULL,
                          .file, .sheet, .fishery) {
  UseMethod("scan_contents")
}

#' @export
scan_contents.default <- function(.dat, species, dictionaries = NULL,
                                  .file, .sheet, .fishery) {
  stop("Fishery '", fishery, "' is not supported now.\n  ",
       "We are able to treat { ", names(list_colnames), " } for the present.")
}

#' @importFrom magrittr %>%
#' @importFrom rlang parse_expr quo
#' @importFrom foreach %do% foreach
#' @export
scan_contents.purse_seine <- function(.dat, species, dictionaries = NULL,
                                      .file, .sheet, .fishery) {
  if (missing(species)) {
    species <- "not_tunas"
    message("'species' is missing then 'not_tunas' is assigned automatically.")
  }

  # i <- 7

  d <- .dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter_at(2, dplyr::all_vars(!is.na(.))) %>%
    tibble::column_to_rownames() %>% print
    post_isFunc(type = check_list$purse_seine$isType[6],
                dat = d,
                column = check_list$purse_seine$column[6],
                !!rlang::quo(!!rlang::parse_expr(check_list$purse_seine$values[6])))

  # needs <- logical(length = 7)
  # # for(i in 1:7) {
  #   needs[i] <-
  #     post_isFunc(type = check_list$purse_seine$isType[i],
  #                 dat = d,
  #                 column = check_list$purse_seine$column[i],
  #                 !!quo(!!parse_expr(check_list$purse_seine$values[i])))
  # }

  return(d)
  # post_isFunc(type = check_list$purse_seine$isType,
  #             dat = d,
  #             column = check_list$purse_seine$column,
  #             !!quo(!!parse_expr(check_list$purse_seine$values)))

  # if (!needs) {
  #   out_empty <- make_empty_df(colnames = out_df_colnames)
  # } else {
  #   out_break <- make_empty_df(colnames = out_df_colnames) %>%
  #     dplyr::bind_rows()
  # }
  # suppressWarnings(dplyr::bind_rows(out1, out2, out3, out4, out5, out6))
}
