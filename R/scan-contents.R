#' Inspect and suggest changes contents of input data
#'
#' @param .dat a data.frame or tibble standardized by \code{uniform_df()}.
#' @param species species such as "not_tuna"
#' @param dictionaries lookup tables referred to.
#' @param .file file name
#' @param .sheet sheet name when excel file read
#' @param .fishery fishery such as "purse_seine"
#' @importFrom magrittr %>%
#' @return a data.frame
#' @examples
#' \dontrun{
#' scan_contents()
#' }
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

#' @export
scan_contents.purse_seine <- function(.dat, species, dictionaries = NULL,
                                      .file, .sheet, .fishery) {
  if (missing(species)) {
    species <- "not_tunas"
    message("'species' is missing then 'not_tunas' is assigned automatically.")
  }

  d <- .dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(!is.na(整理番号)) %>% # This is a business knowledge
    tibble::column_to_rownames()

  return(tibble::as_tibble(.dat))

  if (post_isFunc(type = check_list$purse_seine$isType[[1]], 
                  dat = d,
                  column = check_list$purse_seine$column[[1]],
                  u_len = check_list$purse_seine$values[[1]])) {
    out_empty <- make_empty_df(colnames = out_df_colnames)
  } else {
    out_break <- make_empty_df(colnames = out_df_colnames) %>%
      dplyr::bind_rows()
  }
  # suppressWarnings(dplyr::bind_rows(out1, out2, out3, out4, out5, out6))
}
