#' Check up logbook file.
#'
#' @param file an input file
#' @param ... arguments succeeded to each method function
#' @return data.frame describing error and/or warning records
#' @examples
#' \dontrun{
#' check_logbook(file = "your/file.xlsx",
#'               sheet = "Sheet 1") # An argument for excel file
#' }
#' @export
check_logbook <- function(file, ...) {
  class(file) <- append(tools::file_ext(file), class(file))
  UseMethod("check_logbook", file)
}

#' @export
check_logbook.default <- function(file, ...) {
  ext <- paste0(".", tools::file_ext(file))
  if(ext == ".") ext <- "No extention"
  warn <- paste("\n check_logbook() can handle .csv, .xls, .xlsx files.\n",
                ext, "file can't be treated.")
  rlang::abort(warn)
}

#' @export
check_logbook.xls <- function(file, ...) {
  treat_excel(file, ...)
}

#' @export
check_logbook.xlsx <- function(file, ...) {
  treat_excel(file, ...)
}

#' @export
check_logbook.csv <- function(file, ...) {
  treat_csv(file, ...)
}


#' Update check list
#' 
#' @param fishery fishery name strings such as "purse_seine", "long_line"
#'  (snakle_case).
#' @param column column name strings contained in a target data.frame.
#' @param values values to be matched against.
#' @param isType Choose from "uniq_len", "uniq_val", "all_in", "sum" use for
#' \code{is_unique_values}, \code{is_unique_values}, \code{is_all_uniq_vals_in},
#' \code{is_sum}.
#' @importFrom tibble lst
#' @export
update_check_list <- function(fishery = c(),
                              column = c(), 
                              values = c(),
                              isType = c()) {
  up_check_list <-
    lst(fishery = lst("column" = column, "values" = values, "isType" = isType),
        check_list)
  # usethis::use_data(up_check_list, internal = F, overwrite = T)
}
