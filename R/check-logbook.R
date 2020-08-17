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
  ext <- tools::file_ext(file)
  if (ext %in% c("xls", "xlsx")) ext <- "excel"
  class(file) <- append(ext, class(file))
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

#' Check up logbook excel file.
#'
#' @param file an excel file
#' @param sheet an input sheet in the input file
#' @param fishery fishery which the logbook treats
#' @param ... arguments succeeded to each method function
#' @return data.frame describing error or warning records
#'
#' @examples
#' \dontrun{
#' treat_excel(file = "your/file.xlsx",
#'             sheet = "Sheet 1",
#'             fishery = "purse_seine"))
#' }
#' @export
check_logbook.excel <- function(file, sheet, fishery, ...) {
  sheets <- readxl::excel_sheets(path = file)
  
  if (missing(sheet)) {
    sheet <- 1
    sheet_name <- sheets[sheet]
    message("'sheet' is missing then 1st sheet '", sheet_name,
            "' is read by default." )
  } else if (is.numeric(sheet)) {
    sheet_name <- sheets[sheet]
  } else {
    sheet_name <- sheet
  }
  
  # This code is a hard coding for the business needs.
  if (missing(fishery)) {
    message("'fishery' is missing then 'purse_seine' is assigned automatically.")
    fishery <- "purse_seine"
  }
  
  dat <- readxl::read_excel(path = file, sheet = sheet_name) %>%
    uniform_df(.fishery = fishery)
  class(dat) <- append(fishery, class(dat))
  
  scan_contents(.dat = dat, ...,
                .file = file, .sheet = sheet_name, .fishery = fishery)
}

#' Check up logbook csv file.
#'
#' @param file an csv file
#' @param fishery fishery which the logbook treats
#' @param species species which the logbook treats
#' @param dictionaries a look-up table of vessel name and licence number in
#'   a specific year
#'
#' @return data.frame describing error or warning records
#'
#' @examples
#' \dontrun{
#' check_logbook.csv(file = "your/file.csv",
#'                   fishery = "purse seine",
#'                   species = "not-tunas",
#'                   dictionaries = list("files/at/", "your/local"))
#' }
check_logbook.csv <- function(file, fishery = "purse_seine",
                              species = "not_tunas", dictionaries) {
  message("CSV file can't be treated for now. Please wait until release.")
  # dat <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE)
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
