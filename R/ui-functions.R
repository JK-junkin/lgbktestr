#' Read an input excel file (logbook) and test it.
#'
#' @param file an input excel file
#' @param sheet an input sheet in the input file
#' @param fishery fishery that the logbook treats
#' @param species species that the logbook includes
#' @param dictionary a look-up table of vessel name and licence number in
#'   a specific year
#'
#' @return data.frame describing error or warning records
#'
#' @examples
#' \dontrun{
#' test_excel_logbooks(file = "your/file.xlsx",
#'                     sheet = "Sheet 1",
#'                     fishery = "purse seine",
#'                     species = "not-tunas",
#'                     dictionary = "vessel-license-2020.csv")
#' }
#'               
#' @export
test_excel_logbook <- function(file, sheet,
                               fishery = "purse seine",
                               species = "not-tunas",
                               dictionary = "vessel-license-2020.csv") {
  dat <-
    readxl::read_excel(file, sheet, col_names = TRUE) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(!is.na(sheet)) %>%
    tibble::column_to_rownames()

}

#' Read input files in a directory (folder) and test them all.
#'
#' @param dir an input directory which has one or more input files
#' @param sheet an input sheet name in common with all input excel files
#' @param fishery fishery that the logbook treats
#' @param species species that the logbook includes
#' @param dictionary a look-up table of vessel name and licence number in
#'   a specific year
#'
#' @return data.frame describing error or warning records
#'
#' @examples
#' \dontrun{
#' test_excel_logbooks(dir = "your/directory",
#'                     sheet = "Sheet 1",
#'                     fishery = "purse seine",
#'                     species = "not-tunas",
#'                     dictionary = "vessel-license-2020.csv")
#' }
#'
#' #' @export
test_excel_logbooks <- function(dir, sheet,
                                fishery = "purse seine",
                                species = "not-tunas",
                                dictionary = "vessel-license-2020.csv") {

}
