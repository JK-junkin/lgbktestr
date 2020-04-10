#' Read an input excel file (logbook) and test it.
#'
#' @param file input file
#' @param fishery fishery that the logbook treats
#' @param species species that the logbook includes
#' @param dictionary a look-up table of vessel name and licence number in a specific year
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {
#' lgbktestr::test_logbooks(file = "your/file.xlsx",
#'                          fishery = "purse seine",
#'                          species = "not-tunas",
#'                          dictionary = "vessel-license-2020.csv")
#' }
test_logbook <- function(file, fishery, species, dictionary) {

}

#' Read input files in a directory (folder) and test them all.
#'
#' @param dir an input directory which has one or more input files
#' @param fishery fishery that the logbook treats
#' @param species species that the logbook includes
#' @param dictionary a look-up table of vessel name and licence number in a specific year
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {
#' test_logbooks(dir = "your/directory",
#'               fishery = "purse seine",
#'               species = "not-tunas",
#'               dictionary = "vessel-license-2020.csv")
#' }
test_logbooks <- function(dir, fishery, species, dictionary) {

}
