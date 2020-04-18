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
#' treat_excel(file = "your/file.csv",
#'             fishery = "purse seine",
#'             species = "not-tunas",
#'             dictionaries = list("files/at/", "your/local"))
#' }
treat_excel <- function(file, fishery = "purse seine",
                        species = "not-tunas", dictionaries) {
}