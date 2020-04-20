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
#' treat_csv(file = "your/file.csv",
#'           fishery = "purse seine",
#'           species = "not-tunas",
#'           dictionaries = list("files/at/", "your/local"))
#' }
treat_csv <- function(file, fishery = "purse_seine",
                      species = "not_tunas", dictionaries) {
  message("`treat_csv()` is under construction. Please wait until release.")
  # dat <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE)
}
