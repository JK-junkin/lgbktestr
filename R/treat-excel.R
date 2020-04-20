#' Check up logbook excel file.
#'
#' @param file an excel file
#' @param sheet an input sheet in the input file
#' @param fishery fishery which the logbook treats
#' @param species species which the logbook treats
#' @param dictionaries a look-up table of vessel name and licence number in
#'   a specific year
#'
#' @return data.frame describing error or warning records
#'
#' @examples
#' \dontrun{
#' treat_excel(file = "your/file.xlsx",
#'             sheet = "Sheet 1",
#'             fishery = "purse_seine",
#'             species = "not_tunas",
#'             dictionaries = list("files/at/", "your/local"))
#' }
#' @export
treat_excel <- function(file, sheet = NULL,
                        fishery = NULL, species = NULL,
                        dictionaries = NULL) {
  if (is.null(fishery)) {
    message("'fishery' is NULL then 'purse_seine' is assigned automatically.")
    fishery <- "purse_seine"
  }

  if (is.null(species)) {
    message("'species' is NULL then 'not_tunas' is assigned automatically.")
    fishery <- "not_tunas"
  }

  if (is.null(sheet)) {
    if (fishery == "purse_seine" && species == "not_tunas") sheet <- 2
    else if (length(readxl::excel_sheets(file)) < 2) sheet <- 1
    else sheet
  }

  dat <-
    readxl::read_excel(path = file, sheet, col_types = "text") %>%
    # tibble::rownames_to_column() %>%
    # dplyr::filter(!is.na(sheet)) %>%
    # tibble::column_to_rownames() %>%
    uniform_df()

  scan_contents(dat, fishery, species, dictionaries)
}
