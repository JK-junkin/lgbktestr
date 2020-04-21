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
treat_excel <- function(file, sheet, fishery, ...) {
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
