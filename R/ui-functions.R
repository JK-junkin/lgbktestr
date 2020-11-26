#' Check up logbook file.
#'
#' @param file an input file
#' @param ... arguments succeeded to each method function
#' @return data.frame describing error or warning records
#'
#' @examples
#' \dontrun{
#' check_logbook(file = "your/file.xlsx",
#'               sheet = "Sheet 1") # argument for excel file
#' }
#'
#' @export
check_logbook <- function(file, ...) {
  class(file) <- append(class(file), tools::file_ext(file))
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
  treat_excel(file, sheet = NULL,
              fishery = NULL, species = NULL,
              dictionaries = NULL)
}

#' @export
check_logbook.xlsx <- function(file, ...) {
  treat_excel(file, sheet = NULL,
              fishery = NULL, species = NULL,
              dictionaries = NULL)
}

#' @export
check_logbook.csv <- function(file, ...) {
  treat_csv(file, ...)
}

#' @export
output_checklist <- function(filedir, subdir = "", pattern = "") {
    indir <- dir(filedir, pattern, full.names = TRUE)
    infiles <- dir(file.path(indir, subdir), full.names = TRUE,
                   pattern = "\\.xlsx?$")

    out <- foreach(i = infiles, .combine = "rbind") %do% {
        cat("Check", basename(i), "\n")
        suppressMessages(
            check_logbook(file = i, sheet = "整理番号") %>%
                dplyr::filter(!is.na(Error))
        )
    }
    out %>% dplyr::arrange(Column, File)
}
