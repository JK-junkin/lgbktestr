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
    readxl::read_excel(file, sheet) %>% #, col_names = TRUE) %>%
    # tibble::rownames_to_column() %>%
    # dplyr::filter(!is.na(sheet)) %>%
    # tibble::column_to_rownames() %>%
    uniform_df()

  test1 <- try(test_length_identical(dat, column = "操業海域", u_len = 1L),
               silent = TRUE)
  if (class(test1) == "try-error") {
    out1 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "操業海域の混在",
                       Suggestion = "操業海域ごとに整理番号を振り直して下さい.")
  } else {
    out1 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  }

  test2 <- try(test_value_identical(dat, column = "操業年月日", u_value = 2020L),
               silent = TRUE)
  if (class(test2) == "try-error") {
    out2 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "操業年が違います",
                       Suggestion = paste0("操業年月日列の入力値に間違いが",
                                           "ないか確認してください."))
  } else {
    out2 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  }

  test3 <- try(test_value_equals(dat, column = "操業海域", u_values = 1:2),
               silent = TRUE)
  if (class(test3) == "try-error") {
    out3 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "対象 (1, 2) 外の操業海域が含まれています",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  } else {
    out3 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  }

  test4 <- try(test_value_identical(dat, column = "漁業種類コード", u_value = 13L),
               silent = TRUE)
  if (class(test4) == "try-error") {
    out4 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "対象 (13) 外の漁業種類が含まれています",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  } else {
    out4 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  }

  test5 <- try(test_value_equals(dat, column = "漁法コード", u_values = 251:252),
               silent = TRUE)
  if (class(test5) == "try-error") {
    out5 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "対象 (251, 252) 外の漁法コードが含まれています",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  } else {
    out5 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  }

  test6 <- try(test_sum(dat, total_col = "航海日数",
                        partial_cols = c("操業日数", "探索日数")),
               silent = TRUE)
  if (class(test6) == "try-error") {
    out6 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "航海日数 != 操業日数 + 探索日数",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  } else {
    out6 <- data.frame(File = file,
                       Sheet = sheet,
                       Column_name = NA_character_,
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  }

  suppressWarnings(dplyr::bind_rows(out1, out2, out3, out4, out5, out6))
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
