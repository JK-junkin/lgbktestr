scan_contents <- function(.dat, species, dictionaries = NULL,
                          .file, .sheet, .fishery) {
  UseMethod("scan_contents")
}

scan_contents.default <- function(.dat, species, dictionaries = NULL,
                                  .file, .sheet, .fishery) {
  stop("Fishery '", fishery, "' is not supported now.\n  ",
       "We are able to treat { ", names(list_colnames), " } for the present.")
}

scan_contents.purse_seine <- function(.dat, species, dictionaries = NULL,
                                      .file, .sheet, .fishery) {
  if (missing(species)) {
    species <- "not_tunas"
    message("'species' is missing then 'not_tunas' is assigned automatically.")
  }

  # This is a knowledge of business
  .dat <- .dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(!is.na(整理番号)) %>%
    tibble::column_to_rownames()

  return(tibble::as_tibble(.dat))

  if (test_unique_length(.dat, column = "操業海域", u_len = 1)) {
    out1 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "操業海域",
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  } else {
    out1 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "操業海域",
                       Row_name = NA_character_,
                       Error_type = "操業海域の混在",
                       Suggestion = "操業海域ごとに整理番号を振り直して下さい.")
  }

  if (test_unique_values(.dat, column = "操業年月日", u_vals = "2020")) {
    out2 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "操業年月日",
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  } else {
    out2 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "操業年月日",
                       Row_name = NA_character_,
                       Error_type = "操業年が違います",
                       Suggestion = paste0("操業年月日列の入力値に間違いが",
                                           "ないか確認してください."))
  }

  if (test_all_uniq_vals_in(.dat, column = "操業海域", u_vals = 1:2)) {
    out3 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "操業海域",
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  } else {
    out3 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "操業海域",
                       Row_name = NA_character_,
                       Error_type = "対象 (1, 2) 外の操業海域が含まれています",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  }

  if (test_unique_values(.dat, column = "漁業種類コード", u_vals = 13)) {
    out4 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "漁業種類コード",
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  } else {
    out4 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "漁業種類コード",
                       Row_name = NA_character_,
                       Error_type = "対象 (13) 外の漁業種類が含まれています",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  }

  if (test_all_uniq_vals_in(.dat, column = "漁法コード", u_vals = 251:252)) {
    out5 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "漁法コード",
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  } else {
    out5 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = "漁法コード",
                       Row_name = NA_character_,
                       Error_type = "対象 (251, 252) 外の漁法コードが含まれています",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  }

  if (test_sum(.dat, total_col = "航海日数", "操業日数", "探索日数")) {
    out6 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = paste("航海日数", "操業日数", "探索日数",
                                           collapse = ", "),
                       Row_name = NA_character_,
                       Error_type = "OK (No errors)",
                       Suggestion = NA_character_)
  } else {
    out6 <- data.frame(File = .file,
                       Sheet = .sheet,
                       Column_name = paste("航海日数", "操業日数", "探索日数",
                                           collapse = ", "),
                       Row_name = NA_character_,
                       Error_type = "航海日数 != 操業日数 + 探索日数",
                       Suggestion = paste0("入力値に間違いがないか",
                                           "原票と照合してください."))
  }

  suppressWarnings(dplyr::bind_rows(out1, out2, out3, out4, out5, out6))
}
