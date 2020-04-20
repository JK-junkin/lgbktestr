
scan_contents <- function(dat, fishery, species, dictionaries) {
  if (!test_unique_length(dat, column = "操業海域", u_len = 1L)) {
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
  
  if (!test_unique_values(dat, column = "操業年月日", u_vals = "2020")) {
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
  
  if (!test_all_uniq_vals_in(dat, column = "操業海域", u_vals = 1:2)) {
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
  
  if (!test_unique_values(dat, column = "漁業種類コード", u_vals = "13")) {
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
  
  if (!test_all_uniq_vals_in(dat, column = "漁法コード", u_vals = 251:252)) {
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
  
  if (!test_sum(dat, total_col = "航海日数", "操業日数", "探索日数")) {
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
