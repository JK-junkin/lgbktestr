#' Error handling of input file.
#'
#' @param dat an input data uniformed
#' @param sheet an input sheet name
#' @param fishery fishery name
#' @param species species name
#' @param dictionaries a look-up table of vessel name and licence number in
#'   a specific year
#'
#' @return tibble::tibble describing error or warning records
#'
#' @examples
#' \dontrun{
#' scan_contents(file = "your/file.xlsx",
#'               sheet = "Sheet 1",
#'               fishery = "purse_seine",
#'               species = "not_tunas",
#'               dictionaries = list("files/at/", "your/local"))
#' }
#' @export
scan_contents <- function(dat, fishery, species, dictionaries = NULL, sheet, file) {
    if (fishery == "purse_seine") {
        dat <- dat %>% dplyr::filter(!is.na(整理番号))

        # 操業海域は初めから1つしか入力できない仕様. これは無意味
        # むしろ, 漁区と操業海域が一致しているかを判定した方が良い
        # あるいは, 操業海域が特定の値かどうか
        if (!test_unique_length(dat, column = "操業海域", u_len = 1L)) {
            out1 <- tibble::tibble(Column = "操業海域",
                                   Row = NA_character_,
                                   Error = "操業海域の混在",
                                   Suggestion = paste0("この列の入力間違いがないか確認し,",
                                                       "漁区が間違いないようなら操業海域ごとに",
                                                       "ファイルを分け, 整理番号を振り直して下さい."),
                                   File = basename(file),
                                   Sheet = sheet)
        } else {
            out1 <- tibble::tibble(Column = "操業海域",
                                   Row = NA_character_,
                                   Error = NA_character_,
                                   Suggestion = NA_character_,
                                   File = basename(file),
                                   Sheet = sheet)
        }

        if (!test_unique_values(dat, column = "操業年月日", u_vals = "2020")) {
        out2 <- tibble::tibble(File = basename(file),
                           Sheet = sheet,
                           Column = NA_character_,
                           Row = NA_character_,
                           Error = "操業年が違います",
                           Suggestion = paste0("操業年月日列の入力値に間違いが",
                                               "ないか確認してください."))
        } else {
        out2 <- tibble::tibble(File = basename(file),
                           Sheet = sheet,
                           Column = NA_character_,
                           Row = NA_character_,
                           Error = "OK (No errors)",
                           Suggestion = NA_character_)
        }

#         if (!test_all_uniq_vals_in(dat, column = "操業海域", u_vals = 1:2)) {
#         out3 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "対象 (1, 2) 外の操業海域が含まれています",
#                            Suggestion = paste0("入力値に間違いがないか",
#                                                "原票と照合してください."))
#         } else {
#         out3 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }
# 
#         if (!test_unique_values(dat, column = "漁業種類コード", u_vals = "13")) {
#         out4 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "対象 (13) 外の漁業種類が含まれています",
#                            Suggestion = paste0("入力値に間違いがないか",
#                                                "原票と照合してください."))
#         } else {
#         out4 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }
# 
#         if (!test_all_uniq_vals_in(dat, column = "漁法コード", u_vals = 251:252)) {
#         out5 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "対象 (251, 252) 外の漁法コードが含まれています",
#                            Suggestion = paste0("入力値に間違いがないか",
#                                                "原票と照合してください."))
#         } else {
#         out5 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }
# 
#         if (!test_sum(dat, total_col = "航海日数", "操業日数", "探索日数")) {
#         out6 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "航海日数 != 操業日数 + 探索日数",
#                            Suggestion = paste0("入力値に間違いがないか",
#                                                "原票と照合してください."))
#         } else {
#         out6 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Row = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }

#         suppressWarnings(dplyr::bind_rows(out1, out2, out3, out4, out5, out6))
        suppressWarnings(dplyr::bind_rows(out1)) #, out2, out3, out4, out5, out6))
    } else {
        stop("巻き網以外はまだ対応していません.")
    }
}
