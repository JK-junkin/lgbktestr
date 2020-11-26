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

        # (1) 操業海域 (大海区) が正しいか ----------
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

        # (2) 漁区が正しいか ----------
        # まき網漁績:
        #   操業海域(大海区)は初めから1つしか入力できない仕様.
        #   漁区と操業海域(大海区)が一致しているかを判定.
        if (is.null(dictionaries)) {
            gyoku <- codes$農林漁区 %>%
                dplyr::filter(大海区 %in% 1:2) %>%
                dplyr::pull(漁区)
        } else {
            gyoku <- dictionaries
        }

        if (!test_all_uniq_vals_in(dat, column = "漁区", u_vals = gyoku)) {
            target <- dat %>%
                labdsv::defactorize() %>%
                dplyr::pull(漁区)

            kaiku <- unique(dat$大海区)
            
            out2 <- tibble::tibble(Column = "漁区",
                                   Rows   = paste(which(!(target %in% gyoku)), collapse = ", "),
                                   Error  = paste("大海区", kaiku, "にはない漁区が含まれる"),
                                   Suggestion = paste0("1: 漁区に入力間違いがないか確認\n",
                                                       "2: 漁区が正しいなら大海区ごとに",
                                                       "ファイルを分けて, 整理番号を振り直し"),
                                   File   = basename(file),
                                   Sheet  = sheet)
        } else {
            out2 <- tibble::tibble(Column     = "漁区",
                                   Rows       = NA_character_,
                                   Error      = NA_character_,
                                   Suggestion = NA_character_,
                                   File       = basename(file),
                                   Sheet      = sheet)
        }

        # (3) 航海日数が正しいか ----------
#         if (!test_sum(dat, total_col = "航海日数", "操業日数", "探索日数")) {
#             out3 <- tibble::tibble(Column = "航海日数",
#                                    Rows   = NA_character_,
#                                    Error  = "航海日数 != 操業日数 + 探索日数",
#                                    Suggestion = paste0("操業日数の正確さを優先します"),
#                                    File   = basename(file),
#                                    Sheet  = sheet)
#         } else {
#             out3 <- tibble::tibble(Column     = "航海日数",
#                                    Rows       = NA_character_,
#                                    Error      = NA_character_,
#                                    Suggestion = NA_character_,
#                                    File       = basename(file),
#                                    Sheet      = sheet)
#         }

        # (4) 操業日数が正しいか ----------
        # 操業日のユニークな数が操業日数と等しいという判定
        noper <- dat %>%
            dplyr::pull(操業日数) %>%
            unique() %>%
            as.integer()

        # noper が NA (datにレコードがない) のとき
        if (length(is.na(noper)) != 0 & !test_unique_length(dat, column = "操業日", u_len = noper)) {
            target <- dat %>%
                labdsv::defactorize() %>%
                dplyr::pull(操業日) %>%
                stats::na.omit() %>%
                unique() %>%
                length()

            out4 <- tibble::tibble(Column = "操業日数, 操業日",
                                   Rows   = NA_character_,
                                   Error  = paste("操業日数", noper, "がunique(操業日)の数", target, "と不一致"),
                                   Suggestion = paste0("1: 操業日の入力間違いを確認\n",
                                                       "2: 操業日情報を信頼して操業日数を修正"),
                                   File   = basename(file),
                                   Sheet  = sheet)
        } else {
            out4 <- tibble::tibble(Column     = "操業日数, 操業日",
                                   Rows       = NA_character_,
                                   Error      = NA_character_,
                                   Suggestion = NA_character_,
                                   File       = basename(file),
                                   Sheet      = sheet)
        }

        # (5) 操業年月日が対象としている年と等しいか ----------
#         if (!test_unique_values(dat, column = "操業年月日", u_vals = "2020")) {
#         out2 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Rows = NA_character_,
#                            Error = "操業年が違います",
#                            Suggestion = paste0("操業年月日列の入力値に間違いが",
#                                                "ないか確認してください."))
#         } else {
#         out2 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Rows = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }

 
        # (4) 漁業種類コードが正しいか ----------
#         if (!test_unique_values(dat, column = "漁業種類コード", u_vals = "13")) {
#         out4 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Rows = NA_character_,
#                            Error = "対象 (13) 外の漁業種類が含まれています",
#                            Suggestion = paste0("入力値に間違いがないか",
#                                                "原票と照合してください."))
#         } else {
#         out4 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Rows = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }
  
        # (5) 漁法コードが正しいか ----------
#         if (!test_all_uniq_vals_in(dat, column = "漁法コード", u_vals = 251:252)) {
#         out5 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Rows = NA_character_,
#                            Error = "対象 (251, 252) 外の漁法コードが含まれています",
#                            Suggestion = paste0("入力値に間違いがないか",
#                                                "原票と照合してください."))
#         } else {
#         out5 <- tibble::tibble(File = basename(file),
#                            Sheet = sheet,
#                            Column = NA_character_,
#                            Rows = NA_character_,
#                            Error = "OK (No errors)",
#                            Suggestion = NA_character_)
#         }
 

#         suppressWarnings(dplyr::bind_rows(out1, out2, out3, out4, out5, out6))
        suppressWarnings(dplyr::bind_rows(out2, out4)) #, out3, out1, out5, out6))
    } else {
        stop("------ このバージョンでは青物大中型まき網漁績以外は未対応 ------")
    }
}
