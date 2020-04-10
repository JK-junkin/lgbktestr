#' Remove all newline codes in any strings
#'
#' @param vector vector to be processed
#' @return Cleaned up string(s)
#' @examples
#' x <- c("Main:\rSub", "Main2:\nSub", "Main3:\r\nSub")
#' str_rm_newline_code(x)
#'
str_rm_newline_code <- function(vector) {
  vector %>%
    stringr::str_remove_all("\\r") %>%
    stringr::str_remove_all("\\n")
}

#' Uniform (Standardize) data frame.
#'
#' @param df input data frame to be processed
#' @return A data.frame ncol reduced if excess columns exists.
#' @examples
#' df <- data.frame(整理番号 = 1:3,
#'                  操業次 = rep(2, 3),
#'                  報告年 = rep(2019, 3))
#' uniform_df(df)
#'  Warnings:
#'   Removed column(s): '報告年'
uniform_df <- function(df) {
  ideal_columns <- c("整理番号", "操業次", "報告月", "漁業種類コード",
                     "漁法コード", "県コード", "操業海域", "漁船一連番号",
                     "船名", "漁船登録番号", "トン数", "トン数別階層コード",
                     "航海数", "航海日数", "操業日数", "探索日数",
                     "通常従業員数", "漁協コード", "許可種類", "根拠地", "メモ",
                     "操業年月日", "操業月", "操業日", "大海区", "漁区",
                     "まいわし(小中)", "まいわし(大)", "かたくち(小中)",
                     "かたくち(大)", "うるめ(小中)", "うるめ(大)", "さば(小中)",
                     "さば(大)", "まあじ(小中)", "まあじ(大)", "まるあじ",
                     "むろあじ", "ぶり", "するめいか", "その他", "合計",
                     "操業メモ(毎日の操業の備考欄の事項") %>%
    purrr::map_dfr(~ tibble::tibble(!!.x := logical()))

  processed <- df %>%
    # magrittr::set_colnames(stringr::str_to_title(colnames(df))) %>%
    dplyr::bind_rows(ideal_columns)

  unknown_column <-
    colnames(processed)[!(colnames(processed) %in% colnames(ideal_columns))]

  do_exist_unknown_column <- length(unknown_column) > 0

  if (do_exist_unknown_column)
    warning(
      paste0("Removed column(s): '", paste(unknown_column, collapse = ", '"))
      )

  processed %>%
    dplyr::select(., colnames(ideal_columns))
}
