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

#' Genarater of ideal column names
#'
#' @param fishery fishery type of logbook
#' @examples
#' \dontrun{
#' fetch_ideal_colnames(fishery = "purse seine")
#' }
fetch_ideal_colnames <- function(fishery) {
  if (fishery == "purse seine") {
    c("整理番号", "操業次", "報告月", "漁業種類コード",
      "漁法コード", "県コード", "操業海域", "漁船一連番号",
      "船名", "漁船登録番号", "トン数", "トン数別階層コード",
      "航海数", "航海日数", "操業日数", "探索日数",
      "通常従業員数", "漁協コード", "許可種類", "根拠地", "メモ",
      "操業年月日", "操業月", "操業日", "大海区", "漁区",
      "まいわし(小中)", "まいわし(大)", "かたくち(小中)",
      "かたくち(大)", "うるめ(小中)", "うるめ(大)", "さば(小中)",
      "さば(大)", "まあじ(小中)", "まあじ(大)", "まるあじ",
      "むろあじ", "ぶり", "するめいか", "その他", "合計",
      "操業メモ(毎日の操業の備考欄の事項")
  }
}

#' Make a difference data frame
#'
#' @param df data.frame or derivative class (such as tibble) object
#' @param ideal_header
#'
#' @return
#' @export
#'
#' @examples
diff_colnames <- function(df, ideal_header) {

}


#' Uniform (Standardize) data frame.
#'
#' @param df input data frame to be processed
#' @param ideal_columns
#' @return A data.frame ncol reduced if excess columns exists.
#' @examples
#' df <- data.frame(整理番号 = 1:3,
#'                  操業次 = rep(2, 3),
#'                  報告年 = rep(2019, 3))
#' uniform_df(df)
#'  Warnings:
#'   Removed column(s): '報告年'
uniform_df <- function(df, fishery = "purse seine") {

  proc_df <- fetch_ideal_colnames(fishery = fishery) %>%
    purrr::map_dfr(~ tibble::tibble(!!.x := logical())) %>% # empty data.frame
    dplyr::bind_rows(
      magrittr::set_colnames(df, str_rm_newline_code(colnames(df)))
      )

  list <- list(
    rename = !(colnames(df) %in% colnames(proc_df)),
    drop = !(colnames(proc_df) %in% fetch_ideal_colnames(fishery = fishery)),
    add = !(fetch_ideal_colnames(fishery = fishery) %in% colnames(proc_df))
  )

  # drop_cols <- !(colnames(proc_df) %in% colnames(ideal_header))
  # add_cols <- !(colnames(ideal_header) %in% colnames(df))

  compare_df <- data.frame(
    before = colnames(df),
    after  = colnames(proc_df)
    )

  # do_exist_omit_cols <- length(colnames(proc_df)[drop_cols]) > 0
  # if (do_exist_omit_cols) {
  #   warning(
  #     paste0('Removed column(s): "', paste0(drop_cols, collapse = '", "'), '"')
  #   )}

  # do_exist_new_cols <- length(colnames(ideal_header)[add_cols]) > 0
  # if (do_exist_new_cols) {
    # message('Renamed or added column(s): \n',
    message('Changed columns: \n',
      paste0(
        capture.output(compare_df),
        collapse = "\n"
      )
    )}

  dplyr::select(proc_df, colnames(ideal_header))
}
