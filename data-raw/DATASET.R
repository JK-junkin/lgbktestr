## code to prepare `DATASET` dataset goes here

# Code-sets of fresco ----------------------------------------------------------
## (今後変わる可能性もあるので留意)
code_list <- "~/git/lgbktestr/data-raw/frescoコード一覧_20190722版.xlsx"
sheets <- readxl::excel_sheets(code_list)
codes <- foreach::foreach(i = seq_along(sheets)) %do% {
  cat(i, sheets[i], "\n")
  readxl::read_excel(code_list, sheet = sheets[i], skip = 1, col_names = T)
}
names(codes) <- sheets
# codes
usethis::use_data(codes, internal = FALSE, overwrite = TRUE)
rm(i, sheets, code_list, codes)


## Column names for Purse-seine fishery ----------------------------------------
list_colnames <- 
  tibble::lst(
    "purse_seine" = c("整理番号", "操業次", "報告月", "漁業種類コード",
                      "漁法コード", "県コード", "操業海域", "漁船一連番号",
                      "船名", "漁船登録番号", "トン数", "トン数別階層コード",
                      "航海数", "航海日数", "操業日数", "探索日数",
                      "通常従業員数", "漁協コード", "許可種類", "根拠地",
                      "メモ", "操業年月日", "操業月", "操業日", "大海区",
                      "漁区", "まいわし(小中)", "まいわし(大)",
                      "かたくち(小中)", "かたくち(大)", "うるめ(小中)",
                      "うるめ(大)", "さば(小中)", "さば(大)", "まあじ(小中)",
                      "まあじ(大)", "まるあじ", "むろあじ", "ぶり",
                      "するめいか", "その他", "合計",
                      "操業メモ(毎日の操業の備考欄の事項")
  )

usethis::use_data(list_colnames, internal = TRUE, overwrite = TRUE)
