## code to prepare `DATASET` dataset goes here
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

out_df_colnames <-
  c("File", "Sheet", "Column_name", "Row_name", "Error_type", "Suggestion") 

usethis::use_data(list_colnames, out_df_colnames, internal = T, overwrite = T)

check_list <-
  tibble::lst(
    "purse_seine" = tibble::lst(
      "column" = c("操業海域", "操業海域", "操業年月日", "漁業種類コード",
                   "漁法コード", "航海日数", "合計"),
      "values" = c("1L", "1:2", "2020", "13", "251:252", "操業日数:探索日数",
                   "`まいわし(小中)`:`その他`"),
      "isType" = c("uniq_len", "all_in", "uniq_val", "uniq_val", "all_in",
                   "sum", "sum")
      )
    )
usethis::use_data(check_list, internal = F, overwrite = T)
