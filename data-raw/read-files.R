devtools::load_all() # load lgbktestr

# needed packages
needs::needs(devtools, tidyverse, lubridate, readxl, tidyxl, unpivotr, zipangu,
             magrittr)

parent_dir <- "/Volumes/Extreme SSD/DATA/大中型旋網漁獲成績報告書"
year <- 2020
indir <- dir(parent_dir, pattern = paste0(year, ".+操業"), full.names = T)

infiles <- dir(file.path(indir, "入力済み"), full = T, pattern = "\\.xlsx?$")

## tracer (曳光弾, bullet) -------
file <- infiles[3]
xl <- readxl::read_excel(file, sheet = "整理番号")
colnames(xl)
colnames(xl) <-
  colnames(xl) %>%
  str_rm_newline_code()
xl
cat(paste(colnames(xl), sep = '", "', collapse = '", "'))
message(colnames(xl), "\n")

magrittr::set_colnames()

setNames(colnames(xl), remove_newline_code(x))

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
  purrr::map_dfr( ~tibble::tibble(!!.x := logical() ) )

# cells <- tidyxl::xlsx_cells(file)
# head(cells)
# unique(cells$sheet)
#
# table <-
#   cells %>%
#   dplyr::filter(sheet == "整理番号") %>%
#   dplyr::mutate(character = stringr::str_trim(character)) %>%
#   unpivotr::enhead(., direction = "S", name = "")
#
# stats::setNames(obj = LETTERS, nm = letters)
#
#
# ??unpivotr::`compass-directions`
#
# system.file("extdata", "purpose.xlsx", package = "unpivotr")
# original <- purpose$`NNW WNW`
# cells <- as_cells(original)
#
# row_headers <-
#   cells %>%
#   dplyr::filter(col <= 2, !is.na(chr)) %>% # Select all rows of headers at once
#   select(row, col, header = chr) %>%
#   split(.$col) # Return each row of headers in its own element of a list
# # row_headers
#
# col_headers <-
#   cells %>%
#   dplyr::filter(row <= 2, !is.na(chr)) %>%
#   select(row, col, header = chr) %>%
#   split(.$row)
# # col_headers
#
# data_cells <-
#   cells %>%
#   dplyr::filter(row >= 3, col >= 3, !is.na(chr)) %>%
#   mutate(value = as.integer(chr)) %>%
#   select(row, col, value)
# # head(data_cells)
#
# data_cells <-
#   data_cells %>%
#     enhead(col_headers[[1]], "NNW") %>%
#     enhead(col_headers[[2]], "N") %>%
#     enhead(row_headers[[1]], "WNW") %>%
#     enhead(row_headers[[2]], "W")
#
# NNW_WNW <- data_cells %>% arrange(row, col)
#
# cells <- as_cells(purpose$`NNE WSW`)
