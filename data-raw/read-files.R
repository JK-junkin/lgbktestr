
# remotes::install_github("cran/rowr")
# needs::needs(lubridate, tidyxl, unpivotr, zipangu, rowr)

devtools::load_all() # Load source package on memory (!! skip package install)

# devtools::install()  # Wrapper function of `R CMD INSTALL`
# devtools::build()    # Create bandle pkg from source pkg `R CMD BUILD`
# library(lgbktestr)   # Cannot run only after devtools::load_all().
# devtools::reload()   # Reload.

## -----------------

needs(foreach, tidyverse)

# データファイルのパス ---------
parent_dir <- "/Volumes/ExtremeSSD/DATA/大中型旋網漁獲成績報告書"
# indir <- dir(parent_dir, pattern = paste0(2020, ".+操業"), full.names = T)
# (infiles <- dir(file.path(indir, "01.入力済みファイル"),
#                 full = T, pattern = "\\.xlsx?$"))

# 読み込みテスト --------
# file <- infiles[200]
# 
# xl <- readxl::read_excel(file, sheet = "整理番号")
# head(xl); colnames(xl)

# 動作テスト ----------
out <- output_checklist(filedir = parent_dir,
                        subdir  = "01.入力済みファイル",
                        pattern = "2020.+操業")
formattable::formattable(out, cex = 10)

# out <- foreach(i = infiles, .combine = "rbind") %do% {
#     cat("Check", basename(i), "\n")
#     suppressMessages(
#         check_logbook(file = i, sheet = "整理番号") %>%
#             dplyr::filter(!is.na(Error))
#     )         
# }
# aut <- out %>% dplyr::arrange(Column, File)
# aut
# out$Rows

# (t_xl <- uniform_df(xl, fishery = "purse_seine"))
# 
# check_logbook(file = file)
# treat_excel(file = file)
# treat_excel(file = file, sheet = "整理番号") # 継承されていた
# treat_excel(file = file, sheet = "整理番号", fishery = "pole_and_line") # uniform_dfは機能


## -----------------
## よりやっかいなエクセルデータに対処しなければならなくなったら
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
