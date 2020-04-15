
# remotes::install_github("cran/rowr")
# needs::needs(lubridate, tidyxl, unpivotr, zipangu, rowr)

devtools::load_all() # Load source package on memory (!! skip package install)

# devtools::install()  # Wrapper function of `R CMD INSTALL`
# devtools::build()    # Create bandle pkg from source pkg `R CMD BUILD`
# library(lgbktestr)   # Cannot run only after devtools::load_all().
# devtools::reload()   # Reload.

## -----------------

needs(foreach, tidyverse)
devtools::load_all()

# データファイルのパス ---------
parent_dir <- "/Volumes/Extreme SSD/DATA/大中型旋網漁獲成績報告書"
year <- 2020
indir <- dir(parent_dir, pattern = paste0(year, ".+操業"), full.names = T)

infiles <- dir(file.path(indir, "入力済み"), full = T, pattern = "\\.xlsx?$")

# 読み込みテスト --------
file <- infiles[2]
xl <- readxl::read_excel(file, sheet = "整理番号")
head(xl); colnames(xl)
t_xl <- lgbktestr::uniform_df(xl)

# 動作テスト
out <- foreach(i = seq_along(infiles), .combine = "rbind") %do% {
  test_excel_logbook(file = infiles[i], sheet = "整理番号") %>%
    dplyr::filter(Error_type != "OK (No errors)") %>%
    dplyr::mutate(File = stringr::str_extract(File, "(?<=入力済み\\/).*\\.xlsx$"))
}

formattable::formattable(out, cex = 10)








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
