# type... oogaki, kogaki
read_zipcode <- function(path, type = c("kogaki")) {
  
  address_level <- c("prefecture", "city", "street")
  
  dplyr::mutate_if(readr::read_csv(path, 
                                     locale = readr::locale(encoding = "cp932"),
                                     col_names = c("jis_code", "old_zip_code", "zip_code", 
                                                   paste0(address_level, "_yomi"),
                                                   address_level,
                                                   "is_street_duplicate", # 一町域が二以上の郵便番号で表される場合の表示
                                                   "is_banchi", # 小字毎に番地が起番されている町域の表示
                                                   "is_cyoumoku", # 丁目を有する町域の場合の表示 
                                                   "is_zipcode_duplicate", # 一つの郵便番号で二以上の町域を表す場合の表示
                                                   "status", # 更新の表示 
                                                   "type" # 変更理由
                                                   ),
                                     col_types = "ccccccccclllddd") ,
                     is.character,
                     stringi::stri_trans_general,
                     id = "nfkc")
}

df_type_class <- data.frame(
  type = seq.int(0, 6),
  category = c("変更なし", "市政・区政・町政・分区・政令指定都市施行", "住居表示の実施",
               "区画整理", "郵便区調整等", "訂正", "廃止"),
  stringsAsFactors = FALSE
)

zipcode_spacer <- function(x, remove = FALSE) {
  
  if (rlang::is_false(remove)) {
    paste0(
      stringr::str_sub(x, 1, 3),
      "-",
      stringr::str_sub(x, 4, 7))
  } else {
    stringr::str_remove_all(x, "-")
  }
}
