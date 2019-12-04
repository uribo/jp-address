is_zip <- function(x) {
  # 3、5桁はとりあえず無視
  checked <- stringr::str_detect(x, "^([0-9]{3}-[0-9]{4}|[0-9]{3}[0-9]{4})$")
  if (rlang::is_false(checked))
    rlang::inform("7\u6841\u306e\u6570\u5024\u3067\u306f\u3042\u308a\u307e\u305b\u3093")
  checked
}

# ref) https://www.post.japanpost.jp/zipcode/dl/jigyosyo/readme.html
read_zipcode_jigyosyo <- function(path) {
  address_level <- c("prefecture", "city", "street")
  dplyr::mutate_if(
    readr::read_csv(path, 
                    locale = readr::locale(encoding = "cp932"),
                    col_names = c("jis_code", "name_kana", "name",
                                  address_level,
                                  "street_sub", # 小字名、丁目、番地等 
                                  "jigyosyo_identifier", # 大口事業所個別番号
                                  "old_zip_code",
                                  "grouped", # 取扱局
                                  "individual_id", # 個別番号の種別の表示 0...大口事業所, 1... 私書箱
                                  "multiple_types", # 複数番号の有無 0... 複数番号なし, 1... 複数番号を設定している場合の個別番号の1, 2... 複数番号を設定している場合の個別番号の2, 3... 複数番号を設定している場合の個別番号の3
                                  # 一つの事業所が同一種別の個別番号を複数持つ場合に複数番号を設定しているもの
                                  # 一つの事業所で大口事業所、私書箱の個別番号をそれぞれ一つづつ設定している場合は「0」
                                  "update_type" #修正コード 0... 修正なし, 1... 新規追加, 2... 廃止
                    ),
                    col_types = "cccccccccciii"),
    is.character,
    stringi::stri_trans_general,
    id = "nfkc")
}
# ref) https://www.post.japanpost.jp/zipcode/dl/readme.html
read_zipcode <- function(path, type = c("oogaki", "kogaki", "roman")) {
  rlang::arg_match(type)
  address_level <- c("prefecture", "city", "street")
  col_vars <- 
    list(yomi = c("jis_code", "old_zip_code", "zip_code", 
                  paste0(address_level, "_kana"),
                  address_level,
                  # is_ はそれぞれ 1... 該当、0... 該当せず
                  "is_street_duplicate", # 一町域が二以上の郵便番号で表される場合の表示
                  "is_banchi", # 小字毎に番地が起番されている町域の表示
                  "is_cyoumoku", # 丁目を有する町域の場合の表示 
                  "is_zipcode_duplicate", # 一つの郵便番号で二以上の町域を表す場合の表示
                  "status", # 更新の表示 0... 更新なし, 1... 変更あり, 2... 廃止
                  "modify_type" # 変更理由 0... 変更なし, 1... 市政・区政・町政・分区・政令指定都市施行, 2... 住居表示の実施, 3... 区画整理, 4... 郵便区調整等, 5... 訂正, 6... 廃止
    ),
    roman = c("zip_code",
              address_level,
              paste0(address_level, "_roman"))
    )
  
  if (type == "oogaki") {
    df <- 
      readr::read_csv(path,
                    locale = readr::locale(encoding = "cp932"),
                    col_names = col_vars$yomi,
                    col_types = "ccccccccclllddd")
  } else if (type == "kogaki") {
    df <- 
      readr::read_csv(path, 
                      locale = readr::locale(encoding = "cp932"),
                      col_names = col_vars$yomi,
                      col_types = "ccccccccclllddd")
  } else if (type == "roman") {
    df <- 
      dplyr::mutate_at(readr::read_csv(path,
                                       locale = readr::locale(encoding = "cp932"),
                                       col_names = col_vars$roman,
                                       col_types = "ccccccc"),
                       dplyr::vars(tidyselect::ends_with("roman")),
                       stringr::str_to_title)
  }
  dplyr::mutate_if(df,
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
  purrr::map_chr(x,
                 ~ if (rlang::is_true(is_zip(.x)))
                   if (rlang::is_false(remove)) {
                     paste0(stringr::str_sub(.x, 1, 3),
                            "-",
                            stringr::str_sub(.x, 4, 7))
                   } else {
                     stringr::str_remove_all(.x, "-")
                   }
                 else
                   NA_character_)
}
