# Pkgs --------------------------------------------------------------------
library(dplyr)
library(assertr)
library(ensurer)
library(zipangu)
library(stringr)

# 問題点の整理 ------------------------------------------------------------------
# 1. 複数行への分割
# 2. 重複
if (file.exists(here::here("data-raw/japanpost_kogaki/KEN_ALL.CSV")) == FALSE) {
  df <- 
    read_zipcode(path = "https://www.post.japanpost.jp/zipcode/dl/oogaki/zip/ken_all.zip",
               type = "kogaki")
  dir.create(here::here("data-raw/japanpost_kogaki"))
  fs::file_copy(list.files(tempdir(), pattern = "KEN_ALL.CSV", full.names = TRUE),
                new_path = here::here("data-raw/japanpost_kogaki/KEN_ALL.CSV"))
} else {
  df <- 
    read_zipcode(here::here("data-raw/japanpost_kogaki/KEN_ALL.CSV"), 
                 type = "kogaki")
}
df <- 
  df %>% 
  select(-old_zip_code, -ends_with("kana"), -status, -modify_type) %>% 
  verify(dim(.) == c(124351, 9))


# 1. 複数行への分割
# > 全角となっている町域部分の文字数が38文字を越える場合、また半角となっているフリガナ部分の文字数が76文字を越える場合は、複数レコードに分割しています
df_multi_rows <- 
  df %>% 
  filter(stringr::str_detect(street, "\\(") & stringr::str_detect(street, "\\)$", negate = TRUE)) %>% 
  distinct(jis_code, zip_code, street) %>% 
  verify(nrow(.) == 207L)
# 3行で一レコード分
df %>% 
  filter(zip_code == "0660005") %>% 
  verify(nrow(.) == 3L)

# 2. 同一郵便番号での都道府県名、市区町村名および町域名の重複
# is_cyoumoku
df %>% 
  distinct(jis_code, street, is_cyoumoku, .keep_all = TRUE) %>%
  count(jis_code, zip_code, street, sort = TRUE) %>% 
  filter(n > 1) %>% 
  verify(nrow(.) == 2L)
# ここも重複しているが郵便番号は異なる
df %>% 
  filter(jis_code == "15101", street == "東栄町") %>% 
  verify(nrow(.) == 2L)
duplicate_zipcode <- c("5810027", "6730012")
df %>% 
  filter(zip_code %in% duplicate_zipcode) %>% 
  verify(nrow(.) == 4L)

# 連続した番号の省略
# 複数住所が同一レコード内で扱われる


# 市区町村名の後ろに町域名がなく、番地がくる住所 --> NAで処理?
df %>%
  filter(stringr::str_detect(street, "の次に番地がくる場合")) %>% 
  verify(nrow(.) == 17L)
# 町域名がない市区町村 --> ~~NA~~ そのまま
df %>%
  filter(stringr::str_detect(street, "一円")) %>% 
  verify(nrow(.) == 23L)

# tidy --------------------------------------------------------------------
source("R/split_seq_address.R")
# str <- 
#   "犬落瀬(内金矢、内山、岡沼、金沢、金矢、上淋代、木越、権現沢、四木、七百、下久保「174を除く」、下淋代、高森、通目木、坪毛沢「2沢、南平、柳沢、大曲)"
# fix_unclose_addr(str)
# fix_unclose_addr <- function(str) {
#   if (stringr::str_detect(str, "「[^」]*$"))
#     stringr::str_replace(str, "「[^」]*$", "\\0」")
#   else 
#     str
# }
# df_tgt %>%
#   mutate(split_street = purrr::pmap(.,
#                                     ~ split_seq_address(..4 %>%
#                                                           stringr::str_extract("\\(.+\\)") %>%
#                                                           stringr::str_remove_all("\\(|\\)"),
#                                                         split_chr = "\u301c", "第", "地割"))) %>%
#   tidyr::unnest(cols = split_street) %>%
#   mutate(split_street = paste0("葛巻(", split_street, ")")) %>%
#   transmute(zip_code,
#             prefecture,
#             city,
#             street = split_street)
#

# df_tgt <-
#   df_tgt %>%
#   group_by(zip_code, prefecture, city) %>%
#   mutate(street = stringr::str_c(street, collapse = "")) %>%
#   ungroup() %>%
#   slice(1L)
# 
# df_tgt %>%
#   mutate(split_street = purrr::pmap(.,
#                           ~ split_multiple_str(..4, commons = "亀屋町"))) %>%
#   tidyr::unnest(cols = split_street) %>%
#   transmute(zip_code,
#             prefecture,
#             city,
#             street = split_street)

tidy_zipcode <- function(df) {
  df_duplicate <-
    df %>%
    dplyr::count(zip_code, city, street, sort = TRUE) %>%
    dplyr::filter(n > 1) %>%
    assertr::verify(nrow(.) == 2L) %>%
    dplyr::transmute(zip_code, city, street, duplicate = TRUE)
  df_fix <-
    df %>%
    dplyr::left_join(df_duplicate,
              by = c("zip_code", "city", "street")) %>%
    dplyr::group_by(zip_code, city, street) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    tibble::rowid_to_column() %>% 
    dplyr::select(-duplicate)
  multiple_rows_start <-
    df_fix %>%
    dplyr::filter(stringr::str_detect(street, "\\(") & stringr::str_detect(street, "\\)$", 
                                                                           negate = TRUE)) %>%
    dplyr::select(rowid) %>%
    dplyr::pull(rowid)
  multiple_rows_end <-
    df_fix %>%
    dplyr::filter(stringr::str_detect(street, "\\)$") & stringr::str_detect(street, "\\(", 
                                                                            negate = TRUE)) %>%
    dplyr::select(rowid) %>%
    dplyr::pull(rowid)
  df_merge_rows <-
    purrr::map2_dfr(
      multiple_rows_start,
      multiple_rows_end,
      ~ df_fix[.x:.y, ] %>%
        dplyr::mutate(street = paste(street, collapse = "")) %>%
        dplyr::slice(1L))
  df_fix <-
    df_fix %>%
    #assertr::verify(nrow(.) ==  124349) %>%
    dplyr::anti_join(df_merge_rows %>%
                       dplyr::select(jis_code, zip_code, city),
              by = c("jis_code", "zip_code", "city"))# %>%
    #assertr::verify(nrow(.) == 123834) # -515 (単純に2行分ではない。3行のものもある)
  df_fix <-
    df_fix %>%
    dplyr::bind_rows(df_merge_rows) %>% 
    mutate(street = if_else(stringr::str_detect(street, "大曲\\)$"),
                            stringr::str_replace(street, "大曲\\)", "大曲」\\)"),
                            street))

  df_fix %>% 
    dplyr::arrange(rowid)# %>% dplyr::select(-rowid)
  # df_torina <-
  #   df_fix %>%
  #   dplyr::filter(stringr::str_detect(city, "京都市((上|中|下)京|東山)区"),
  #                 stringr::str_detect(street, "上る|下る"))
  # df_fix <-
  #   df_fix %>%
  #   anti_join(df_torina,
  #             by = c("jis_code", "zip_code", "prefecture", "city", "street",
  #                    "is_street_duplicate", "is_banchi", "is_cyoumoku", "is_zipcode_duplicate",
  #                    "status", "modify_type", "duplicate"))
}
separate_street_rows <- function(data, col, pattern = "丁目", split_chr, prefix = NULL, suffix = NULL) {
  street <- rlang::enquo(col)
  data %>%
    filter(stringr::str_detect(!!street, split_chr)) %>%
    filter(stringr::str_detect(!!street, paste0("\\(.+", pattern, "\\)$"))) %>%
    tidyr::extract(col = !!street,
                   into = c("street", "street_sub"),
                   regex  = c("(.+)\\((.+)\\)")) %>%
    mutate(split_street = purrr::pmap(.,
                                      ~ split_seq_address(..6,
                                                          split_chr,
                                                          prefix,
                                                          suffix))) %>%
    tidyr::unnest(cols = split_street) %>%
    mutate(split_street = paste0(street, split_street)) %>%
    select(-street, -street_sub) %>%
    rename(street = split_street) %>% 
    select(names(data))
}

df_fix <-
  df %>%
  tidy_zipcode()# %>% mutate(id = row_number())


df_split_rows <- 
  df_fix %>%
  dplyr::filter(stringr::str_detect(street, "、")) %>% 
  dplyr::mutate(split_street = purrr::pmap(.,
                                           ~ split_inside_address(..6))) %>%
  tidyr::unnest(cols = split_street) %>%
  dplyr::select(-street) %>%
  dplyr::rename(street = split_street) %>%
  dplyr::select(names(data), "rowid")

df_fix <-
  df_fix %>% 
  filter(!rowid %in% df_split_rows$rowid) %>% 
  bind_rows(df_split_rows)


# df_fix <-
#   df_fix %>%
#   select(jis_code, zip_code, city, street, ends_with("_duplicate"))

# 複数の行を一行に
df_fix %>% 
  filter(zip_code %in% unique(df_multi_rows$zip_code)) %>% 
  pull(street) %>% 
  stringr::str_subset("除く")

"犬落瀬(内金矢、内山、岡沼、金沢、金矢、上淋代、木越、権現沢、四木、七百、下久保「174を除く」、下淋代、高森、通目木、坪毛沢「2沢、南平、柳沢、大曲」)" %>% 
  split_inside_address()
"山田町下谷上(大上谷、修法ケ原、中一里山「9番地の4、12番地を除く」、長尾山、再度公園)" %>% 
  split_inside_address()

df_fix %>% 
  filter(zip_code == "0660005") %>% 
  pull(street)
df_fix %>%
  filter(zip_code == "8260043") %>% 
  pull(street)

# 、で区切られた複数の住所を別々の行に分割
# 合わせて括弧を取り除くが、京都市の通り名は括弧の中の住所が町域名の前に来るようにする
df %>% 
  filter(jis_code == "26104", stringr::str_detect(street, "中之町.+寺町通四条上る"))
# --> 寺町通四条上る中之町
df_fix %>% 
  filter(jis_code == "26104", stringr::str_detect(street, "寺町通四条上る"))


df_fix %>% 
  filter(zip_code =="0350002")

df_fix %>% 
  filter(stringr::str_detect(street, "、"))

x <- "藤野(400、400−2番地) "



# 〜を含み、複数の行に分割可能。、−及びを含まない
df_tmp <-
  df_fix %>%
  filter(stringr::str_detect(street, "\u301c"), is_street_duplicate == 1) %>% 
  filter(stringr::str_detect(street, "、|−|及び", negate = TRUE)) %>% 
  filter(stringr::str_detect(street, ".+\u301c.+\u301c", negate = TRUE)) %>% 
  assertr::verify(nrow(.) == 186L)
chyome_x <- 
  unique(df_tmp$street) %>% 
  stringr::str_subset("\\(.+丁目\\)")%>% 
  stringr::str_remove("\\(.+\\)")

# 残っているの
df_tmp %>% 
  filter(stringr::str_detect(street, "[0-9]{1,}(丁目|の).+番地"))


# 1. 丁目 -------------------------------------------------------------------
df_tmp_chyome <-
  separate_street_rows(df_tmp, street, pattern = "丁目", split_chr = "\u301c", suffix = "丁目")
df_tmp_chyome %>% 
  filter(stringr::str_detect(street, paste0("^(",
                                            paste0(chyome_x, collapse = "|"),
                                            ")"),
                             negate = TRUE))
unique(df_tmp_chyome$street)

# 2. 番地 -------------------------------------------------------------------
df_tmp_banchi <-
  df_tmp %>% 
  filter(stringr::str_detect(street, "[0-9]{1,}(丁目|の).+番地", negate = TRUE) &
         stringr::str_detect(street, "番地")) %>% 
  separate_street_rows(street, pattern = "番地", split_chr = "\u301c", suffix = "番地")



# df_tmp %>%
#   filter(stringr::str_detect(street, "[0-9]{1,}(丁目|の).+番地", negate = TRUE) &
#            stringr::str_detect(street, "番地")) %>% 
#   filter(stringr::str_detect(street, "\u301c")) %>%
#   filter(stringr::str_detect(street, paste0("\\(.+", "番地", "\\)$"))) %>%
#   tidyr::extract(col = street,
#                  into = c("street", "street_sub"),
#                  regex  = c("(.+)\\((.+)\\)")) -> d
# d$street_sub[44:48] %>% 
#   purrr::map(
#     ~ split_seq_address(.x, split_chr = "〜", suffix = "番地")
#   )

unique(df_tmp_banchi$street)

df_tmp_ban <-
  separate_street_rows(df_tmp, street, pattern = "番", split_chr = "\u301c", suffix = "番")
unique(df_tmp_ban$street)

# 3. 地割 -------------------------------------------------------------------
df_tmp_chiwari <-
  separate_street_rows(df_tmp, street, pattern = "地割", split_chr = "\u301c", prefix = "第", suffix = "地割")

df_tmp_sen <-
  separate_street_rows(df_tmp, street, pattern = "線", split_chr = "\u301c", suffix = "線")

df_tmp %>% 
  filter(!id %in% unique(df_tmp_chyome$id)) %>% 
  filter(!id %in% unique(df_tmp_banchi$id)) %>% 
  filter(!id %in% unique(df_tmp_ban$id)) %>% 
  filter(!id %in% unique(df_tmp_chiwari$id)) %>% 
  pull(street) %>% 
  unique()

df_fix %>% 
  filter(stringr::str_detect(street, "鶴見.+組"))

df_fix %>% 
  filter(stringr::str_detect(zip_code, "1340015"))

# df_fix %>% 
#   filter(stringr::str_detect(street, "\\(.+丁目\\)")) %>%
#   filter(stringr::str_detect(street, "、", negate = TRUE), 
#          stringr::str_detect(street, "〜")) %>% 
#   separate_street_rows(street, pattern = "丁目", split_chr = "〜", suffix = "丁目", sep_chr = "、")

df_fix %>% 
  filter(stringr::str_detect(street, "\\(.+丁目\\)")) %>%
  filter(stringr::str_detect(street, "、"), stringr::str_detect(street, "〜", negate = TRUE)) %>% 
  select(jis_code, zip_code, prefecture, city, street) %>% 
  separate_street_rows(street, pattern = "丁目", split_chr = "、", suffix = "丁目", sep_chr = "、") %>% 
  filter(city %in% c("苫小牧市"))


df_tmp %>%
  filter(stringr::str_detect(street, "葛巻")) %>%
  filter(stringr::str_detect(street, paste0("\\(.+", pattern, "\\)$")),
         stringr::str_detect(street, "、", negate = TRUE))


df_tmp %>%
  pull(street) %>%
  unique() %>%
  stringr::str_subset("〜") %>%
  stringr::str_extract("\\(.+\\)") %>%
  stringr::str_remove_all("\\(|\\)") %>%
  stringr::str_subset("丁目", negate = TRUE) %>%
  stringr::str_subset("番地", negate = TRUE) %>%
  stringr::str_subset("番", negate = TRUE) %>%
  stringr::str_subset("第.+地割", negate = TRUE)

df_fix %>%
  filter(stringr::str_detect(street, "\u301c"), is_street_duplicate == 0)
