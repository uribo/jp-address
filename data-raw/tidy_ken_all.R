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
df %>% 
  filter(str_detect(street, "〜"))

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
tidy_zipcode <- function(df) {
  df_duplicate <-
    df %>%
    dplyr::count(zip_code, city, street, sort = TRUE) %>%
    dplyr::filter(n > 1) %>%
    #assertr::verify(nrow(.) == 2L) %>%
    dplyr::transmute(zip_code, city, street, duplicate = TRUE)
  if (nrow(df_duplicate) >= 1) {
    df_fix <-
      df %>%
      dplyr::left_join(df_duplicate,
                       by = c("zip_code", "city", "street")) %>%
      dplyr::group_by(zip_code, city, street) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(-duplicate)    
  }
    df_fix <- 
      df %>% 
      tibble::rowid_to_column()
  multiple_rows_start <-
    df_fix %>%
    dplyr::filter(stringr::str_detect(street, "\\(") & stringr::str_detect(street, "\\)$", 
                                                                           negate = TRUE)) %>%
    dplyr::pull(rowid)
  multiple_rows_end <-
    df_fix %>%
    dplyr::filter(stringr::str_detect(street, "\\)$") & stringr::str_detect(street, "\\(", 
                                                                            negate = TRUE)) %>%
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
    dplyr::bind_rows(df_merge_rows)

  df_fix %>% 
    dplyr::arrange(rowid)# %>% dplyr::select(-rowid)

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
  dplyr::filter(stringr::str_detect(street, "「.+」", negate = TRUE),
                stringr::str_detect(street, "、")) %>% 
  dplyr::mutate(split_street = purrr::pmap(.,
                                           ~ split_inside_address(..6))) %>%
  tidyr::unnest(cols = split_street) %>%
  dplyr::select(-street) %>%
  dplyr::rename(street = split_street) %>%
  dplyr::select(names(df_fix), "rowid")

# そのままでOK
df_split_rows2 <-
  df_fix %>%
  dplyr::filter(stringr::str_detect(street, "「.+」"),
                stringr::str_detect(street, "、", negate = TRUE))

# bind_rows(
#   distinct(df_split_rows, rowid, .keep_all = TRUE),
#   df_split_rows2
# ) %>% 
#   count(rowid, sort = TRUE)

df_keep <- 
  df_fix %>%
  filter(!rowid %in% unique(df_split_rows$rowid)) %>% 
  filter(!rowid %in% unique(df_split_rows2$rowid))

street_fix_keys <- 
  c(`大江(1丁目、2丁目「651、662、668番地」以外、3丁目5、13−4、20、678、687番地)` = paste0("大江",
                                                                   c("1丁目", "2丁目「651、662、668番地」以外", "3丁目5番地", "3丁目13-4番地", "3丁目20番地", "3丁目678番地", "3丁目687番地"),
                                                                   collapse = "_"),
    `犬落瀬(内金矢、内山、岡沼、金沢、金矢、上淋代、木越、権現沢、四木、七百、下久保「174を除く」、下淋代、高森、通目木、坪毛沢「25、637、641、643、647を除く」、中屋敷、沼久保、根古橋、堀切沢、南平、柳沢、大曲)` = paste0("犬落瀬",
                                                                                                                                c("内金矢", "内山", "岡沼", "金沢", "金矢", "上淋代", "木越", "権現沢", 
                                                                                                                                  "四木", "七百", "下久保「174を除く」", "下淋代", "高森", "通目木",
                                                                                                                                  "坪毛沢「25、637、641、643、647を除く」", "中屋敷", "沼久保", "根古橋",
                                                                                                                                  "堀切沢", "南平", "柳沢", "大曲"),
                                                                                                                                collapse = "_"),
    `折茂(今熊「213〜234、240、247、262、266、275、277、280、295、1199、1206、1504を除く」、大原、沖山、上折茂「1−13、71−192を除く」)` = paste0("折茂",
                                                                                                           c("今熊「213〜234、240、247、262、266、275、277、280、295、1199、1206、1504を除く」",
                                                                                                             "大原",
                                                                                                             "沖山",
                                                                                                             "上折茂「1−13、71−192を除く」"),
                                                                                                           collapse = "_"),
    `葛巻(第40地割「57番地125、176を除く」〜第45地割)` = paste0("葛巻",
                                               c("第40地割「57番地125、176を除く」",
                                                 "第41地割",
                                                 "第42地割",
                                                 "第43地割",
                                                 "第44地割",
                                                 "第45地割"), collapse = "_"),
    `南山(430番地以上「1770−1〜2、1862−42、1923−5を除く」、大谷地、折渡、鍵金野、金山、滝ノ沢、豊牧、沼の台、肘折、平林)` = paste0("南山",
                                                                                      c("430番地以上「1770−1〜2、1862−42、1923−5を除く」",
                                                                                        "大谷地",
                                                                                        "折渡",
                                                                                        "鍵金野",
                                                                                        "金山",
                                                                                        "滝ノ沢",
                                                                                        "豊牧",
                                                                                        "沼の台",
                                                                                        "肘折",
                                                                                        "平林"),
                                                                                      collapse = "_"),
    `泉沢(烏帽子「榛名湖畔」、烏帽子国有林77林班)` = paste0("泉沢",
                                        c("烏帽子「榛名湖畔」",
                                          "烏帽子国有林77林班"),
                                        collapse = "_"),
    `茂田井(1〜500「211番地を除く」「古町」、2527〜2529「土遠」)` = paste0("茂田井",
                                                      c("1〜500「211番地を除く」「古町」",
                                                        "2527〜2529「土遠」"),
                                                      collapse = "_"),
    `山田町下谷上(大上谷、修法ケ原、中一里山「9番地の4、12番地を除く」、長尾山、再度公園)` = paste0("山田町下谷上",
                                                             c("大上谷", 
                                                               "修法ケ原",
                                                               "中一里山「9番地の4、12番地を除く」", 
                                                               "長尾山",
                                                               "再度公園"),
                                                             collapse = "_"),
    `山田町下谷上(菊水山、高座川向、中一里山「9番地の4、12番地」、念仏堂、ひよどり越)` = paste0("山田町下谷上",
                                                           c("菊水山",
                                                             "高座川向",
                                                             "中一里山「9番地の4、12番地」",
                                                             "念仏堂",
                                                             "ひよどり越"),
                                                           collapse = "_"))

df_fix2 <- 
  df_keep %>% 
  mutate(street = street %>% 
           recode(!!!street_fix_keys)) %>% 
  tidyr::separate_rows(street, sep = "_") %>% 
  bind_rows(df_split_rows) %>% 
  bind_rows(df_split_rows2) %>% 
  arrange(rowid) %>% 
  select(-rowid)

# 通り名もOK
df_fix2 %>% 
  filter(city == "京都市中京区", str_detect(street, "御池上る")) %>% 
  pull(street)

# 重複なし
# df_fix2 %>%
#   tibble::rowid_to_column() %>% 
#   distinct(jis_code, zip_code, prefecture, .keep_all = TRUE) %>% 
#   count(jis_code, zip_code, prefecture, city, street, sort = TRUE)

# 、で区切られた複数の住所を別々の行に分割
# 合わせて括弧を取り除くが、京都市の通り名は括弧の中の住所が町域名の前に来るようにする
df %>% 
  filter(jis_code == "26104", stringr::str_detect(street, "中之町.+寺町通四条上る"))
# --> 寺町通四条上る中之町
df_fix2 %>% 
  filter(jis_code == "26104", stringr::str_detect(street, "寺町通四条上る"))

# 〜を含み、複数の行に分割可能。、−及びを含まない
df_tmp <-
  df_fix2 %>%
  filter(stringr::str_detect(street, "\u301c"), is_street_duplicate == 1) %>% 
  filter(stringr::str_detect(street, "、|−|及び", negate = TRUE)) %>% 
  filter(stringr::str_detect(street, ".+\u301c.+\u301c", negate = TRUE)) %>% 
  assertr::verify(nrow(.) == 274L) %>% 
  mutate(rowid = row_number()) # 最後に
chyome_x <- 
  unique(df_tmp$street) %>% 
  stringr::str_subset("\\(.+丁目\\)")%>% 
  stringr::str_extract("(.*)(?=\\()") %>% 
  unique()

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
df_tmp_banchi_a <- 
  df_tmp %>% 
  filter(zip_code == "8918221",
         str_detect(street, "阿三\\(799の1〜867番地\\)")) %>% 
  verify(nrow(.) == 1L) %>% 
  separate_street_rows(street, pattern = "番地", split_chr = "\u301c", prefix = "799の", suffix = "番地")
# 5番地とか存在するの??
# isjでもなし
df_tmp_banchi_b <- 
  df_tmp %>% 
  filter(zip_code == "6712202",
         str_detect(street, "北夢前台\\(1丁目1〜77番地\\)")) %>% 
  verify(nrow(.) == 1L) %>% 
  separate_street_rows(street, pattern = "番地", split_chr = "\u301c", prefix = "1丁目", suffix = "番地")

# df %>% 
#   filter(str_detect(street, "北夢前台"))

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
df_tmp_kumi <-
  separate_street_rows(df_tmp, street, pattern = "組", split_chr = "\u301c", suffix = "組")

separate_street_rows(df_tmp, street, pattern = "組", split_chr = "\u301c", suffix = "組")

df_last <- 
  df_tmp %>% 
  filter(!rowid %in% unique(df_tmp_chyome$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_banchi$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_banchi_a$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_banchi_b$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_ban$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_sen$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_kumi$rowid)) %>% 
  filter(!rowid %in% unique(df_tmp_chiwari$rowid))

df_last %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}$")) %>% 
  mutate(split_street = purrr::pmap(.,
                                    ~ split_seq_address(..5,
                                                        split_chr = "〜",
                                                        prefix = str_remove(..5, "[0-9]{1,}〜[0-9]{1,}"),
                                                        suffix = NULL))) %>% 
  tidyr::unnest(cols = split_street) %>% 
  select(-street) %>%
  rename(street = split_street) %>% 
  select(names(df_last))

df_last %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}$", negate = TRUE)) %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}(丁目|番地)$")) %>% 
  mutate(split_street = purrr::pmap(.,
                                    ~ split_seq_address(..5,
                                                        split_chr = "〜",
                                                        prefix = str_remove(..5, "[0-9]{1,}〜[0-9]{1,}.+"),
                                                        suffix = str_remove(..5, ".+[0-9]{1,}〜[0-9]{1,}")))) %>% 
  tidyr::unnest(cols = split_street)


df_last %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}$", negate = TRUE)) %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}(丁目|番地)$", negate = TRUE)) %>% 
  filter(str_detect(street, "\\([0-9]{1,}〜[0-9]{1,}\\)")) %>% 
  mutate(split_street = purrr::pmap(.,
                                    ~ split_seq_address(..5,
                                                        split_chr = "〜",
                                                        prefix = str_remove(..5, "\\([0-9]{1,}〜[0-9]{1,}\\)"),
                                                        suffix = NULL))) %>% 
  tidyr::unnest(cols = split_street)
  
df_last %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}$", negate = TRUE)) %>% 
  filter(str_detect(street, "[0-9]{1,}〜[0-9]{1,}(丁目|番地)$", negate = TRUE)) %>% 
  filter(str_detect(street, "\\([0-9]{1,}〜[0-9]{1,}\\)", negate = TRUE)) %>% 
  pull(street)


str_remove("美富183〜579番地", "[0-9]{1,}〜[0-9]{1,}")

# 括弧なし
df_tmp %>% 
  filter(str_detect(street, "〜"), 
         str_detect(street, "\\(.+\\)", negate = TRUE)) %>% 
  filter(str_detect(street, "美富183〜579番地")) %>% 
  mutate(split_street = purrr::pmap(.,
                                    ~ split_seq_address(..5,
                                                        split_chr = "〜",
                                                        prefix = str_remove(..5, "[0-9]{1,}〜[0-9]{1,}"),
                                                        suffix = NULL))) %>% 
  tidyr::unnest(cols = split_street)
  



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
