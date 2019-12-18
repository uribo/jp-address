library(dplyr)
library(data.table)
library(zipangu)

dt_isja <-
  readr::read_rds("/Users/suryu/Documents/projects2019/jp-address/data-raw/isj_2017a.rds") %>%
  as.data.table()
dt_isja[prefecture == "岩手県" &
        stringr::str_detect(city, "岩手郡")][, city] %>% unique()

df <-
  read_zipcode("~/Documents/projects2019/jp-address/data-raw/japanpost_kogaki/KEN_ALL.CSV",
               type = "kogaki") %>%
  select(-old_zip_code, -ends_with("kana"))

# 市区町村名の後ろに町域名がなく、番地がくる住所 --> NAで処理?
df %>%
  filter(stringr::str_detect(street, "の次に番地がくる場合"))
# 町域名がない市区町村 --> ~~NA~~ そのまま
df %>%
  filter(stringr::str_detect(street, "一円"))

# 同じ町域名 (street) で複数のレコード (カッコ内に書かれている小字で分かれている)
df %>%
  filter(is_street_duplicate == 1) %>%
  select(street)
df %>%
  filter(is_zipcode_duplicate == 1) %>%
  select(street)

df %>%
  count(zip_code, prefecture, city, sort = TRUE)

df %>%
  filter(zip_code == "4520961", prefecture == "愛知県", city == "清須市") %>%
  select(zip_code, prefecture, city, street)

df %>%
  filter(stringr::str_detect(street, "\\(")) %>%
  filter(stringr::str_detect(street, "\\)$", negate = TRUE)) %>%
  select(zip_code, prefecture, city, street)
df %>%
  filter(stringr::str_detect(street, "\\)$")) %>%
  filter(stringr::str_detect(street, "\\(", negate = TRUE)) %>%
  select(zip_code, prefecture, city, street)


df_tgt <-
  df %>%
  # 0285102 ... 〜で複数の地名が混ざる（行を分割可能）
  # 8260043 ... カッコ内の地名が2行にまたがる
  # 5810027 ... 重複
  # 6040094、6040811
  filter(zip_code == "8260043") %>%
  select(zip_code, prefecture, city, street)

x <-
  "葛巻(第40地割「57番地125、176を除く」〜第45地割)" %>%
  stringr::str_extract("\\(.+\\)") %>%
  stringr::str_remove_all("\\(|\\)")

seq_split_numchar <- function(str, split_chr = "-", prefix = NULL, suffix = NULL) {
  x_split <-
    str %>%
    stringr::str_split(split_chr, simplify = TRUE)
  # length(x_split) == 2L
  seq_num <- x_split
  if (!is.null(prefix)) {
    seq_num <-
      stringr::str_remove(seq_num, prefix)
    x_split <-
      x_split %>%
      purrr::modify_if(~ stringr::str_detect(.x, paste0("^", prefix), negate = TRUE),
                       ~ stringr::str_c(prefix, .x))
  }
  if (!is.null(suffix)) {
    seq_num <-
      stringr::str_remove(seq_num, paste0(suffix, ".+"))
    x_split <-
      x_split %>%
      purrr::modify_if(~ stringr::str_detect(.x, paste0(suffix, "$"), negate = TRUE),
                       ~ stringr::str_c(.x, suffix))
  }
  seq_num <-
    seq_num %>%
    stringr::str_extract_all("[0-9]{1,}", simplify = TRUE) %>%
    as.numeric()
  x_add <-
    paste0(
      prefix,
      seq.int(min(seq_num) + 1, max(seq_num) - 1),
      suffix
    )
  append(x_split, x_add, after = 1)
}
seq_split_numchar(str = "1〜4丁目", split_chr = "〜", prefix = NULL, suffix = "丁目")
df_tgt %>%
  mutate(split_street = purrr::pmap(.,
                                    ~ seq_split_numchar(..4 %>%
                                                          stringr::str_extract("\\(.+\\)") %>%
                                                          stringr::str_remove_all("\\(|\\)"),
                                                        split_chr = "\u301c", "第", "地割"))) %>%
  tidyr::unnest(cols = split_street) %>%
  mutate(split_street = paste0("葛巻(", split_street, ")")) %>%
  transmute(zip_code,
            prefecture,
            city,
            street = split_street)

x <- c("奈良(青葉町、大浦、会社町、霞ケ丘、後藤寺西団地、後藤寺東団地、希望ケ丘、",
       "松の木、三井後藤寺、緑町、月見ケ丘)")

merge_row_char <- function(str) {
  stringr::str_c(str, collapse = "") %>%
    stringr::str_extract("\\(.+\\)") %>%
    stringr::str_remove_all("\\(|\\)") %>%
    stringr::str_split("、", simplify = TRUE)
}

merge_row_char(x)

df_tgt <-
  df_tgt %>%
  group_by(zip_code, prefecture, city) %>%
  mutate(street = stringr::str_c(street, collapse = "")) %>%
  ungroup() %>%
  slice(1L)

split_multiple_str <- function(str, commons) {
  str %>%
    stringr::str_extract("\\(.+\\)") %>%
    stringr::str_remove_all("\\(|\\)") %>%
    stringr::str_split("、", simplify = TRUE)  %>%
    stringr::str_c(commons, .)
}

split_multiple_str(df_tgt$street, "亀屋町")
df_tgt %>%
  mutate(split_street = purrr::pmap(.,
                          ~ split_multiple_str(..4, commons = "亀屋町"))) %>%
  tidyr::unnest(cols = split_street) %>%
  transmute(zip_code,
            prefecture,
            city,
            street = split_street)

tidy_zipcode <- function(df) {
  df_duplicate <-
    df %>%
    count(zip_code, city, street, sort = TRUE) %>%
    filter(n > 1) %>%
    assertr::verify(nrow(.) == 2L) %>%
    transmute(zip_code, city, street, duplicate = TRUE)
  df_fix <-
    df %>%
    left_join(df_duplicate,
              by = c("zip_code", "city", "street")) %>%
    group_by(zip_code, city, street) %>%
    slice(1L) %>%
    ungroup() %>%
    tibble::rowid_to_column()
  multiple_rows_start <-
    df_fix %>%
    filter(stringr::str_detect(street, "\\(") & stringr::str_detect(street, "\\)$", negate = TRUE)) %>%
    select(rowid) %>%
    pull(rowid)
  multiple_rows_end <-
    df_fix %>%
    filter(stringr::str_detect(street, "\\)$") & stringr::str_detect(street, "\\(", negate = TRUE)) %>%
    select(rowid) %>%
    pull(rowid)
  df_merge_rows <-
    purrr::map2_dfr(
      multiple_rows_start,
      multiple_rows_end,
      ~ df_fix[.x:.y, ] %>%
        mutate(street = paste(street, collapse = "")) %>%
        slice(1L))
  df_fix <-
    df_fix %>%
    assertr::verify(nrow(.) ==  124349) %>%
    anti_join(df_merge_rows %>%
                select(jis_code, zip_code, city),
              by = c("jis_code", "zip_code", "city")) %>%
    assertr::verify(nrow(.) == 123834) # -515 (単純に2行分ではない。3行のものもある)
  df_fix <-
    df_fix %>%
    bind_rows(df_merge_rows) %>%
    arrange(rowid)
  df_fix
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

df <-
  read_zipcode("~/Documents/projects2019/jp-address/data-raw/japanpost_kogaki/KEN_ALL.CSV",
               type = "kogaki") %>%
  select(-old_zip_code, -ends_with("kana"))

df_fix <-
  df %>%
  tidy_zipcode()

df_fix <-
  df_fix %>%
  select(jis_code, zip_code, city, street, ends_with("_duplicate"))

df_fix %>%
  filter(zip_code == "8260043")
df_tmp <-
  df_fix %>%
  filter(stringr::str_detect(street, "\u301c"), is_street_duplicate == 1)

separate_street_rows <- function(data, col, pattern = "\\(.+丁目\\)$", split_chr, prefix = NULL, suffix = NULL) {
  street <- rlang::enquo(col)
  data %>%
    filter(stringr::str_detect(!!street, split_chr)) %>%
    filter(stringr::str_detect(!!street, paste0("\\(.+", pattern, "\\)$")),
           stringr::str_detect(!!street, "、", negate = TRUE)) %>%
    tidyr::extract(col = !!street,
                   into = c("street", "street_sub"),
                   regex  = c("(.+)\\((.+)\\)")) %>%
      mutate(split_street = purrr::pmap(.,
                                      ~ seq_split_numchar(..5,
                                                          split_chr,
                                                          prefix,
                                                          suffix))) %>%
    tidyr::unnest(cols = split_street) %>%
    mutate(split_street = paste0(street, split_street)) %>%
    select(-street, -street_sub) %>%
    rename(street = split_street)
}
df_tmp_chyome <-
  separate_street_rows(df_tmp, street, pattern = "丁目", split_chr = "\u301c", suffix = "丁目")
df_tmp_banchi <-
  separate_street_rows(df_tmp, street, pattern = "番地", split_chr = "\u301c", suffix = "番地")
df_tmp_ban <-
  separate_street_rows(df_tmp, street, pattern = "番", split_chr = "\u301c", suffix = "番")
separate_street_rows(df_tmp, street, pattern = "地割", split_chr = "\u301c", prefix = "第", suffix = "地割")

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
