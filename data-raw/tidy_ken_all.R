library(dplyr)
library(zipangu)

df <-
  read_zipcode("~/Documents/projects2019/jp-address/data-raw/japanpost_kogaki/KEN_ALL.CSV",
               type = "kogaki") %>%
  select(-old_zip_code, -ends_with("kana"))

# 市区町村名の後ろに町域名がなく、番地がくる住所 --> NAで処理?
df %>%
  filter(stringr::str_detect(street, "の次に番地がくる場合")) %>% 
  assertr::verify(nrow(.) == 17L)
# 町域名がない市区町村 --> ~~NA~~ そのまま
df %>%
  filter(stringr::str_detect(street, "一円")) %>% 
  assertr::verify(nrow(.) == 23L)

seq_split_numchar <- function(str, split_chr = "-", prefix = NULL, suffix = NULL, seq = TRUE) {
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
  if (rlang::is_true(seq)) {
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
  } else {
    c(x_split)
  }
}
seq_split_sep_mix <- function(str, split_chr = "-", prefix = NULL, suffix = NULL, seq = TRUE, sep_chr = "、") {
  seq_split_numchar(str, split_chr, prefix, suffix, seq) %>% 
    as.list() %>% 
    purrr::modify_if(~ stringr::str_detect(.x, sep_chr),
                     ~ seq_split_numchar(str = .x, 
                                         split_chr = sep_chr, 
                                         prefix = prefix, 
                                         suffix = suffix, 
                                         seq = FALSE)) %>% 
    purrr::reduce(c)
}

seq_split_numchar(str = "1〜4丁目", split_chr = "〜", prefix = NULL, suffix = "丁目")
seq_split_sep_mix(str = "1〜4丁目、6、9丁目", split_chr = "〜", prefix = NULL, suffix = "丁目", sep_chr = "、")
seq_split_sep_mix(str = "1〜4丁目", split_chr = "〜", prefix = NULL, suffix = "丁目")

seq_split_numchar(str = "1、2丁目", split_chr = "、", prefix = NULL, suffix = "丁目", seq = FALSE)

# df_tgt %>%
#   mutate(split_street = purrr::pmap(.,
#                                     ~ seq_split_numchar(..4 %>%
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
# x <- c("奈良(青葉町、大浦、会社町、霞ケ丘、後藤寺西団地、後藤寺東団地、希望ケ丘、",
#        "松の木、三井後藤寺、緑町、月見ケ丘)")
# 
# merge_row_char <- function(str) {
#   stringr::str_c(str, collapse = "") %>%
#     stringr::str_extract("\\(.+\\)") %>%
#     stringr::str_remove_all("\\(|\\)") %>%
#     stringr::str_split("、", simplify = TRUE)
# }
# 
# merge_row_char(x)
# 
# df_tgt <-
#   df_tgt %>%
#   group_by(zip_code, prefecture, city) %>%
#   mutate(street = stringr::str_c(street, collapse = "")) %>%
#   ungroup() %>%
#   slice(1L)
# 
# split_multiple_str <- function(str, commons) {
#   str %>%
#     stringr::str_extract("\\(.+\\)") %>%
#     stringr::str_remove_all("\\(|\\)") %>%
#     stringr::str_split("、", simplify = TRUE)  %>%
#     stringr::str_c(commons, .)
# }
# 
# split_multiple_str(df_tgt$street, "亀屋町")
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
    assertr::verify(nrow(.) ==  124349) %>%
    dplyr::anti_join(df_merge_rows %>%
                       dplyr::select(jis_code, zip_code, city),
              by = c("jis_code", "zip_code", "city")) %>%
    assertr::verify(nrow(.) == 123834) # -515 (単純に2行分ではない。3行のものもある)
  df_fix <-
    df_fix %>%
    dplyr::bind_rows(df_merge_rows) %>%
    dplyr::arrange(rowid) %>% 
    dplyr::select(-rowid)
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
separate_street_rows <- function(data, col, pattern = "丁目", split_chr, prefix = NULL, suffix = NULL) {
  street <- rlang::enquo(col)
  data %>%
    filter(stringr::str_detect(!!street, split_chr)) %>%
    filter(stringr::str_detect(!!street, paste0("\\(.+", pattern, "\\)$"))) %>%
    tidyr::extract(col = !!street,
                   into = c("street", "street_sub"),
                   regex  = c("(.+)\\((.+)\\)")) %>%
    mutate(split_street = purrr::pmap(.,
                                      ~ seq_split_numchar(..6,
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
  tidy_zipcode() %>% 
  mutate(id = row_number())

# df_fix <-
#   df_fix %>%
#   select(jis_code, zip_code, city, street, ends_with("_duplicate"))

df_fix %>%
  filter(zip_code == "8260043")
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
#     ~ seq_split_numchar(.x, split_chr = "〜", suffix = "番地")
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
