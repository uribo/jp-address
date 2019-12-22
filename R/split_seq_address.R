# split_seq_address(str = "吾妻1、2丁目", split_chr = "、", prefix = "吾妻", suffix = "丁目", seq = FALSE)
# split_seq_address(str = "1〜4丁目", split_chr = "〜", prefix = NULL, suffix = "丁目")
# split_seq_address(str = "草刈(1〜4)", split_chr = "〜", prefix = "草刈")
# split_seq_address_mix(str = "1〜4丁目、6、9丁目", split_chr = "〜", prefix = NULL, suffix = "丁目", sep_chr = "、")
# split_seq_address_mix(str = "1〜4丁目", split_chr = "〜", prefix = NULL, suffix = "丁目")
split_seq_address <- function(str, split_chr = "-", prefix = NULL, suffix = NULL, seq = TRUE) {
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
                       ~ stringr::str_c(prefix, .x)) %>% 
      stringr::str_remove("\\(|\\)")
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
      stringi::stri_trans_nfkc() %>% 
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
split_seq_address_mix <- function(str, split_chr = "-", prefix = NULL, suffix = NULL, seq = TRUE, sep_chr = "、") {
  split_seq_address(str, split_chr, prefix, suffix, seq) %>% 
    as.list() %>% 
    purrr::modify_if(~ stringr::str_detect(.x, sep_chr),
                     ~ split_seq_address(str = .x, 
                                         split_chr = sep_chr, 
                                         prefix = prefix, 
                                         suffix = suffix, 
                                         seq = FALSE)) %>% 
    purrr::reduce(c)
}

separate_street_rows <- function(data, col, pattern = "丁目", split_chr, prefix = NULL, suffix = NULL) {
  street <- rlang::enquo(col)
  data %>%
    dplyr::filter(stringr::str_detect(!!street, split_chr)) %>%
    dplyr::filter(stringr::str_detect(!!street, paste0("\\(.+", pattern, "\\)$"))) %>%
    tidyr::extract(col = !!street,
                   into = c("street", "street_sub"),
                   regex  = c("(.+)\\((.+)\\)")) %>%
    mutate(split_street = purrr::pmap(.,
                                      ~ split_seq_address(..6,
                                                          split_chr,
                                                          prefix,
                                                          suffix))) %>%
    tidyr::unnest(cols = split_street) %>%
    dplyr::mutate(split_street = paste0(street, split_street)) %>%
    dplyr::select(-street, -street_sub) %>%
    dplyr::rename(street = split_street) %>% 
    dplyr::select(names(data))
}

# is_jhistorical_street("京都市中京区寺町通御池上る上本能寺町488番地")
is_jhistorical_street <- function(str) {
  stringr::str_detect(str, "(下る|上る|東入|西入)")
}
# x <- c("青葉町、大浦、会社町、霞ケ丘、後藤寺西団地、後藤寺東団地、希望ケ丘、",
#        "松の木、三井後藤寺、緑町、月見ケ丘")
# split_multiple_address(x)
# split_multiple_address(x, prefix = "奈良")
# split_multiple_address(c("寺町通四条上る、新京極通錦小路下る"), suffix = "中之町")
# split_inside_address("材木町(木屋町通松原下る、松原通木屋町東入、松原通高瀬川筋東入)")
# split_inside_address(str = "藤野(400、400−2番地)")
# split_inside_address(str = "天王(追分、追分西、上北野、長沼)")
split_multiple_address <- function(str, delim = "、", prefix = NULL, suffix = NULL) {
  x <- 
    stringr::str_c(str, collapse = "")
  x <- 
    x %>% 
    stringr::str_split(delim, simplify = TRUE) %>% 
    as.vector()
  # if (sum(stringr::str_detect(x, "「|」")) > 2) {
  #   stringr::str_c(str, collapse = "")
  # }
  # if (sum(stringr::str_detect(x, "「|」")) == 2) {
  #   xx <- 
  #     str_extract_all(str, "「.+、.+」、", simplify = TRUE)
  #   x[seq.int(str_which(x, "「"), str_which(x, "「") + 1)] <- 
  #     x[3:4] %>% 
  #     stringr::str_remove("「.+|.+」") %>% 
  #     stringr::str_subset(".{1}", negate = FALSE) %>% 
  #     stringr::str_c(xx)
  #   x <- 
  #     unique(x)
  # }
  
  if (!is.null(prefix))
    x <- stringr::str_c(prefix, x)
  if (!is.null(suffix))
    x <- stringr::str_c(x, suffix)
  x
}
split_inside_address <- function(str, delim = "、", prefix = NULL, suffix = NULL) {
  common_str <- 
    stringr::str_extract(str, "(.*)(?=\\()")
  if (is.null(prefix) & is.null(suffix)) {
    if (is_jhistorical_street(str)) {
      suffix <- common_str
    } else {
      prefix <- common_str
    }
  }
  inside_str <- 
    stringr::str_extract(str, "(?<=\\().*?(?=\\))")
  if (is.null(suffix)) {
    suffix <-
      stringr::str_extract(inside_str, "(丁目|番地)$")
    if (!is.na(suffix)) {
      inside_str <- 
        stringr::str_remove_all(inside_str, suffix)  
    } else {
      suffix <- NULL
    }
  }
  split_multiple_address(inside_str, 
                         delim = delim, 
                         prefix = prefix, 
                         suffix = suffix)
}

zip_tidy_prep <- function(df) {
  df_duplicate <-
    df %>%
    dplyr::count(zip_code, city, street, sort = TRUE) %>%
    dplyr::filter(n > 1) %>%
    dplyr::transmute(zip_code, city, street, duplicate = TRUE)
  if (nrow(df_duplicate) >= 1) {
    df <-
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
    dplyr::anti_join(df_merge_rows %>%
                       dplyr::select(jis_code, zip_code, city),
                     by = c("jis_code", "zip_code", "city"))
  df_fix <-
    df_fix %>%
    dplyr::bind_rows(df_merge_rows) %>% 
    df_fix %>% 
    dplyr::arrange(rowid)
}

street_fix_keys <- 
  c(`大江(1丁目、2丁目「651、662、668番地」以外、3丁目5、13−4、20、678、687番地)` = paste0("大江",
                                                                   c("1丁目", "2丁目「651、662、668番地」以外", "3丁目5番地", "3丁目13-4番地", "3丁目20番地", "3丁目678番地", "3丁目687番地"),
                                                                   collapse = "_"),
    `犬落瀬(内金矢、内山、岡沼、金沢、金矢、上淋代、木越、権現沢、四木、七百、下久保「174を除く」、下淋代、高森、通目木、坪毛沢「2沢、南平、柳沢、大曲)` = paste0("犬落瀬",
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
