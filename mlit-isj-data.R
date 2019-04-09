################################
# 国土交通省 位置参照情報ダウンロードサービス
# http://nlftp.mlit.go.jp/isj/
# 全国のデータを都道府県単位でダウンロードしておく
################################
library(dplyr)
library(readr)
library(assertr)
library(ensurer)

# a -----------------------------------------------------------------------
if (rlang::is_false(file.exists(here::here("data-raw/isj_2017a.rds")))) {
  var_a <-
    c("prefecture", "city", 
      "street_lv1", # ... "大字_町丁目名"
      "street_lv2", # ... "小字_通称名",
      "street_lv3", # ... "街区符号_地番"
      "cs_num", 
      "coord_x", "coord_y",
      "latitude", "longitude", 
      "住居表示フラグ", "代表フラグ",
      "更新前履歴フラグ", "更新後履歴フラグ") %>% 
    ensure(length(.) == 14L)
  
  df_isj_a <-
    fs::dir_ls(here::here("data-raw/isj/h29_a"), regexp = ".csv$") %>%
    purrr::map_dfr(
      ~ readr::read_csv(.x,
                        locale = readr::locale(encoding = "cp932"),
                        col_names = var_a) %>%
        dplyr::slice(-1L) %>%
        dplyr::mutate_at(dplyr::vars(prefecture, city, 
                                     num_range("street_lv", seq(1, 3))), 
                         as.character) %>%
        dplyr::mutate_at(dplyr::vars(`座標系番号`, coord_x, coord_y,
                                     latitude, longitude,
                                     `住居表示フラグ`, `代表フラグ`,
                                     `更新前履歴フラグ`,`更新後履歴フラグ`),
                         as.double)) %>% 
    verify(dim(.) == c(19637134, 14))
  
  df_isj_a %>% 
    select(prefecture, city, 
           num_range("street_lv", 1:3),
           cs_num, longitude, latitude,
           contains("フラグ")) %>% 
    verify(dim(.) == c(19637134, 12)) %>% 
    readr::write_rds(here::here("data-raw/isj_2017a.rds"), compress = "xz")
} else {
  df_isj_a <- 
    readr::read_rds(here::here("data-raw/isj_2017a.rds"))
}

# 大字・町丁目レベル ----------------------------------------------------------------------
if (rlang::is_false(file.exists(here::here("data-raw/isj_2017b.rds")))) {
  var_b <- 
    c("prefecture_code", "prefecture", 
      "city_code", "city",
      "street_lv1_code", # "大字_町丁目コード"
      "street_lv1", # ... "大字_町丁目名"
      "latitude", "longitude",
      "原典資料コード",
      "street_levels") %>% 
    ensure(length(.) == 10L)
  
  street_level_code <- 
    list(
      "大字" = 1,
      "字" = 2,
      "丁目" = 3,
      "不明(通称)" = 0)
  
  df_isj_b <- 
    fs::dir_ls(here::here("data-raw/isj/h29_b/"), regexp = ".csv$") %>% 
    purrr::map_dfr(
      ~ readr::read_csv(.x,
                        locale = locale(encoding = "cp932"),
                        col_names = var_b,
                        skip = 1) %>%
        select(prefecture, city_code, city, 
               street_lv1_code, street_lv1,
               longitude, latitude, street_levels) %>% 
        mutate_at(vars(prefecture, city_code, city, 
                       street_lv1_code, street_lv1, 
                       street_levels),
                  as.character) %>% 
        mutate_at(vars(longitude, latitude), as.double)) %>% 
    verify(dim(.) == c(189539, 8))
  
  df_isj_b %>% 
    select(city, contains("street_"), longitude, latitude) %>% 
    verify(ncol(.) == 6L) %>% 
    readr::write_rds(here::here("data-raw/isj_2017b.rds"), compress = "xz")
} else {
  df_isj_b <- 
    readr::read_rds(here::here("data-raw/isj_2017b.rds"))  
}
