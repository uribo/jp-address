################################
# 国土交通省 位置参照情報ダウンロードサービス
# http://nlftp.mlit.go.jp/isj/
# 全国のデータを都道府県単位でダウンロードしておく
################################
library(dplyr)
library(readr)

# a -----------------------------------------------------------------------
var_a <-
  c("prefecture", "city", 
    "street_lv1", # ... "大字_町丁目名"
    "street_lv2", # ... "小字_通称名",
    "street_lv3", # ... "街区符号_地番"
    "座標系番号", "coord_x", "coord_y",
    "latitude", "longitude", 
    "住居表示フラグ", "代表フラグ",
    "更新前履歴フラグ", "更新後履歴フラグ")

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
                as.double))


# 大字・町丁目レベル ----------------------------------------------------------------------
var_b <- 
  c("prefecture_code", "prefecture", 
    "city_code", "city",
    "street_lv1_code", # "大字_町丁目コード"
    "street_lv1", # ... "大字_町丁目名"
    "latitude", "longitude",
    "原典資料コード",
    "street_levels")

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
      mutate_at(vars(longitude, latitude), as.double))
