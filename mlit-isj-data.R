################################
# 国土交通省 位置参照情報ダウンロードサービス
# http://nlftp.mlit.go.jp/isj/
# 全国のデータを都道府県単位でダウンロードしておく
# ref) cutting_board/ 190522_isj.R
################################
library(dplyr)
library(readr)
library(assertr)
library(ensurer)

# download ----------------------------------------------------------------
req_isj <- function(areaCode = 33000, fyear = "'平成29年'", posLevel = 0) {
  r <-
    httr::GET("http://nlftp.mlit.go.jp/isj/api/1.0b/index.php/app/getISJURL.xml",
              query = list(appId = "isjapibeta1",
                           areaCode = paste(areaCode, collapse = ","),
                           fiscalyear = fyear,
                           posLevel = posLevel))
  
  r_xml <-
    xml2::as_list(content(r, encoding = "UTF-8"))
  
  r_xml2_items <-
    purrr::pluck(purrr::pluck(r_xml, "ISJ_URL_INF"), "ISJ_URL")
  
  df_isj <-
    tibble::tibble(item = r_xml2_items)
  
  df_isj %>%
    tidyr::hoist(item,
                 fyear = list("fiscalyear", 1L),
                 prefCode = list("prefCode", 1L),
                 posLevel = list("posLevel", 1L),
                 prefecture = list("prefName", 1L),
                 city = list("cityName", 1L),
                 ver = list("verNumber", 1L),
                 zipFileUrl = list("zipFileUrl", 1L)) %>%
    dplyr::select(-item)
}

# 全国の市町村ダウンロード ------------------------------------------------------------
# 平成29年 (2017)
source("https://gist.githubusercontent.com/uribo/4bdf76e07399ad75e9896763dd24aa60/raw/9dd1ae700afbdfc64ffe642012ab6372cc722c04/ksj_collect_n03.R")

files <-
  fs::dir_ls("~/Documents/resources/国土数値情報/N03/2017/",
             recurse = TRUE,
             regexp = ".shp") %>%
  ensurer::ensure(length(.) == 47L)

jp_city_codes <- files %>%
  purrr::map(
    ~ read_ksj_n03(.x) %>%
      dplyr::pull(administrativeAreaCode)
  ) %>%
  purrr::reduce(c) %>%
  unique()

req_isj(areaCode = c(na.omit(jp_city_codes[1401:1903])),
        "'平成29年'", 0) %>%
  dplyr::pull(zipFileUrl) %>%
  purrr::walk(
    ~ curl::curl_download(
      .x,
      destfile = paste0("~/Documents/resources/位置参照情報/街区レベル位置参照情報/h29/",
                        basename(.x))))

fs::dir_ls("~/Documents/resources/位置参照情報/街区レベル位置参照情報/h29/", regexp = "-16.0a.zip") %>%
  purrr::walk(
    ~ unzip(zipfile = .x,
            exdir = "~/Documents/resources/位置参照情報/街区レベル位置参照情報/h29/"))

fs::dir_ls("~/Documents/resources/位置参照情報/街区レベル位置参照情報/h29/",
           regexp = ".(xml|html)$",
           recurse = TRUE) %>%
  unlink()


# a. 街区レベル -----------------------------------------------------------------------
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
