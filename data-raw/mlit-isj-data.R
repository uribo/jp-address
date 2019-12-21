################################
# 国土交通省 位置参照情報ダウンロードサービス
# http://nlftp.mlit.go.jp/isj/
# last update: 2019-12-18 (平成29年版)
################################
library(dplyr)
library(readr)
library(assertr)
library(ensurer)
library(kuniumi)

# download ----------------------------------------------------------------
# posLevel... 0: 街区レベル、1: 大字・町丁目レベル
req_isj <- function(areaCode = 33000, fyear = c("平成29年", "平成30年"), posLevel = c(0, 1)) {
  r <-
    httr::GET("http://nlftp.mlit.go.jp/isj/api/1.0b/index.php/app/getISJURL.xml",
              query = list(appId = "isjapibeta1",
                           areaCode = paste(areaCode, collapse = ","),
                           fiscalyear = paste0("'", fyear, "'"),
                           posLevel = posLevel))
  r_xml <-
    xml2::as_list(httr::content(r, encoding = "UTF-8"))
  status <- 
    purrr::pluck(purrr::pluck(purrr::pluck(purrr::pluck(r_xml, "ISJ_URL_INF"), "RESULT"), "STATUS"), 1)
  if (status == "1") {
    tibble::tibble(
      fyear = fyear,
      prefCode = as.numeric(substr(areaCode, 1, 2)),
      posLevel = posLevel,
      prefecture = zipangu::jpnprefs[[as.numeric(substr(areaCode, 1, 2)), "prefecture_kanji"]],
      city = NA_character_,
      ver = NA_character_,
      zipFileUrl = NA_character_
    )
  } else if (status == "0") {
    r_xml2_items <-
      purrr::pluck(purrr::pluck(r_xml, "ISJ_URL_INF"), "ISJ_URL")
    df_isj <-
      tibble::tibble(item = r_xml2_items)
    df_isj %>%
      tidyr::hoist(item,
                   fyear      = list("fiscalyear", 1L),
                   prefCode   = list("prefCode", 1L),
                   posLevel   = list("posLevel", 1L),
                   prefecture = list("prefName", 1L),
                   city       = list("cityName", 1L),
                   ver        = list("verNumber", 1L),
                   zipFileUrl = list("zipFileUrl", 1L)) %>%
      dplyr::select(-item)
  }
}
merge_isj_csv <- function(dir, posLevel = c(0, 1)) {
  files <- 
    fs::dir_ls(dir, 
               regexp = ".csv$",
               recurse = TRUE)
  isj_colnames <- 
    list(
      a = c("prefecture", "city", 
            "street_lv1", # ... "大字_町丁目名"
            "street_lv2", # ... "小字_通称名",
            "street_lv3", # ... "街区符号_地番"
            "cs_num", 
            "coord_x", "coord_y",
            "latitude", "longitude", 
            "住居表示フラグ", "代表フラグ",
            "更新前履歴フラグ", "更新後履歴フラグ"),
      b = c("prefecture_code", "prefecture", 
            "city_code", "city",
            "street_lv1_code", # "大字_町丁目コード"
            "street_lv1", # ... "大字_町丁目名"
            "latitude", "longitude",
            "原典資料コード",
            "street_levels"))
  if (posLevel == 0) {
    files %>% 
      purrr::map_dfr(
        ~ readr::read_csv(.x,
                          locale = readr::locale(encoding = "cp932"),
                          col_names = isj_colnames$a) %>%
          dplyr::slice(-1L) %>%
          # dplyr::mutate_at(dplyr::vars(prefecture, city, 
          #                              num_range("street_lv", seq(1, 3))), 
          #                  as.character) %>%
          dplyr::mutate_at(dplyr::vars(cs_num, coord_x, coord_y,
                                       latitude, longitude,
                                       `住居表示フラグ`, `代表フラグ`,
                                       `更新前履歴フラグ`,`更新後履歴フラグ`),
                           as.double))
  } else if (posLevel == 1) {
    files %>% 
      purrr::map_dfr(
        ~ readr::read_csv(.x,
                          locale = locale(encoding = "cp932"),
                          col_names = isj_colnames$b,
                          skip = 1) %>%
          select(prefecture, city_code, city, 
                 street_lv1_code, street_lv1,
                 longitude, latitude, street_levels) %>% 
          mutate_at(vars(prefecture, city_code, city, 
                         street_lv1_code, street_lv1, 
                         street_levels),
                    as.character) %>% 
          mutate_at(vars(longitude, latitude), as.double))  
  }
}

# 全国の市町村ダウンロード ------------------------------------------------------------
# 平成30年 (2018)
# resourceからコピーしておく
extract_city_codes <- function(path) {
  read_ksj_n03(path) %>%
    dplyr::pull(administrativeAreaCode) %>% 
    unique() %>% 
    na.omit()
}
download_isj_zip <- function(path, dest_path, level = c("街区レベル", "大字・町丁目レベル"), year = c("h29", "h30")) {
  curl::curl_download(
    path,
    destfile = paste0(here::here(glue::glue("{dest_path}/{level}/{year}/")),
                      basename(path)))
  fs::dir_ls(here::here(glue::glue("{dest_path}/{level}/{year}/")), 
             regexp = basename(path)) %>%
    purrr::walk(
      ~ unzip(zipfile = .x,
              exdir = here::here(glue::glue("{dest_path}/{level}/{year}/"))))
  fs::dir_ls(here::here(glue::glue("{dest_path}/{level}/{year}")),
             regexp = ".(xml|html)$",
             recurse = TRUE) %>%
    unlink()
}
task_isj_download <- function(dir, city, jyear, posLevel = c(0, 1)) {
  type <- 
    dplyr::case_when(
      posLevel == 0 ~ "街区レベル",
      posLevel == 1 ~ "大字・町丁目レベル")
  base_dir <- 
    here::here(glue::glue("{dir}/{type}/{jyear}"))
  dir.create(base_dir, 
             recursive = TRUE)
  download_zip <- 
    purrr::slowly(~ download_isj_zip(path = .x,
                                     dest_path = dir,
                                     level = type,
                                     year = jyear), 
                  rate = purrr::rate_delay(pause = 3), 
                  quiet = FALSE)
  df_isj_dl <- 
    city %>% 
    purrr::map(
      ~ req_isj(areaCode = .x,
                fyear = paste0("平成", readr::parse_number(jyear), "年"), 
                posLevel = posLevel)) %>% 
    purrr::reduce(rbind)
  df_isj_dl %>% 
    dplyr::filter(!is.na(zipFileUrl)) %>% 
    dplyr::pull(zipFileUrl) %>% 
    purrr::walk(
      ~ download_zip(.x))
}
jp_city_codes <- 
  fs::dir_ls(here::here("data-raw/国土数値情報/N03/2018/"),
             recurse = TRUE,
             regexp = ".shp") %>% 
  ensure(length(.) == 47L) %>% 
  purrr::map(
    extract_city_codes) %>% 
  purrr::reduce(c) %>% 
  ensure(length(.) == 1902L)

if (file.exists(here::here("data-raw/isj_2018a.rds")) == FALSE) {
  task_isj_download("data-raw/位置参照情報/", jp_city_codes, "h30", posLevel = 0)
  df_isj_a <-
    merge_isj_csv(here::here("data-raw/位置参照情報/街区レベル/h30/"),
                  posLevel = 0) %>% 
    verify(dim(.) == c(19603996, 14))
  # remove coord_x and coord_y
  df_isj_a %>% 
    select(prefecture, city, 
           num_range("street_lv", 1:3),
           cs_num, longitude, latitude,
           contains("フラグ")) %>% 
    readr::write_rds(here::here("data-raw/isj_2018a.rds"), compress = "xz")
}

if (file.exists(here::here("data-raw/isj_2018b.rds")) == FALSE) {
  task_isj_download("data-raw/位置参照情報/", jp_city_codes, "h30", posLevel = 1)
  df_isj_b <-
    merge_isj_csv(here::here("data-raw/位置参照情報/大字・町丁目レベル/h30/"),
                  posLevel = 1) %>% 
    verify(dim(.) == c(189817, 8))
  df_isj_b %>% 
    select(city, contains("street_"), longitude, latitude) %>% 
    verify(ncol(.) == 6L) %>% 
    readr::write_rds(here::here("data-raw/isj_2018b.rds"), compress = "xz")
}

# 平成29年 (2017)
# resourceからコピーしておく
files <- 
  fs::dir_ls(here::here("data-raw/国土数値情報/N03/2017/"),
             recurse = TRUE,
             regexp = ".shp")
if (length(files) != 47) {
  if (file.exists(here::here("data-raw/.gitignore")) == FALSE)
    write(cat("国土数値情報/\n"), file = here::here("data-raw/.gitignore"))
  usethis::use_git_ignore("国土数値情報/", directory = "data-raw")
  dl_urls <-
    ksj_data_url("N03", fiscalyear = 2017) %>%
    dplyr::pull(zipFileUrl) %>%
    ensurer::ensure(length(.) == 48L)
  dl_urls %>%
    purrr::walk(
      ~ curl::curl_download(.x,
                            destfile = paste0(here::here("data-raw/国土数値情報/N03/2017/"),
                                              basename(.x[1]))))
  dl_files <- 
    fs::dir_ls(here::here("data-raw/国土数値情報/N03/2017"),
               regexp = ".+zip")
  dl_files %>%
    purrr::walk(
      ~ unzip(zipfile = .x,
              exdir = paste0(here::here("data-raw/国土数値情報/N03/2017/"),
                             stringr::str_remove(basename(.x), ".zip"))))
  unlink(dl_files)
  files <-
    fs::dir_ls(here::here("data-raw/国土数値情報/N03/2017/"),
               recurse = TRUE,
               regexp = ".shp")
}
jp_city_codes <- 
  files %>% 
  ensure(length(.) == 47L) %>% 
  purrr::map(
    extract_city_codes) %>% 
  purrr::reduce(c) %>% 
  ensure(length(.) == 1902L)



if (file.exists(here::here("data-raw/isj_2017a.rds")) == FALSE) {
  task_isj_download(dir = "data-raw/位置参照情報/", city = jp_city_codes, jyear = "h29", posLevel = 0)
  df_isj_a <-
    merge_isj_csv(here::here("data-raw/位置参照情報/街区レベル/h29/"),
                  posLevel = 0) %>% 
    verify(dim(.) == c(20697813, 14))
  df_isj_a %>% 
    select(prefecture, city, 
           num_range("street_lv", 1:3),
           cs_num, longitude, latitude,
           contains("フラグ")) %>% 
    verify(dim(.) == c(20697813, 12)) %>% 
    readr::write_rds(here::here("data-raw/isj_2017a.rds"), compress = "xz")
} else {
  df_isj_a <- 
    readr::read_rds(here::here("data-raw/isj_2017a.rds"))
}
# 大字・町丁目レベル ----------------------------------------------------------------------
# 大字・町丁目レベル のダウンロード posLevel = 1
# req_isj("33000", "平成29年", posLevel = 1)
# street_level_code <- 
#   list(
#     "大字" = 1,
#     "字" = 2,
#     "丁目" = 3,
#     "不明(通称)" = 0)
if (rlang::is_false(file.exists(here::here("data-raw/isj_2017b.rds")))) {
  task_isj_download(dir = "data-raw/位置参照情報/", city = jp_city_codes, jyear = "h29", posLevel = 1)
  df_isj_b <- 
    merge_isj_csv(here::here("data-raw/位置参照情報/大字・町丁目レベル/h29"),
                  posLevel = 1) %>% 
    verify(dim(.) == c(189539, 8))
  df_isj_b %>% 
    select(city, contains("street_"), longitude, latitude) %>% 
    verify(ncol(.) == 6L) %>% 
    readr::write_rds(here::here("data-raw/isj_2017b.rds"), compress = "xz")
} else {
  df_isj_b <- 
    readr::read_rds(here::here("data-raw/isj_2017b.rds"))  
}
