################################
# 日本郵便
# 郵便番号データダウンロード
################################
if (length(fs::dir_ls(here::here("data-raw"), regexp = "japanpost_")) != 2) {
  library(rvest)
  library(ensurer)
  
  if (rlang::is_false(dir.exists(here::here("data-raw"))))
    dir.create(here::here("data-raw"))
  
  japan_post_site <- "https://www.post.japanpost.jp"
  
  target_urls <- 
    glue::glue(japan_post_site, "/zipcode/download.html") %>% 
    read_html() %>% 
    html_nodes(css = '#main-box > div > ul > li > a') %>% 
    html_attr(name = "href") %>% 
    stringr::str_subset("bangobo", negate = TRUE) %>% 
    paste0(japan_post_site, .)
  # 1. 住所の郵便番号 --------------------------------------------------------------
  if (dir.exists(here::here("data-raw/japanpost_kogaki")) == FALSE)
    dir.create(here::here("data-raw/japanpost_kogaki"))
  if (length(fs::dir_ls(here::here("data-raw/japanpost_kogaki/"), regexp = ".CSV$")) != 47) {
    target_files <- 
      target_urls %>% 
      stringr::str_subset("kogaki-zip.html") %>% 
      read_html() %>% 
      html_nodes(css = '#main-box > div > table:nth-child(7) > tbody > tr > td > a') %>% 
      html_attr(name = "href") %>% 
      stringr::str_subset("ken_all.zip", negate = TRUE) %>% 
      ensure(length(.) == 47L)
    target_files %>% 
      xml2::url_absolute(base = glue::glue(japan_post_site, "/zipcode/dl/")) %>% 
      purrr::walk(
        ~ curl::curl_download(.x,
                              destfile = here::here("data-raw/japanpost_kogaki",
                                                    basename(.x))))
    here::here("data-raw/japanpost_kogaki/", basename(target_files)) %>% 
      purrr::walk(
        ~ unzip(zipfile = .x, 
                exdir = here::here("data-raw/japanpost_kogaki")))
  }
  # 2. 事業所 ------------------------------------------------------------------
  if (dir.exists(here::here("data-raw/japanpost_jigyosyo")) == FALSE)
    dir.create(here::here("data-raw/japanpost_jigyosyo"))
  if (length(fs::dir_ls(here::here("data-raw/japanpost_jigyosyo"), regexp = ".CSV$")) != 47L) {
    # 最新データのダウンロード
    target_files <- 
      target_urls %>% 
      stringr::str_subset("jigyosyo/index-zip.html") %>% 
      read_html() %>% 
      html_nodes(css = '#main-box > div > div:nth-child(9) > ul > li > a') %>% 
      html_attr(name = "href") %>% 
      ensure(length(.) == 1L)
# 正解... https://www.post.japanpost.jp/zipcode/dl/jigyosyo/zip/jigyosyo.zip
    #     https://www.post.japanpost.jp/zipcode/dl/jigyosyo/zip/jigyosyo.zip
    target_files %>% 
      glue::glue(glue::glue(japan_post_site, "/zipcode/dl/jigyosyo/"), .) %>% 
      curl::curl_download(url = .,
                          destfile = paste0(here::here("data-raw/japanpost_jigyosyo/"), 
                                            basename(.)))
    unzip(zipfile = here::here("data-raw/japanpost_jigyosyo/", 
                               basename(target_files)), 
          exdir = here::here("data-raw/japanpost_jigyosyo"))
  }
}
