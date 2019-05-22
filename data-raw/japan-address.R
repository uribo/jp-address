################################
# 日本郵便
# 郵便番号データダウンロード
################################
if (length(fs::dir_ls(here::here("data-raw/japanpost_kogaki/"), regexp = ".CSV$")) != 47) {
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
  
  if (dir.exists(here::here("data-raw/japanpost_kogaki")) == FALSE)
    dir.create(here::here("data-raw/japanpost_kogaki"))
  
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
                            destfile = here::here("data-raw", basename(.x)))
    )
  
  here::here("data-raw", basename(target_files)) %>% 
    purrr::walk(
      ~ unzip(zipfile = .x, 
              exdir = here::here("data-raw/japanpost_kogaki"))
    )  
}
