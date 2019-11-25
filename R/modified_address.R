modified_address  <- function(city, adds, target_city, fix_adds, replace = NULL) {
  
  if (is.null(replace)) replace = fix_adds
  
  dplyr::if_else(
    stringr::str_detect(city, target_city),
    stringr::str_replace(adds, glue::glue(fix_adds, ".+"), replace),
    adds)  
}

separate_address_cols <- function(df, col = "address", into = c("prefecture", "address")) {
  df %>% 
    tidyr::separate(col = col,
                    into = into,
                    sep = "(?<=東京都|北海道|(大阪|京都)府|(青森|岩手|宮城|秋田|山形|福島|茨城|栃木|群馬|埼玉|千葉|神奈川|新潟|富山|石川|福井|山梨|長野|岐阜|静岡|愛知|三重|滋賀|兵庫|奈良|和歌山|鳥取|島根|岡山|広島|山口|徳島|香川|愛媛|高知|福岡|佐賀|長崎|熊本|大分|宮崎|鹿児島|沖縄)県)",
                    extra = "merge")
}

#' @examples 
#' separate_address("北海道札幌市中央区北1条西2丁目")
separate_address <- function(x) {
  split_pref = 
    stringr::str_split(x, stringr::regex("(?<=(都|道|府|県))"), n = 2, simplify = TRUE) %>% 
    stringr::str_subset("...{1}", negate = FALSE)
  if (length(split_pref) == 1L) {
    split_pref[2] <- split_pref
    split_pref[1] <- NA_character_
  }
  list(
    pref = split_pref[1],
    city = 
      split_pref[2] %>% 
      stringr::str_replace("(.+市.+区|市|町|村).+", replacement = "\\1"),
    street = 
      split_pref[2] %>% 
      stringr::str_remove(
        split_pref[2] %>% 
          stringr::str_replace("(.+市.+区|市|町|村).+", replacement = "\\1")
      )
  ) %>% 
    purrr::map(
      ~ dplyr::if_else(.x == "", NA_character_, .x)
    )
}
