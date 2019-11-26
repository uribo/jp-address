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
  city_name_regex <- 
    "(岡山市南区|(飯能|上尾|安中|坂井|町田|市原|市川|村上)市|村山市|余市郡(余市町|仁木町|赤井川村)|(余市|高市)郡.+(町|村)|柴田郡(村田町|大河原町)|(武蔵|東)村山市|西村山郡河北町|北村山郡大石田町|(東|西|北)村山郡.+(町|村)|田村(市|郡..町)|芳賀郡市貝町|(佐波郡)?玉村町|[羽大]村市|(十日|大)町市|(中新川郡)?上市町|(野々|[四廿]日)市市|西八代郡市川三郷町|神崎郡市川町|高市郡(高取町|明日香村)|(吉野郡)?下市町|(杵島郡)?大町町|(南相馬|北上|奥州|那須塩原|印西|上越|姫路|玉野|山口|佐伯|北見|士別|富良野|旭川|伊達|石狩|富山|黒部|小諸|塩尻|松阪|豊川|福知山|周南|西海|別府)市|(千代田|中央|港|新宿|文京|台東|墨田|江東|品川|目黒|大田|世田谷|渋谷|中野|杉並|豊島|北|荒川|板橋|練馬|足立|葛飾|江戸川)区|.+市.+区|市|町|村)"
  split_pref = 
    stringr::str_split(x, stringr::regex("(?<=(東京都|道|府|県))"), n = 2, simplify = TRUE) %>% 
    stringr::str_subset(".{1}", negate = FALSE)
  if (length(split_pref) == 1L) {
    split_pref[2] <- split_pref
    split_pref[1] <- NA_character_
  }
  if (length(split_pref[2] %>% 
             stringr::str_split(city_name_regex,
                                n = 2, 
                                simplify = TRUE) %>% 
             stringr::str_subset(".{1}", negate = FALSE)) == 0L) {
    res <- 
      list(
        prefecture = split_pref[1],
        city = 
          split_pref[2] %>% 
          stringr::str_replace(city_name_regex, 
                               replacement = "\\1"),
        street = 
          split_pref[2] %>% 
          stringr::str_remove(
            split_pref[2] %>% 
              stringr::str_replace(city_name_regex, 
                                   replacement = "\\1")
          )
      )  
  } else {
    res <- 
      list(
        prefecture = split_pref[1],
        city = 
          split_pref[2] %>% 
          stringr::str_replace(
            paste0(city_name_regex, "(.+)"), 
            replacement = "\\1"),
        street = 
          split_pref[2] %>% 
          stringr::str_remove(
            split_pref[2] %>% 
              stringr::str_replace(
                paste0(city_name_regex, "(.+)"), 
                replacement = "\\1")
          )
      )
  }
  res %>% 
    purrr::map(
      ~ dplyr::if_else(.x == "", NA_character_, .x)
    )
}
