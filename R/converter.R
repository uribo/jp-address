convert_ga <- function(x, small = TRUE) {
  
  if (rlang::is_false(stringr::str_detect(x, "\u30b1|\u30f6")))
    rlang::abort("Don't contains \u30b1 or \u30f6")
  
  if (rlang::is_true(small))
    stringr::str_replace_all(x, pattern = "\u30b1", replacement = "\u30f6")
  else
    stringr::str_replace_all(x, pattern = "\u30f6", replacement = "\u30b1")
  
}

convert_e <- function(x, small = TRUE) {
  
  if (rlang::is_false(stringr::str_detect(x, "\u30a8|\u30a7")))
    rlang::abort("Don't contains \u30a8 or \u30a7")
  
  if (rlang::is_true(small))
    stringr::str_replace_all(x, pattern = "\u30a8", replacement = "\u30a7")
  else
    stringr::str_replace_all(x, pattern = "\u30a7", replacement = "\u30a8")
  
}

convert_kanji <- function(x, jyoyo = TRUE) {
  
  t_s <- "Traditional-Simplified"
  s_t <- "Simplified-Traditional"
  
  if (rlang::is_true(jyoyo)) {
    stringr::str_replace_all(x, 
                             "\u5f4c", 
                             stringi::stri_trans_general("\u5f4c", 
                                                         t_s))
  } else {
    stringr::str_replace_all(x, 
                             "\u5f25", 
                             stringi::stri_trans_general("\u5f25", 
                                                         s_t))
  }
}

