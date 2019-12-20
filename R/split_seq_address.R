# split_seq_address(str = "吾妻1、2丁目", split_chr = "、", prefix = "吾妻", suffix = "丁目", seq = FALSE)
# split_seq_address(str = "1〜4丁目", split_chr = "〜", prefix = NULL, suffix = "丁目")
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
                       ~ stringr::str_c(prefix, .x))
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
split_multiple_address <- function(str, delim = "、", prefix = NULL, suffix = NULL) {
  x <- 
    stringr::str_c(str, collapse = "") %>%
    stringr::str_split(delim, simplify = TRUE) %>% 
    as.vector()
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
  str <- 
    stringr::str_extract(str, "(?<=\\().*?(?=\\))")
  split_multiple_address(str, delim = delim, prefix = prefix, suffix = suffix)
}
