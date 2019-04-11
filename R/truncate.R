truncate_address <- function(x, level = "city") {
  
  rlang::arg_match(level)
  
  if (rlang::is_true(is_ward(x))) {
    stringr::str_split(x, 
                       pattern = "(?<=[[:print:]]市[[:print:]]区|[[:print:]]区|[[:print:]]町|[[:print:]]村)", n = 2) %>%
      purrr::pluck(1) %>% 
      purrr::pluck(1)
  } else if (rlang::is_true(is_include_level_char(x))) {
    
    if (stringr::str_detect(x, ".+(郡村田町|村山郡)"))
      stringr::str_split(x,
                         pattern = "(?<=[[:print:]]郡村田町|[[:print:]]村山郡)") %>% 
      purrr::pluck(1) %>% 
      purrr::pluck(1)
    else
      stringr::str_split(x,
                         pattern = "(?<=[[:print:]]市市|[[:print:]]市町|[[:print:]]町町|[[:print:]]郡村)", n = 2) %>% 
      purrr::pluck(1) %>% 
      purrr::pluck(1)
  } else {
    stringr::str_split(x,
                       pattern = "(?<=[[:print:]]市|[[:print:]]区|[[:print:]]町|[[:print:]]村)", n = 2) %>% 
      purrr::pluck(1) %>% 
      purrr::pluck(1)
  }
}
