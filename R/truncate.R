truncate_address <- function(x, level = "city") {
  
  rlang::arg_match(level)
  
  if (rlang::is_true(is_ward(x))) {
    stringr::str_split(x, 
                       pattern = "(?<=[[:print:]]市[[:print:]]区|[[:print:]]区|[[:print:]]町|[[:print:]]村)", n = 2) %>%
      purrr::pluck(1) %>% 
      purrr::pluck(1)
  } else if (rlang::is_true(is_include_level_char(x))) {
    
    if (stringr::str_detect(x, "佐波郡玉村町|大村市|.+(郡市貝町|郡赤井川村|郡村田町|村山郡|市郡明日香村)"))
      stringr::str_split(x,
                         pattern = "(?<=佐波郡玉村町|大村市|[[:print:]]郡市貝町|[[:print:]]郡村田町|[[:print:]]村山郡(河北町|西川町|朝日町|大江町)|高市郡明日香村|余市郡赤井川村)") %>% 
      purrr::pluck(1) %>% 
      purrr::pluck(1)
    else
      stringr::str_split(x,
                         pattern = "(?<=薩摩川内市|安来市|木更津市|[[:print:]]市市|[[:print:]]市町|[[:print:]]町市|[[:print:]]町町|[[:print:]]郡村)", n = 2) %>% 
      purrr::pluck(1) %>% 
      purrr::pluck(1)
  } else {
    stringr::str_split(x,
                       pattern = "(?<=[[:print:]]市|[[:print:]]区|[[:print:]]町|[[:print:]]村)", n = 2) %>% 
      purrr::pluck(1) %>% 
      purrr::pluck(1)
  }
}
