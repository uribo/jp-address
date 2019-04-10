modified_address  <- function(city, adds, target_city, fix_adds, replace = NULL) {
  
  if (is.null(replace)) replace = fix_adds
  
  dplyr::if_else(
    stringr::str_detect(city, target_city),
    stringr::str_replace(adds, glue::glue(fix_adds, ".+"), replace),
    adds)  
}
