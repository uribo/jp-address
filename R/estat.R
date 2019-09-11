estat_district <- function(path) {
  d <-
    sf::st_read(path,
                stringsAsFactors = FALSE,
                as_tibble = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::select(-area, -perimeter, -ken, -ken_name, -dummy1, -area_max_f,
                  -n_ken, -n_city, -kbsum, -x_code, -y_code, -kcode1,
                  -h27k_axx_id, -h27k_axx,
                  -kigo_e, -kigo_d, -kigo_i, -hcode,
                  -keycode1, -keycode2)
  d <-
    d %>%
    dplyr::select(pref, pref_name, city, city_name, s_name, jinko, setai, kihon1) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(city, kihon1) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(d %>%
                       dplyr::group_by(city, kihon1) %>%
                       dplyr::summarise() %>%
                       dplyr::ungroup(),
                     by = c("city", "kihon1")) %>%
    sf::st_sf() %>%
    dplyr::mutate_if(is.character, stringi::stri_trans_nfkc)
  d %>%
    dplyr::mutate(s_name = stringr::str_replace_all(s_name, "ノ", "の"),
                  s_name = dplyr::if_else(stringr::str_detect(s_name, "丁目$"),
                                   stringr::str_remove(s_name, "([一二三四五六七八九十壱弐参拾百千万萬億兆〇]|[0-9])丁目$"),
                                   s_name),
                  s_name = stringr::str_remove_all(s_name, "[A-Z]$"))
}
