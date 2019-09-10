##################################
# e-stat小地域
##################################
library(sf)
library(dplyr)
source(here::here("data-raw/japan-address.R"))

# sf_estat_pref08 <- 
#   st_read("~/Documents/resources/e-Stat/2015_国勢調査_小地域/A002005212015DDSWC08/h27ka08.shp",
#           stringsAsFactors = FALSE,
#           as_tibble = TRUE) %>% 
#   janitor::clean_names() %>% 
#   # filter(city == "220") %>% 
#   select(-area, -perimeter, -ken, -ken_name, -dummy1, -area_max_f,
#          -n_ken, -n_city, -kbsum, -jinko, -x_code, -y_code, -kcode1,
#          -h27k_axx_id, -h27k_axx, -setai,
#          -kigo_e, -kigo_d, -kigo_i, -hcode,
#          -keycode1, -keycode2)
# 
# 
# sf_estat_pref08 %>% 
#   select(city_name, s_name, kihon1) %>% 
#   st_drop_geometry() %>% 
#   group_by(city_name, kihon1) %>% 
#   slice(1L) %>% 
#   ungroup() %>% 
#   filter(s_name == "谷田部")
# 
# sf_estat_pref08 <- 
#   sf_estat_pref08 %>% 
#   select(city_name, s_name, kihon1) %>% 
#   st_drop_geometry() %>% 
#   group_by(kihon1) %>% 
#   slice(1L) %>% 
#   ungroup() %>% 
#   left_join(sf_estat_pref08 %>% 
#               group_by(kihon1) %>% 
#               summarise() %>% 
#               ungroup(),
#             by = "kihon1") %>% 
#   st_sf() %>% 
#   mutate_if(is.character, stringi::stri_trans_nfkc)

# sf_estat_pref08 <- 
#   sf_estat_pref08 %>% 
#   mutate(s_name = stringr::str_replace_all(s_name, "ノ", "の"),
#          s_name = if_else(stringr::str_detect(s_name, "丁目$"),
#                           stringr::str_remove(s_name, "([一二三四五六七八九十壱弐参拾百千万萬億兆〇]|[0-9])丁目$"),
#                           s_name),
#          s_name = stringr::str_remove_all(s_name, "[A-Z]$")) %>% 
#   mutate(s_name = recode(s_name,
#                          `上の室` = "上ノ室"))
# 
sf_estat_pref08 <- 
  estat_district(path = "~/Documents/resources/e-Stat/2015_国勢調査_小地域/A002005212015DDSWC08/h27ka08.shp")

sf_estat_city08220 <- 
  sf_estat_pref08 %>% 
  filter(city == "220") %>% 
    mutate(s_name = recode(s_name,
                           `上の室` = "上ノ室"))

# estat_district("~/Documents/resources/e-Stat/2015_国勢調査_小地域/A002005212015DDSWC33/h27ka33.shp")

# お試し: 茨城県つくば市 ---------------------------------------------------------------------
# sf_bを使う
df_zip08 <-
  df_japanpost_zip %>% 
  filter(prefecture == "茨城県", city == "つくば市",
         street != "以下に掲載がない場合",
         is_jigyosyo == FALSE) %>% 
  distinct(street, .keep_all = TRUE)

sf_a <- 
  df_zip08 %>%
  inner_join(sf_estat_city08220,
            by = c("city" = "city_name",
                   "street" = "s_name")) %>% 
  st_sf()

sf_b <- 
  df_zip08 %>%
  right_join(sf_estat_city08220,
            by = c("city" = "city_name",
                   "street" = "s_name")) %>%
  mutate(zip_code = if_else(is.na(zip_code), "305-0000", zip_code)) %>% 
  st_sf()

mapview::mapview(sf_b) +
  mapview::mapview(sf_estat_city08220)

# sf_estat_city08220 %>% 
#   st_join(st_sf(st_union(sf_a)), join = st_disjoint, left = TRUE) %>% 
#   mapview::mapview()



df_zip08 %>%
  anti_join(sf_estat_city08220,
             by = c("city" = "city_name",
                    "street" = "s_name")) %>% 
  pull(street)
#  [1] "かみかわ"   "五人受"     "さくらの森" "高山"       "中東"       "西の沢"     "西の沢"     "花園"       "春風台"     "万博公園西" "本沢"       "流星台"    

# 305-0000 とする
sf_estat_city08220 %>% 
  anti_join(df_zip08,
            by = c("city_name" = "city",
                   "s_name" = "street")) %>% 
  mapview::mapview()


sf_estat_city08220 %>% 
  select(s_name, kihon1) %>% 
  distinct(kihon1, .keep_all = TRUE)

sf_estat_city08220 %>% 
  filter(stringr::str_detect(s_name, "春風台")) %>% glimpse()
sf_estat_city08220 %>% 
  filter(stringr::str_detect(s_name, "谷田部")) %>% 
  mapview::mapview()


df_zip08 %>% 
  filter(stringr::str_detect(street, "和台"))

sf_estat_city08220 %>% 
  filter(stringr::str_detect(s_name, "和台")) %>% 
  mapview::mapview()
