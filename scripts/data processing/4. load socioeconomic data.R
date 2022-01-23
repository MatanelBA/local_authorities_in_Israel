

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr)

rm(list = objects())

getwd()

list.files("originals/socioeconomic attributes by geo area/2008/")

#2017
setwd("originals/socioeconomic attributes by geo area/2017/")

col_names_geo_area_2017 <- c("city_code", "city_name", "geo_area_code", "index_population", "index_value", "rank", "cluster",
                             "median_age",               "median_age_std",              "median_age_rank",
                             "dependency_ratio",         "dependency_ratio_std",        "dependency_ratio_rank",
                             "percent_families_4_",      "percent_families_4_std",      "percent_families_4_rank",
                             "avg_years_scooling",       "avg_years_scooling_std",      "avg_years_scooling_rank",
                             "percent_degree",           "percent_degree_std",          "percent_degree_rank",
                             "percent_have_earning",     "percent_have_earning_std",    "percent_have_earning_rank",
                             "percent_women_no_earning", "percent_women_noearning_std", "percent_women_no_earning_rank",
                             "percent_high_earning",     "percent_high_earning_std",    "percent_high_earning_rank",
                             "percent_low_earning",      "percent_low_earning_std",     "percent_low_earning_rank",
                             "percent_get_support",      "percent_get_support_std",     "percent_get_support_rank",
                             "avg_income_per_capita",    "avg_income_per_std",          "avg_income_per_rank",
                             "rate_owned_viechles",      "rate_owned_std",              "rate_owned_rank",
                             "avg_viechles_license",     "avg_viechles_std",            "avg_viechles_rank",
                             "avg_days_abroad",          "avg_days_abroad_std",         "avg_days_abroad_rank",
                             "city_name_english") 

col_names_local_authorities_2017 <-  c("municipal_code", col_names_geo_area_2017[-3])


col_names_settelments_2017 <-  c("municipal_code", "drop_1",          "drop_2", "drop_3", "drop_4",
                                 "city_code",        "city_name",        "city_name_english",
                                 "settlement_type",  "index_population", "index_value",
                                 "rank",             "cluster",          "drop_5")


socioeconomic_local_authorities_raw_2017 <- read_xls("t01.xls",  na = c("", ".."), col_names = col_names_local_authorities_2017)
socioeconomic_settelments_raw_2017       <- read_xls("t07.xls",  na = c("", ".."), col_names = col_names_settelments_2017)
socioeconomic_geo_area_raw_2017          <- read_xls("t12.xls",  na = c("", ".."), col_names = col_names_geo_area_2017)

setwd("./../../..")

#2015

setwd("originals/socioeconomic attributes by geo area/2015/")
col_names_settelments_2015 <- c(col_names_settelments_2017[-c(4,5,9)],"drop_6")

col_names_geo_area_2015 <-   c(col_names_geo_area_2017[1:7] , "drop_1")

socioeconomic_local_authorities_raw_2015 <- read_xlsx("t01.xlsx",         na = c("", ".."), col_names = col_names_local_authorities_2017)
socioeconomic_settelments_raw_2015       <- read_xlsx("t07.xlsx",         na = c("", ".."), col_names = col_names_settelments_2015)
socioeconomic_geo_area_raw_2015          <- read_xls ("24_19_246t1.xls",  na = c("", ".."), col_names = col_names_geo_area_2015) # כאן חסרים משתני העזר
 
setwd("./../../..")

#2013
setwd("originals/socioeconomic attributes by geo area/2013/")

col_names_settelments_2013 <- c("municipal_code",     "drop_1",            "drop_2" ,
                                "drop_3",            "drop_4",             "city_code",
                                "city_name" ,         "city_name_english", "settlement_type",
                                "index_population", "index_value",       "cluster" ) 

socioeconomic_local_authorities_raw_2013 <- read_xls("t01.xls", na = c("", ".."), col_names = col_names_local_authorities_2017)
socioeconomic_settelments_raw_2013       <- read_xls("t07.xls", na = c("", ".."), col_names = col_names_settelments_2013)

setwd("./../../..")

#2008
# setwd("originals/socioeconomic attributes by geo area/2008/")
# המשתנים ששימשו לחישוב המדד היו שונים בשנה זו. דורש להזין מחדש את שמות העמודות
# socioeconomic_local_authorities_raw_2008 <- read_xls("tab01_01.xls")
# socioeconomic_geo_area_raw_2008          <- read_xls("tab02_01.xls")
# 
# setwd("./../../..")



#merge 

socioeconomic_local_authorities_raw <- bind_rows(socioeconomic_local_authorities_raw_2013,
                                                 socioeconomic_local_authorities_raw_2015,
                                                 socioeconomic_local_authorities_raw_2017,
                                                 .id = "year") %>%
                                       mutate(year = case_when(year == "1" ~ 2013,
                                                               year == "2" ~ 2015,
                                                               year == "3" ~ 2017 ))
                                            

socioeconomic_geo_area_raw <- bind_rows(socioeconomic_geo_area_raw_2015,
                                        socioeconomic_geo_area_raw_2017,
                                                 .id = "year")  %>%
                               mutate(year = case_when(year == "1" ~ 2015,
                                                       year == "2" ~ 2017))

socioeconomic_settelments_raw <- bind_rows(socioeconomic_settelments_raw_2013,
                                           socioeconomic_settelments_raw_2015,
                                           socioeconomic_settelments_raw_2017,
                                                 .id = "year")   %>%
                                 mutate(year = case_when(year == "1" ~ 2013,
                                                         year == "2" ~ 2015,
                                                         year == "3" ~ 2017 ))

rm(list = objects(pattern = "*\\d"))


socioeconomic_raw <- bind_rows(socioeconomic_local_authorities_raw,
                               socioeconomic_geo_area_raw,
                               socioeconomic_settelments_raw,
                               .id = "data_type") %>%
                   mutate(data_type = case_when(data_type == "1" ~ "local_authorities",
                                                data_type == "2" ~ "geo_area",
                                                data_type == "3" ~ "settelments" ),
                          municipal_code = if_else(municipal_code %in% c(0,99),
                                                   NA_integer_ ,
                                                   as.integer(municipal_code)),
                           city_code          = as.integer(city_code),
                          settlement_type     = as.integer(settlement_type),
                           geo_area_code      = as.integer(geo_area_code)
                          ) %>%
                   mutate(place_code_temp  = coalesce(.$city_code,.$municipal_code),
                          geo_area_code = replace_na(geo_area_code, 0)
                          ) %>%
                   mutate(place_code       = factor(paste(place_code_temp, geo_area_code, sep = "_") )) %>%
          #        select(-c(starts_with("drop"), city_code, municipal_code, settlement_type)) %>%
  relocate(  geo_area_code ,place_code, city_name_english, .after = year ) 

# check <- socioeconomic_raw %>%
#   filter(!is.na(municipal_code) & !is.na(city_code))%>%
#      group_by(year, data_type)
# 
# sum(duplicated(check$city_code))


socioeconomic <- socioeconomic_raw %>%
    mutate(across(7:53, as.numeric )) %>%
    filter(! (is.na(median_age) & is.na(index_population) ) )

glimpse(socioeconomic)

# socioeconomic_not_ok <- socioeconomic %>%
#   group_by(year, data_type) %>%
#   summarise(num = n(),
#             num_na_geo_area_code =sum(is.na(geo_area_code)),
#             num_na_place_code =sum(is.na(place_code)),
#             num_na_city_name =sum(is.na(city_name)),
#             .groups = "keep")
# 
#  sum(duplicated(socioeconomic$place_code))
# # 
# # socioeconomic_not_ok <- socioeconomic_raw %>%
# #   mutate(across(7:53, as.numeric )) %>%
# #   filter( (is.na(median_age) & is.na(index_population) ) )
# 
#  socioeconomic_not_ok <- socioeconomic %>%
#    group_by(year, data_type) %>%
#    summarise(across(5:51, mean, na.rm = TRUE),.groups = "keep")%>%
#    arrange(data_type, year)
# 
#  socioeconomic %>% ggplot(aes(fct_reorder(place_code, rank), index_value)) +
#    geom_point()+
#    facet_grid(year~data_type, scales = "free")+
#    coord_flip()
#  
#  
# socioeconomic %>% 
#    filter(year == 2015, data_type == "settelments") %>% 
#  # ungroup() %>%
#    arrange(rank) %>%
#    ggplot(aes(fct_reorder(place_code, index_value, na.rm = TRUE), index_value)) +
#    geom_point() +
#    coord_flip()
#  