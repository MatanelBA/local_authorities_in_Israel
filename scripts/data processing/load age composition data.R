#this script load, clean and merge the "age composition" files. the proudct is #AAA#

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, data.table)

rm(list = objects())

getwd()
setwd("originals/population by sattelment and geo area/age composition by geo area")

#files_list_xlsx <-list.files(full.names = TRUE)


col_names_19 <- c('city_type', 'city_code', 'city_name', 'geo_code', 'total', '4_0', '5-9', '10-14', '15-19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_54', '55_59', '60_64', '65_69', '70_74', '75_79', '80_84', '85+')
age_composition_by_geo_area_2019 <- read_excel("population_madaf_2019_7.xlsx", 
                                                 sheet = 3, skip = 15, col_names = col_names_19, na = "..") %>%
                                      mutate(year = 2019,
                                             `75+` = sum(`75_79`, `80_84`,`85+`, na.rm = TRUE ),
                                             geo_code = if_else(geo_code == 'סה"כ' ,0, as.numeric(geo_code) )
                                             )%>%
                                      select(-c(`75_79`, `80_84`,`85+`))


col_names_17_18 <-c(col_names_19[1:20], "75+")
age_composition_by_geo_area_17_18 <- map_dfr(c( "t2.xlsx",
                                                "population_madaf_2018_3.xlsx"),
                                           read_excel, sheet = 'סה"כ אוכלוסייה', col_names = col_names_17_18, na = "..", .id = "year") %>%
                                    filter(!is.na(city_name)  ) %>%
                                    mutate(year = as.numeric(year)+2016,
                                           geo_code = if_else(geo_code == 'סה"כ' ,0, as.numeric(geo_code) )
                                           ) %>%
                                    mutate(across(col_names_17_18[-3], as.numeric))


col_names_14_16 <-c(col_names_17_18[1:4], "sex", col_names_17_18[5:21])
age_composition_by_geo_area_14_16 <- map_dfr(c("population_madaf_1.xls"     ,
                                               "population_madaf_1_15.xls"  ,
                                               "population_madaf_1_16.xlsx"),
                                             read_excel, sheet = 'סה"כ אוכלוסייה', col_names = col_names_14_16, na = "..", .id = "year") %>%
                                     filter(!is.na(city_name) & !is.na(sex) ) %>%
                                     select(-sex) %>%
                                     mutate(year = as.numeric(year)+2013,
                                            geo_code = if_else(geo_code == 'סה"כ' ,0, as.numeric(geo_code) )
                                            ) %>%
                                     mutate(across(col_names_14_16[-(3:5)], as.numeric))


glimpse(age_composition_by_geo_area_2019)
glimpse(age_composition_by_geo_area_17_18)
glimpse(age_composition_by_geo_area_14_16)

age_composition_by_geo_area <- bind_rows(age_composition_by_geo_area_14_16,age_composition_by_geo_area_17_18,age_composition_by_geo_area_2019) %>%
                                arrange(year, city_type, city_code, geo_code )


##הפרסום לשנת 2020 שונה. צריך לבדוק שוב באתר הלמס האם יש טבלה מקבילה לטבלאות האחרות
# 
# col_names <- c('city_code', 'city_name', 'geo_code', 'gender', 'total', '4_0', '5-9', '10-14', '15-19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_54', '55_59', '60_64', '65+')
# 
# age_composition_by_geo_area_2020_j <- read_excel("originals/population by sattelment and geo area/age composition by geo area/population_madaf_2020_7.xlsx", 
#                                                  sheet = 2,
#                                                  skip = 14,
#                                                  col_names = col_names)
# View(age_composition_by_geo_area_2020)
# glimpse(age_composition_by_geo_area_2020)
