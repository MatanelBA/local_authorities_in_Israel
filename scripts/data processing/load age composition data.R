#this script load, clean and merge the "age composition" files. the proudct is #AAA#

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, data.table)

rm(list = objects())

getwd()

col_names <- c('city_code', 'city_name', 'geo_code', 'gender', 'total', '4_0', '5-9', '10-14', '15-19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_54', '55_59', '60_64', '65+')

#files_list_xlsx <-list.files("originals/population by sattelment and geo area/age composition by geo area")
#age_composition_by_geo_area <- 


age_composition_by_geo_area_2020 <- read_excel("originals/population by sattelment and geo area/age composition by geo area/population_madaf_2020_7.xlsx", 
                                               sheet = 2,
                                               skip = 14,
                                               col_names = col_names)
View(age_composition_by_geo_area_2020)
glimpse(age_composition_by_geo_area_2020)
#זה מביא מידע רק על ישובים יהודים. צריך לחזור על זה לעוד שני גיליונות באקסל, ולנסות להכליל על כל השנים באמצעות ליסט

