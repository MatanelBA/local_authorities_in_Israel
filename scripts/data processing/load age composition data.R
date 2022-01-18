#this script load, clean and merge the "age composition" files. the proudct is #AAA#

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, data.table)

rm(list = objects())
getwd()

###

#1 load files for age composition by geo area 




setwd("originals/population by sattelment and geo area/age composition by geo area")

#list.files(full.names = TRUE)


 col_names_19_20 <- c('city_type', 'city_code', 'city_name', 'geo_code', 'total', '4_0', '5_9', '10_14', '15_19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_54', '55_59', '60_64', '65_69', '70_74', '75_79', '80_84', '85+' )

 # age_composition_by_geo_area_2019 <- read_excel("population_madaf_2019_7.xlsx", 
#                                                  sheet = 3, skip = 15, col_names = col_names_19_20, na = "..") %>%
#                                       mutate(year = 2019,
#                                              `75+` = sum(`75_79`, `80_84`,`85+`, na.rm = TRUE ),
#                                              geo_code = if_else(geo_code == 'סה"כ' ,0, as.numeric(geo_code) )
#                                              )%>%
#                                       select(-c(`75_79`, `80_84`,`85+`))


age_composition_by_geo_area_19_20 <- map_dfr(c( "population_madaf_2019_7.xlsx",
                                                 "population_madaf_2020_9.xlsx"),
                                              read_excel, sheet = 'סה"כ אוכלוסייה', col_names = col_names_19_20, na = "..", .id = "year") %>%
                                     filter(!is.na(city_name)  ) %>%
                                     mutate(
                                            year     = as.numeric(year)+2018,
                                            `75+`    = sum(`75_79`, `80_84`,`85+`, na.rm = TRUE ),
                                            geo_code = if_else(geo_code == 'סך הכל' , 
                                                               0,
                                                               as.numeric(geo_code) )
                                            ) %>%
                                     mutate(across(col_names_19_20[-3], as.numeric))%>%
                                     select(-c(`75_79`, `80_84`,`85+`))
 
 
 
col_names_17_18 <-c(col_names_19_20[1:20], "75+")
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
                                               "population_madaf_1_16.xlsx"
                                               ),
                                             read_excel, sheet = 'סה"כ אוכלוסייה', col_names = col_names_14_16, na = "..", .id = "year") %>%
                                     filter(!is.na(city_name) & !is.na(sex) ) %>%
                                     select(-sex) %>%
                                     mutate(year = as.numeric(year)+2013,
                                            geo_code = if_else(geo_code == 'סה"כ' ,0, as.numeric(geo_code) )
                                            ) %>%
                                     mutate(across(col_names_14_16[-(3:5)], as.numeric))


# glimpse(age_composition_by_geo_area_19_20)
# glimpse(age_composition_by_geo_area_17_18)
# glimpse(age_composition_by_geo_area_14_16)

age_composition_by_geo_area <- bind_rows(age_composition_by_geo_area_14_16,age_composition_by_geo_area_17_18,age_composition_by_geo_area_19_20) %>%
                                arrange(year, city_type, city_code, geo_code )

city_type_by_year <- age_composition_by_geo_area %>%
                      select(year, city_code,city_type)%>%
                      distinct()

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



#2. load files for age composition by population group and geo area (mixed cities only)


setwd("./../../..")
setwd("originals/population by sattelment and geo area/age composition by geo area and population group/")

files_list_xlsx <-list.files(full.names = TRUE)

col_names_pop_group <-c(col_names_17_18[2:4],"population_group", col_names_17_18[5:21])

age_composition_by_area_pop_group <- read_excel("./population_madaf_2.xls", sheet = 3, col_names = col_names_pop_group, na = "..") %>%
                                            bind_rows(
                                                       map_dfr(c("population_madaf_2_15.xls",
                                                       "population_madaf_2_16.xlsx",
                                                       "t3.xlsx",
                                                       "population_madaf_2018.xlsx",
                                                       "population_madaf_2019_5.xlsx",
                                                       "population_madaf_2020_8.xlsx" ),
                                                         read_excel, sheet = 2, col_names = col_names_pop_group, na = ".." , .id = "year")
                                                     ) %>%
  mutate(year     = if_else(is.na(year), 
                            2014,
                            as.numeric(year)+2014),
         geo_code = if_else(geo_code %in% c('סך הכל ביישוב', 'סה"כ אוכלוסייה' ),
                            0,
                            as.numeric(geo_code)
                            ),
        population_group = case_when( population_group == 'סה"כ אוכלוסייה בא"ס'   ~ "total_population"  ,
                                      population_group == 'יהודים ואחרים'         ~ "jewish_population" ,
                                      population_group == 'ערבים'                 ~ "arab_population"
                                     )
        ) %>%
  mutate(across(col_names_pop_group[-(2:4)], as.numeric)) %>%
  filter(!is.na(city_name),
         !is.na(geo_code) ,
         population_group != "total_population") %>% #to prevent duplication with the same rows in age_composition_by_geo_area
  arrange(year, city_code, geo_code ) %>% left_join(city_type_by_year, by = c("year", "city_code"))


#3 merge the  table for total population in all cities with the table for group population in  mixed cities
  
# glimpse(age_composition_by_area_pop_group)
# glimpse(age_composition_by_geo_area)
# 
# as.data.frame(names(age_composition_by_area_pop_group),
#       names(age_composition_by_geo_area))

age_composition <- age_composition_by_geo_area %>% 
                        mutate (population_group = "total_population") %>%
                        bind_rows(age_composition_by_area_pop_group)   %>%
                        relocate(population_group, .after = geo_code)  %>%
                        arrange(year, city_type, city_code, geo_code, desc(population_group) )
                      

setwd("./../../..") 
dict_city_type <- read_excel("originals/dictionaries.xlsx", sheet = "city_type")

age_composition <- left_join(age_composition,dict_city_type, by = c("city_type" = "city_type_code") )%>%
                    select(-elaborated_description) %>%
                    mutate(geo_code = replace_na(geo_code, 0))  %>%
                    mutate(place_code = paste(city_code, geo_code, sep = "_"))%>%
                    relocate(city_type_desc,place_code, .after = 2) 


write.xlsx(age_composition, "products/age_composition_geo_area_14_20.xlsx")
