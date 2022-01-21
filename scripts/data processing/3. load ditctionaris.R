#load dictionaries  

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, data.table)

rm(list = objects())

 getwd()
 
files_list <-list.files("originals/settlements/settlements dims", full.names = TRUE) #, pattern = "xlsx"

#ניסיון על מילון אחד

names_2 <- c("sub_district_desc_2","sub_district_desc", "district_desc", "district_code" )

dims_03_20_sheet_2 <- map(files_list, read_excel, sheet = 2 ) %>%   
  map(            setNames, names_2  ) %>%
  bind_rows() %>%
  mutate(district_code = as.integer(district_code)) %>%
  filter(!is.na(district_code))%>%
  distinct() %>%
  tidyr::fill(district_desc, .direction = "down") %>%
  mutate(sub_district_desc  = if_else(is.na(sub_district_desc_2),
                                      sub_district_desc,
                                      sub_district_desc_2)
  )%>%
  select(-sub_district_desc_2)

#הרחבה לכל המילונים
#לא בכולם הצלחתי. את מה שלא הצלחתי הוצאתי החוצה

names_list <-  list  (names_2  = c("sub_district_desc_2",           "sub_district_desc",              "district_desc",            "district_code"         ),
                      names_3  = c("municipal_status_desc",         "municipal_status_code"                                                               ),
                      names_4  = c("drop"                 ,         "natural_region_desc",            "subdistrict",              "natural_region_code"   ),
                      names_5  = c("religion_desc",                 "religion_code"                                                                       ),
                      names_6  = c("planning_committee_type",       "planning_committee_desc",        "planning_committee_code"                           ),
                      names_7  = c("metropoline_area",              "metropoline_ring",               "metropoline_name",         "metropoline_code"      ),
                      names_8  = c("city_type_desc_2",              "city_type_desc_1",               "city_type_code"                                    ),
                      names_9  = c("organizational_belonging_desc", "organizational_belonging_code"                                                       ),
                      names_10 = c("police_station_code",           "police_station_desc_3",          "police_station_desc_2",    "police_station_desc_1" ),
                      names_11 = c("local_authoritis_cluster_desc", "local_authoritis_cluster_code"                                                       )
)




dims_03_20_sheet_2  <- map_dfr(files_list, read_excel, sheet = 2  , col_names = names_list[[1]]  )# %>%   
#dims_03_20_sheet_3  <- map_dfr(files_list, read_excel, sheet = 3  , col_names = names_list[[2]]  )# %>%
#dims_03_20_sheet_4  <- map_dfr(files_list, read_excel, sheet = 4  , col_names = names_list[[3]]  )# %>%
dims_03_20_sheet_5  <- map_dfr(files_list, read_excel, sheet = 5  , col_names = names_list[[4]]  ) %>%
  mutate(religion_code = as.integer(religion_code)) %>%
  filter(!is.na(religion_code))%>%
  distinct() 
#dims_03_20_sheet_6  <- map_dfr(files_list, read_excel, sheet = 6  , col_names = names_list[[5]]  )# %>%
dims_03_20_sheet_7  <- map_dfr(files_list, read_excel, sheet = 7  , col_names = names_list[[6]]  ) %>%
  mutate(metropoline_code = as.integer(metropoline_code)) %>%
  filter(!is.na(metropoline_code))%>%
  distinct() %>%
  tidyr::fill(metropoline_area, metropoline_ring, metropoline_name, .direction = "down") 
  
#dims_03_20_sheet_8  <- map_dfr(files_list, read_excel, sheet = 8  , col_names = names_list[[7]]  ) %>%
#  mutate(city_type_code = as.integer(city_type_code)) %>%
#  filter(!is.na(city_type_code))%>%
#  distinct() %>%
#  tidyr::fill(city_type_desc_2, city_type_desc_1, .direction = "down") 
#כאן המילון שונה לפי שנה. יש שנים שבהן הערים הגדולות קיבלו סמל נפרד ויש שנים שבהן הערים הגדולות קובצו לסמל אחד

dims_03_20_sheet_9  <- map_dfr(files_list, read_excel, sheet = 9  , col_names = names_list[[8]]  )  %>%
  mutate(organizational_belonging_code = as.integer(organizational_belonging_code)) %>%
  filter(!is.na(organizational_belonging_code))%>%
  distinct()


#dims_03_20_sheet_10 <- map_dfr(files_list, read_excel, sheet = 10 , col_names = names_list[[9]] )# %>%
#dims_03_20_sheet_11 <- map_dfr(files_list, read_excel, sheet = 11 , col_names = names_list[[10]] )# %>%

