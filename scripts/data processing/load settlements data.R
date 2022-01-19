#this script load, clean and merge the "settlements" files. the product is "products/settlements_03_20.csv"

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, data.table)

rm(list = objects())

getwd()

#1. load settlements list for every year

col_names <- c("city_name", "city_code", "city_name_english",
               "district", "subdistrict", "natural_region", 
               "municipal_status", "metropoline", "religion",
               "total_population", "jewish_and_other_population", "jewish_population",
               "arab_population", "foundation_year", "settlement_type",
               "organizational_belonging", "coordination", "avg_haight", 
               "planning_committee", "police_district", "year",
               "city_name_english_2", "local_authorities_cluster")



files_list_xlsx <-list.files("originals/settlements/settlements list/", pattern = "xlsx", full.names = TRUE)
settlements_18_20 <- map_dfr(files_list_xlsx, read_excel, col_names = col_names, skip = 1 )  %>%
                      mutate(settlement_type = as.character(settlement_type),
                             coordination    = as.character(coordination)
                              )

##עד כאן עובד

files_list_xls <- list.files("originals/settlements/settlements list/", pattern = "xls$", full.names = TRUE)

settlements_03_17_list <- map(files_list_xls,  read_excel) #  אני לא יודע איך לעבור מהליסט לקובץ אחיד כאשר השמות שונים זה מזה באיות והם בעברית. השתמשתי בנתינת שמות ידנית.יכול להיות שיש דרך יותר יעילה לעשות זאת

settlements_03_17_raw <- rbindlist(settlements_03_17_list, use.names = TRUE, fill = TRUE, idcol = TRUE) %>%
                          as_tibble()

col_names_long <- c("id", "city_name", "city_code", 
                "subdistrict","municipal_status", "natural_region", 
                 "religion","planning_committee",
               "total_population", "jewish_and_other_population", "jewish_population",
               "arab_population", "no_pop", "coordination",
               "metropoline", "year", 
                  "settlement_type", "organizational_belonging",
               "settlement_type_2", "organizational_belonging_2",
               "foundation_year", "city_name_english",
               "avg_haight","police_district", "avg_haight_2",
               "city_name_2", "city_name_3","city_name_english_2",
               "district", "municipal_status_2","metropoline_2",
               "religion_2", "total_population_2", "jewish_and_other_population_2",
               "jewish_population_2", "arab_population_2", "coordination_2", "avg_haight_3",
               "connection", "total_population_3",  "jewish_and_other_population_3",
               "jewish_population_3", "foundation_year_2", "organizational_belonging_3",
               "planning_committee_2",
               "city_name_english_3", "total_population_4", "total_population_5",
               "city_name_4", "city_name_5", 
               "city_code_2", "city_name_english_4", "municipal_status_3",
               "religion_3", "total_population_6", "avg_haight_4",
               "city_name_english_5", "total_population_7", "year_2", 
               "total_population_8", "police_district_2", "total_population_9",
                "city_name_6", "total_population_10", "total_population_11"
               )

#col_names_long[duplicated(col_names_long)]

names(settlements_03_17_raw) <- col_names_long
  
#names(settlements_03_17_df_raw) <- c(col_names, paste("x", as.character(c(24:65)), sep = "") )
#settlements_03_17_raw%>% select(starts_with("arab_population")) %>% glimpse()

glimpse(settlements_03_17_raw)

settlements_03_17 <- settlements_03_17_raw %>% 
  mutate(city_name                   = coalesce( city_name,  city_name_2,  city_name_3,  city_name_4,  city_name_5),
         city_name_english           = coalesce(city_name_english, city_name_english_2, city_name_english_3, city_name_english_4, city_name_english_5),
         religion                    = coalesce( religion, religion_2,religion_3 ),
         city_code                   = coalesce(city_code, city_code_2),
         total_population            = coalesce(total_population, as.numeric(total_population_2), total_population_3, total_population_4 , total_population_5 , total_population_6 ,total_population_7, total_population_8, total_population_9 ,total_population_10, total_population_11),
         jewish_population           = coalesce(as.numeric(jewish_population), as.numeric(jewish_population_2) ,jewish_population_3),
         jewish_and_other_population = coalesce(jewish_and_other_population, as.numeric(jewish_and_other_population_2) ,jewish_and_other_population_3),
         arab_population             = coalesce(arab_population, as.numeric(arab_population_2)),
         planning_committee          = coalesce(planning_committee, planning_committee_2),
         avg_haight                  = coalesce(avg_haight, avg_haight_2, avg_haight_3, avg_haight_4),
         foundation_year             = coalesce(foundation_year, foundation_year_2),
         organizational_belonging    = coalesce(organizational_belonging, organizational_belonging_2, organizational_belonging_3),
         metropoline                 = coalesce(metropoline, metropoline_2),
         settlement_type             = coalesce(settlement_type, settlement_type_2),
         year = id+2002
         ) %>%  
          select ( one_of( col_names))
  


settlements_03_20_base <- bind_rows(settlements_03_17, settlements_18_20)



#2. load olim population by settlement for every year

#list.files("originals/population by sattelment and geo area/olim population", full.names = TRUE)

files_list_olim <- c("originals/population by sattelment and geo area/olim population/population_madaf_3.xls"       ,
                     "originals/population by sattelment and geo area/olim population/population_madaf_3_15.xls"    ,
                     "originals/population by sattelment and geo area/olim population/population_madaf_3_16.xlsx"   ,
                     "originals/population by sattelment and geo area/olim population/t1.xlsx"                      ,
                     "originals/population by sattelment and geo area/olim population/population_madaf_2018_1.xlsx" ,
                     "originals/population by sattelment and geo area/olim population/population_madaf_2019_6.xlsx" ,
                     "originals/population by sattelment and geo area/olim population/population_madaf_2020_11.xlsx"
                     )



col_names_olim <- c("city_name", "city_code", "total_population", "olim_population")
col_types_olim <- c("text", "numeric", "numeric", "numeric")


olim <- map_dfr(files_list_olim, read_excel, sheet = 2,  col_names = col_names_olim, col_types = col_types_olim, na = "..", .id = "year") %>%
        filter(!is.na(city_code)) %>%
        mutate(olim_population = replace_na(olim_population,0),
               prc_olim = olim_population/total_population,
               year = as.numeric(year) + 2013)
        

anti_join(olim, filter(settlements_03_20_base, year>2013), by = c("city_code", "year" )) #בדיקת שפיות: האם כל הערים בטבלת העולים מופיעות בטבלת הישובים?

settlements_03_20 <- left_join(settlements_03_20_base, olim,  by = c("city_code", "year" ) )

#settlements_14_20 <- filter(settlements_03_20, year>2013, total_population.x > 5000)
# ok<- settlements_14_20[(settlements_14_20$city_name.x != settlements_14_20$city_name.y),]  
# ok<- settlements_14_20[(settlements_14_20$total_population.x != settlements_14_20$total_population.y),]  %>%  mutate(diff =total_population.x- total_population.y)


write_csv(settlements_03_20, "products/settlements_03_20.csv")
