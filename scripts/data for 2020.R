#0 setup

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr)

getwd()

rm(list = objects())



#1 base for CBS area's list - from population data 

#1.1 load and clean population files

#1.1.1 load and clean 2020 population file

population_CBS_raw_2020 <- read_xlsx("originals/population by geo area 2020.xlsx", sheet = 3, skip =12)


population_CBS_2020 <- population_CBS_raw_2020 %>%
                         rename( city_type        = ...1,
                                 city_code        = ...2,
                                 city_name        = ...3,
                                 agas_code        = ...4,
                                 total_population = ...5) %>%
                        mutate_at(vars(-city_name) , as.numeric ) %>%     
                        filter( !is.na(city_code)  & !is.na(total_population) ) %>%
#                        mutate(geo_area_code =  paste(city_code, agas_code, sep = "_")) %>% # now I think that connecting on 2 variables is more efficient
                        group_by(city_code) %>%
                        add_count(sort = TRUE) %>% 
                        ungroup() 

names(population_CBS_2020) <-str_replace_all(names(population_CBS_2020), "-", "_")

#1.1.2 load and clean 2020 population file
# add here

#1.2 merge population tables




#2.general attributes files
#those files contain information at settlement level

#2.1 load and clean general attributes files

#2.1.1 load and clean 2020 general attributes file

col_names <- c("city_name", "city_code", "city_name_english",
               "district", "subdistrict", "natural_region", 
               "municipal_status", "metropolin", "religion",
               "total_population", "jewish_popolatoin", "jewish_popolatoin_2",
               "arab_popolatoin", "foundation_year", "settlement_type",
               "organizational_belonging", "coordination", "avg_haight", 
               "planning_committee", "police_district", "year",
               "city_name_english_2", "local_authoritis_cluster")

general_attributes_CBS_raw_2020 <- read_xlsx("originals/general attributs by town 2020.xlsx", col_names = col_names, skip = 1)

general_attributes_CBS_2020 <- general_attributes_CBS_raw_2020 %>%
  mutate(foundation_year = as.numeric(foundation_year) ) %>%     
  filter( !is.na(total_population) ) #this is to remove all rows  about places that are not settlements



#2.2 merge general attributes tables



#3. CBS data about socioeconomic attributes

#3.1 load and clean socioeconomic files

#3.1.1 load and clean 2017 socioeconomic files




col_names = c("city_code", "city_name", "agas_code", "population", "index_value", "rank", "cluster",
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

#3.1.1.1 file for geographic area in cities 

socioeconomic_CBS_geo_area <- read_xls("originals/socio-economic status by geo area 2017.xls", range = "A8:AX1637", col_names = col_names, na = c("", "..") )

#glimpse(demographic_CBS_geo_area)



#3.1.1.2 file for full cities 
socioeconomic_CBS_cities <- read_xls("originals/socio-economic status by town 2017.xls", range = "A11:AX265", col_names = c("municipal_code", col_names[-3]), na = c("", "..") ) 

socioeconomic_CBS_cities <- socioeconomic_CBS_cities %>%
  mutate(municipal_code = na_if(municipal_code, 99 )) %>%
  mutate(municipal_code = na_if(municipal_code, 0 )) 

socioeconomic_CBS_cities <- socioeconomic_CBS_cities %>%
mutate(city_code = coalesce(socioeconomic_CBS_cities$municipal_code, socioeconomic_CBS_cities$city_code))%>%
  select( -municipal_code)


  
#3.2 merge socioeconomic files
  
#כאן יש חיבור של שני קבצים: מועצות מקומיות ואזורים סטטיסטיים בתוך ערים. אין כאן נתונים על אופן החישוב של הדירוג החברתי כלכלי לפי ישובים. יש באתר רק את התוצאה הסופית ברמת הישוב. 


socioeconomic_CBS <- socioeconomic_CBS_geo_area %>%
  bind_rows(socioeconomic_CBS_cities) %>%
#  mutate(geo_area_code =  paste(city_code, agas_code, sep = "_")) %>%
  group_by(city_code) %>%
  add_count(sort = TRUE) %>% 
  ungroup() 





#999 merge all CBS files

#999.1 some sanity checks

anti_join(population_by_place, demographic_CBS, by = "geo_area_code") %>% glimpse()
anti_join(demographic_CBS, population_by_place, by = "geo_area_code" )%>% glimpse() #ניתן לראות הבדלים של כמה עשרות איזורים בין הטבלאות, אולי בגלל שינויים בין שנים. 


#999.2 merging the files

#999.2.1 merge population with general attributes 


CBS_place <- left_join(population_CBS_2020, general_attributes_CBS_2020, by = "city_code" ) %>%
  select(  -city_name.y, -total_population.y) %>% 
  rename(city_name         = city_name.x,
         total_population  =  total_population.x) %>%
  arrange(desc(total_population)) 

glimpse(CBS_place)

#CBS_place$pop_diff <- CBS_place$total_population.x - CBS_place$total_population.y

#999.2.2 add socioeconomic data

CBS_place <- left_join(CBS_place, socioeconomic_CBS, by = c("city_code", "agas_code" ) ) 

write_csv(CBS_place,"products/CBS_place.csv")

#rm(list = ls(pattern = "_.*_"))

  
