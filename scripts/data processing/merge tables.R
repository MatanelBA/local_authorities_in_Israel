library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr)

rm(list = objects())

getwd()

age_composition_wide<- read_csv( "products/age_composition_geo_area_14_20_wide.csv")

settlements_13_20 <- read_csv( "products/settlements_03_20.csv") %>%
  filter(year>2012)

socioeconomic <- read_csv( "products/socioeconomic_13_17.csv")

local_data <- full_join(age_composition_wide, settlements_13_20, by = c("year", "place_code") ) %>%
              full_join(socioeconomic, by = c("year", "place_code") ) %>%
               mutate(city_name         = coalesce(city_name,  city_name.x         , city_name.y   ),
                      city_name_english = coalesce(            city_name_english.x , city_name_english.y),
                      city_code         = coalesce(            city_code.x         , city_code.y),
                      data_type         = coalesce(data_type,  data_type.x         , data_type.y)
                      ) %>%
          #    select(-ends_with(".y"), -ends_with("x.x")) %>%
              relocate(data_type,city_name, place_code, .after = "year") %>%
              arrange(place_code, year)

glimpse(local_data)

#1. connect tables by geo_area

#1.1 add socioeconomic data to age composition data

#1.2 add geo_area description

#2. connect tables by settelments


#2.1 add local authorities to settelment table 

#3. add settelment table to geo_area table