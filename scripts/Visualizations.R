#0 setup

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr)
p_load(corrplot)
getwd()

rm(list = objects())


CBS_place <-  read_rds(file = "products/CBS_place.rds")


  
  
big_cities<- CBS_place  %>% 
                        mutate(rank = ordered(rank))  %>%
                         filter (total_population >50000) 
  
big_cities %>%
  ggplot(aes(reorder(city_name, desc(total_population)), total_population)) +
  geom_col() +
  coord_flip() +
  labs(y = "אוכלוסיה", x = "") 
  
big_cities %>%
  ggplot(aes(x = reorder(city_name, desc(index_value)), y = index_value, fill = index_value)) +
  geom_col() +
  coord_flip() +
  labs(y = "מדד סוציו-אקונומי", x = "") +
  scale_y_continuous(limits = c(-2.5, 2.5) ) +
  scale_fill_distiller(type = "div", palette = "RdBu", direction = "1")+
 # scale_fill_gradient2()+
   theme_bw( )+
  theme(panel.grid.major.y = element_blank())

geo_area_jerusalem <-  CBS_place %>% mutate(rank = ordered(rank))  %>%
                          filter (city_code == 3000) 


geo_area_jerusalem %>%
  ggplot(aes( x = median_age, fill = median_age)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 50) ) 

geo_area_jerusalem %>%
  ggplot(aes( x = median_age, y = percent_families_4_, color = dependency_ratio)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 50) ) 

geo_area_jerusalem %>%
  ggplot(aes( x = median_age, y = percent_families_4_, color = percent_high_earning)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 50) ) 

geo_area_jerusalem_cors <- geo_area_jerusalem %>% select(names(geo_area_jerusalem)[45:90]) %>%
                                                  select((where(is.numeric)) )%>%
                                                  select(-ends_with("std"), -ends_with("rank")) 
                                                  
names(geo_area_jerusalem)[45:90]

cor_m <- cor(geo_area_jerusalem_cors)

corrplot(cor_m, method = 'number') # problem = format does not suit so many variables
