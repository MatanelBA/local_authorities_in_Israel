#0 setup

library(pacman)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, visdat) #, assertive #visdat
p_load(corrplot)
getwd()

rm(list = objects())


CBS_place <-  read_rds(file = "products/CBS_place.rds")


#1. city level
  
  
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



#2. geo erea level

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
  ggplot(aes( x = percent_degree , y = avg_income_per_capita, color = index_value)) +
  geom_point() +
  theme_bw()


geo_area_jerusalem_cors <- geo_area_jerusalem %>% select(names(geo_area_jerusalem)[45:90]) %>%
                                                  select((where(is.numeric)) )%>%
                                                  select(-ends_with("std"), -ends_with("rank")) 
                                                  



#3. EDA



cor_m <- CBS_place %>% select(names(CBS_place)[46:90]) %>%
               select((where(is.numeric)) )%>%
                select(-ends_with("std"), -ends_with("rank")) %>% cor( use = "na.or.complete")


svglite("myplot.svg", width = 16, height = 16)
corrplot(cor_m, method = 'color',number.font = 0.02, 
         addCoef.col = 'black', addCoefasPercent = T, 
         tl.col = "black", tl.offset = 1, 
         order = 'hclust', addrect = 2) 
dev.off()


