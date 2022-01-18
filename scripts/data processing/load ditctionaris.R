#load dictionaries  

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr, data.table)

rm(list = objects())

 getwd()
 
files_list <-list.files("originals/settlements/settlements dims", full.names = TRUE) #, pattern = "xlsx"


names_2 <- c("sub_district_desc_2","sub_district_desc", "district_desc", "district_code" )
dims_03_20_sheet_2 <- map(files_list, read_excel, sheet = 2 ) %>%   
                      map(            setNames, names_2  ) %>%
                      bind_rows() %>%
                      mutate(district_code = as.integer(district_code)) %>%
                      filter(!is.na(district_code))%>%
                      distinct() %>%
                      tidyr::fill(district_desc, .direction = "down")%>%
                      mutate(sub_district_desc  = if_else(is.na(sub_district_desc_2),
                                                         sub_district_desc,
                                                         sub_district_desc_2)
                            )%>%
                      select(-sub_district_desc_2)


#את זה מתבקש להמשיך עם פור לופ. ליצור וקטור של שמות לכל גיליון,ולרוץ עם הלולאה על הפונקציה שמתוארת כאן עבור כל גיליון  מהגיליונות 2 עד 10
#זה לא כל כך פשוט כי אז אי אפשר להשתמש בשמות המשתנים (הם שונים בכל וקטור), ולא באינדקס שלהם כי הם לא באותו אורך
