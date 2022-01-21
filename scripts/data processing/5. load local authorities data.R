#this script load, clean and merge the "local authorities" files. the produt is #AAA#

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr)

rm(list = objects())

getwd()



local_authorities_raw_2019 <- read_xlsx("originals/local authorities/2019.xlsx")
