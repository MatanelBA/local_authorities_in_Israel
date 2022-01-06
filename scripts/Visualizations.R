#0 setup

library(pacman)
p_load(dplyr, openxlsx, readxl,readr, tidyr, utf8)
p_load(tidyverse, openxlsx, readxl, utf8, ggplot2, scales, magrittr)

getwd()

rm(list = objects())


CBS_place <- load(file = "products/CBS_place")
