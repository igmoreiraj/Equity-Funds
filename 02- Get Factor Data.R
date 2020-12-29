#remove old data
rm(list=ls())

library(readxl)
library(httr)
library(tidyverse)
library(lubridate)
library(purrr)

#load private functions
source('functions/pega_fatores.R', echo=TRUE)

#cria lista com urls
urls<-c("Market" = "http://nefin.com.br/Risk%20Factors/Market_Factor.xls", 
        "WML" = "http://nefin.com.br/Risk%20Factors/WML_Factor.xls", 
        "Rf" = "http://nefin.com.br/Risk%20Factors/Risk_Free.xls")

#cria base fatores
fatores<-urls %>% 
  map(.f = pega_fatores) %>% 
  reduce(left_join, by="date") %>% 
  relocate(date)

#salva base
rds_out_factors <- 'data/base_factors.rds'
write_rds(fatores, rds_out_factors)
