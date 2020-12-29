#remove old data
rm(list=ls())

library(readxl)
library(httr)
library(tidyverse)
library(lubridate)
library(purrr)
library(broom)
library(lmtest)
library(sandwich)

#load private functions
source('functions/pega_fatores.R', echo=TRUE)

#read saved data
rets <- read_rds('data/base_fundos.rds')
fatores<-read_rds('data/base_factors.rds')
de_para<-read_rds('data/de_para.rds')

de_para<-de_para %>% 
  rename(CNPJ_FUNDO = consulta_cnpjs_2) %>% 
  mutate(CNPJ_FUNDO = as.character(CNPJ_FUNDO),
         V3 = as.character(V3)) %>% 
  select(CNPJ_FUNDO, V3)

#join databases
df<-rets %>% 
  rename(date = DT_COMPTC) %>% 
  left_join(fatores, by = "date") %>% 
  relocate(date)

#run regression
df.reg<-df %>% 
  mutate(Return_minus_rf = Return - Risk_free) %>% 
  group_by(CNPJ_FUNDO) %>%
  do(model.fia = glance(lm(Return_minus_rf ~ Rm_minus_Rf + WML, data = .))) %>%
  unnest(model.fia)


# df.stat<-df %>%
#   mutate(Return_minus_rf = Return - Risk_free) %>% 
#   split(df$CNPJ_FUNDO) %>% 
#   map(.,~ lm(formula = Return_minus_rf ~ Rm_minus_Rf + WML, data = .)) %>% 
#   map2(.x = ., .y = split(df, f=df$CNPJ_FUNDO), .f = ~augment_columns(x=.x, data=.y)) %>% 
#   bind_rows()

model.df<-df %>% 
  as_tibble() %>% 
  filter(date >= as.Date("2016-01-01")) %>% 
  mutate(Return_minus_rf = Return - Risk_free) %>% 
  nest(-CNPJ_FUNDO) %>%
  mutate(n_linhas = map(data, .f=~ nrow(.))) %>% 
  unnest(n_linhas) %>% 
  filter(n_linhas==max(n_linhas)) %>% 
  mutate(model.lm = map(data,.f= ~lm(Return_minus_rf ~ Rm_minus_Rf + WML, data = .)),
         robust.lm = map(model.lm, .f= ~ tidy(coeftest(., vcov. = NeweyWest)))) %>% 
  unnest(robust.lm) %>% 
  filter(term == "(Intercept)") %>% 
  select(CNPJ_FUNDO, statistic, p.value) %>% 
  arrange(desc(statistic)) %>% 
  left_join(de_para, by = "CNPJ_FUNDO")


#create chart
model.df %>% 
  ggplot()



