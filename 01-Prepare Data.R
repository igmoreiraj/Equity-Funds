
#remove old data
rm(list=ls())

library(httr)
library(jsonlite)
library(reshape2)
library(xts)
library(tidyverse)
library(RSQLite)
library(skimr)

#pega fundos abertos xp
temp<-fromJSON(txt="fundos_xp_20201224.txt")

#filtra para fundos de acoes (codigo 3) abertos para captacao apenas
fundos_abertos<-temp[temp$fundingBlocked==F,] %>% 
  filter(categoryCode==3)

#ajusta coluna de cnpjs para o formato padrao de cnpj (adicionando pontos e tracos)
consulta_cnpjs<-fundos_abertos$cnpj
consulta_cnpjs_2<-paste(substr(consulta_cnpjs,1,2), 
                        substr(consulta_cnpjs,3,5),
                        substr(consulta_cnpjs,6,14), sep = ".")
consulta_cnpjs_2<-paste(substr(consulta_cnpjs_2,1,10),substr(consulta_cnpjs_2,11,16),sep = "/")                        
consulta_cnpjs_2<-paste(substr(consulta_cnpjs_2,1,15),substr(consulta_cnpjs_2,16,17),sep = "-")


#Conectar ao BD
conn = dbConnect(dbDriver("SQLite"),"D:/Blog/Estudo FIAs/base_fundos.sqlite")
#atribuir variavel a tabela da base
tabela_base<-tbl(conn,"dataset_fundos")

#criar query
query_db<-tabela_base %>% 
  filter(CNPJ_FUNDO %in% consulta_cnpjs_2) %>% 
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, VL_QUOTA)

#pega base
base<-collect(query_db)

#check if all data was imported
length(unique(base$DENOM_SOCIAL)) == length(consulta_cnpjs_2)

#achar fundos que mudaram a denominacao social
base2<-base %>% 
  select(CNPJ_FUNDO, DENOM_SOCIAL) %>% 
  distinct(.keep_all=T) %>% 
  group_by(CNPJ_FUNDO) %>% 
  filter(n()>1)
  

#delimitar parametro de decay para calculo da vol
lambda<-0.94

#ajustar dados e calcula metricas basicas como retorno vol, var e retorno acumulado
base2<-base %>% 
  mutate(DT_COMPTC = as.Date(DT_COMPTC)) %>% 
  distinct(.keep_all = T) %>% 
  group_by(CNPJ_FUNDO) %>% 
  mutate(Return = VL_QUOTA/dplyr::lag(VL_QUOTA)-1) %>% 
  replace_na(list(Return = 0)) %>% 
  #create EWMA Vol Columns with Decay = Lambda for Price, Price in USD and Yield
  #the columns are created using the recursive accumulate function. Afterwards the VaR for those vols
  #is calculated
  mutate(Ewma_vol_Price = accumulate(Return, .f = ~ sqrt((.x^2) * lambda+ (.y^2) * (1-lambda))),
         VaR_Price = Ewma_vol_Price * qnorm(0.99),
         Cumulative_Returns = cumprod(1+Return)-1) %>% 
  arrange(DT_COMPTC)

#cria tabela de de_para
de_para<-as.data.frame(cbind(consulta_cnpjs, consulta_cnpjs_2, fundos_abertos$name))

nomes_base<-base[match(de_para$consulta_cnpjs_2,base$CNPJ_FUNDO),"DENOM_SOCIAL"]

de_para<-cbind(de_para,nomes_base)
#names(base.xts)<-de_para$V3

rds_out <- 'data/base_fundos.rds'
write_rds(base2, rds_out)

rds_out_xp <- 'data/base_xp.rds'
write_rds(fundos_abertos, rds_out_xp)

rds_out_cnpj <- 'data/cnpj.rds'
write_rds(consulta_cnpjs_2, rds_out_cnpj)

rds_out_base_crua <- 'data/base_crua.rds'
write_rds(base, rds_out_base_crua)

rds_out_de_para <- 'data/de_para.rds'
write_rds(de_para, rds_out_de_para)




