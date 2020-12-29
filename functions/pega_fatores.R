pega_fatores<- function(url, extension = ".xls"){
  #carrega libraries exigidas pela funcao
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("httr")) install.packages("httr")
  #error checking
  stopifnot(extension %in% c(".xls",".xlsx", ".xlsm"), is(url, "character"), length(url)==1)
  
  #read data from url
  GET(url, write_disk(tf <- tempfile(fileext = extension)))
  df <- read_excel(tf, 1L)
  
  #adjust data-frame
  df_temp<-df %>%
    mutate(date = make_date(year, month, day)) %>% 
    select(-year,-month, -day)
  
  return(df_temp)
  
}
