rm(list = ls())
cat("\014")
library(data.table)
library(tidyverse)
library("googlesheets4")
library("googledrive")
library(lubridate)
## DATA SOURCES
# Definindo tempo zero com 21h/horário de São Paulo:
t0 <- as_datetime(as.Date("2021-11-30") + dhours(21), tz = "America/Sao_Paulo")
#saveRDS(t0, file = "./time0.rds")
t0 <- readRDS(file = "./time0.rds")

# Pegando hora atual de acordo com horário de São Paulo:
t1 <- as_datetime(Sys.time(), tz = "America/Sao_Paulo")


if (duration(int_length(interval(t0, t1))) >= duration(60)) {
  # Baixando os dados de 12 em 12 horas de acordo com acessos a aplicação:
  
  
  # Google sheets authentification -----------------------------------------------
  options(gargle_oauth_cache = ".secrets")
  drive_auth(cache = ".secrets", email = "fernando.bastos@ufv.br")
  gs4_auth(token = drive_token())
  #list.files(".secrets/")
  #gs4_auth()
  arq <- "https://docs.google.com/spreadsheets/d/1eVcgXa-p-O8aOYJWXVamoJYxNFVccbTr-fB-NSoWrbo/edit?usp=sharing"
  df1 <- read_sheet(arq)
  df1$DATA <- lubridate::ymd(df1$DATA)
  require("pbapply");require("data.table");require("httr");require("rvest");require("dplyr")
  require("lubridate");require("jsonlite")
  PASS <- new.env()
  assign("apikey","d9c2eefc-bafd-4f45-87e5-386327c717b3",envir = PASS)
  # HELPER FUNCTION - Converts timestamp to local timestamp
  # format TimeZone
  fixTZ = function(timeStamp){
    tmDIFF = round(as.numeric(difftime(Sys.time(),
                                       lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                       units = "hours")),0)
    as.POSIXct(timeStamp + hours(tmDIFF), tz= Sys.timezone())
  }}
  # *******************************************************************************************************
  # https://coinmarketcap.com/api/documentation/v1/#operation/getV1FiatMap
  