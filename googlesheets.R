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
  arq <- "https://docs.google.com/spreadsheets/d/1oFtbeDzhXEWSc2pC8q8vBKuavcCAajQzn3WF_i1uch0/edit?usp=sharing"
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
  }
  # *******************************************************************************************************
  # https://coinmarketcap.com/api/documentation/v1/#operation/getV1FiatMap
  getFiat = function()
  {
    # url
    url = paste0("https://pro-api.coinmarketcap.com/v1/fiat/map")
    # GET request
    pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                          `X-CMC_PRO_API_KEY` = PASS$apikey))
    # read in content
    dt<- fromJSON(rawToChar(pg$content))
    
    fiat <- dt[["data"]] %>% as.data.frame()
    fiat
  }
  tmp <- getFiat()
  
  # *******************************************************************************************************
  # https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyQuotesLatest
  getLatestQuote= function(symbol, fiat)
  {
    # build URL
    url = paste0("https://pro-api.coinmarketcap.com/v1/cryptocurrency/quotes/latest",
                 "?convert=",fiat,"&symbol=",symbol)
    # GET request
    pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                          `X-CMC_PRO_API_KEY` = PASS$apikey))
    # read in content
    dt<- fromJSON(rawToChar(pg$content))
    
    # extract quote
    qte <- rbindlist(dt$data[[1]]$quote) %>% as.data.frame()
    # format column types
    qte$price <- round(qte$price, 10)
    qte$percent_change_1h <- round(qte$percent_change_1h/100,5)
    qte$percent_change_24h <- round(qte$percent_change_24h/100,5)
    qte$percent_change_7d <- round(qte$percent_change_7d/100,5)
    qte$percent_change_30d <- round(qte$percent_change_30d/100,5)
    qte$percent_change_60d <- round(qte$percent_change_60d/100,5)
    qte$percent_change_90d <- round(qte$percent_change_90d/100,5)
    qte$market_cap_dominance<-round(qte$market_cap_dominance/100,5)
    qte$last_updated <- fixTZ(as.POSIXct(qte$last_updated, format="%Y-%m-%dT%H:%M:%S.000Z"))
    
    # add Meta
    meta <- as.data.frame(cbind(dt$data[[1]]$id,
                                dt$data[[1]]$name,
                                dt$data[[1]]$symbol,
                                dt$data[[1]]$slug,
                                dt$data[[1]]$num_market_pairs,
                                dt$data[[1]]$date_added,
                                ifelse(is.null(dt$data[[1]]$max_supply), NA,dt$data[[1]]$max_supply),
                                dt$data[[1]]$circulating_supply,
                                dt$data[[1]]$total_supply,
                                dt$data[[1]]$is_active
    ))
    colnames(meta) <- c("id","name","symbol","slug","num_market_pairs",
                        "date_added","max_supply","circulating_supply",
                        "total_supply","is_active")
    meta$date_added <- fixTZ(as.POSIXct(meta$date_added, format="%Y-%m-%dT%H:%M:%S.000Z"))
    # combine meta & qte data
    all <- cbind(meta,qte)
    # return data
    all
    
  }
  # TEST Function
  #tmp1 = getLatestQuote(symbol = "BTC", fiat = "USD")
  #tmp2 = getLatestQuote(symbol = "BTC", fiat = "CAD")
  # call multiple quotes:
  #symbols = c("BTC","ETH","DOGE","ADA","XTZ","USDC")
  DolarValue = getLatestQuote(symbol = "USDT", fiat = "BRL")
  DolarValue <- DolarValue$price
  
  symbols = unique(df1$SIGLA)
  qte <- pblapply(as.list(symbols), function(x){
    tmp <- try(getLatestQuote(symbol=x, fiat="USD"))
    if(!inherits(tmp, 'try-error'))
      tmp
  })
  # row bind data
  qte <- rbindlist(qte,use.names = TRUE,fill = TRUE)
  df1$symbol <- df1$SIGLA
  qte2 <- merge(qte, df1, by = "symbol")
  #qte2 <- cbind(qte,df1)
  qte2$Custo <- qte2$VALOR*qte2$QUANTIDADE
  #df <- qte2 %>% select(-c(1, 4, 5, 10, 21:23, 25))
  df <- qte2 %>% select(-c(2, 4, 5, 10, 21:23, 25))
  
  # saveRDS(df, "ArquivosRDS/dados.Rds")
  # rm(list = ls())
  # cat("\014")
  # library(tidyverse)
  # df <- readRDS("ArquivosRDS/dados.Rds")
  names(df) <- abjutils::rm_accent(names(df))
  names(df) <- str_to_upper(names(df))
  df <- df %>% select(c("DATE_ADDED",
                        "SYMBOL",
                        "NAME",
                        "PRICE",
                        "MAX_SUPPLY",
                        "CIRCULATING_SUPPLY",
                        "TOTAL_SUPPLY",
                        "DATA",
                        "QUANTIDADE",
                        "VALOR",
                        "CUSTO",
                        "SITUACAO",
                        "EXCHANGE",
                        "MARKET_CAP",
                        "VOLUME_24H",
                        "VOLUME_CHANGE_24H",
                        "PERCENT_CHANGE_1H",
                        "PERCENT_CHANGE_24H",
                        "PERCENT_CHANGE_7D",
                        "PERCENT_CHANGE_30D",
                        "PERCENT_CHANGE_60D",
                        "PERCENT_CHANGE_90D"))
  names(df) <- c("DATE_ADDED",
                 "SYMBOL",
                 "NAME",
                 "PRICE",
                 "MAX_SUPPLY",
                 "CIRCULATING_SUPPLY",
                 "TOTAL_SUPPLY",
                 "DATE_ACTION",
                 "AMOUNT",
                 "VALUE",
                 "EXPENSE",
                 "ACTION",
                 "EXCHANGE",
                 "MARKET_CAP",
                 "VOLUME_24H",
                 "VOLUME_CHANGE_24H",
                 "PERCENT_CHANGE_1H",
                 "PERCENT_CHANGE_24H",
                 "PERCENT_CHANGE_7D",
                 "PERCENT_CHANGE_30D",
                 "PERCENT_CHANGE_60D",
                 "PERCENT_CHANGE_90D")
  df <- tidyr::separate(df, DATE_ADDED, c("Data", "Hora"), sep = " ")
  df <- df %>% select(-Hora)
  df$Data <- lubridate::ymd(df$Data)
  df$MAX_SUPPLY <- as.numeric(df$MAX_SUPPLY)
  df$CIRCULATING_SUPPLY <- as.numeric(df$CIRCULATING_SUPPLY)
  df$TOTAL_SUPPLY <- as.numeric(df$TOTAL_SUPPLY)
  
  #str(df)
  Compra <- df %>% filter(ACTION=="COMPRA") %>% group_by(SYMBOL) %>% summarise(Compra= sum(AMOUNT), Custo= sum(EXPENSE), PM=round(sum(EXPENSE)/sum(AMOUNT), digits = 2))
  
  Venda <- df %>% filter(ACTION=="VENDA") %>% group_by(SYMBOL) %>% summarise(Venda=sum(AMOUNT), Lucro= sum(EXPENSE))
  
  df1 <- full_join(Compra, Venda, by="SYMBOL") %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
  df1$N_Moedas <- (df1$Compra-df1$Venda)
  
  df2 <- df %>% group_by(SYMBOL, EXCHANGE) %>% summarise(PRICE=mean(PRICE), 
                                                         MAX_SUPPLY=mean(MAX_SUPPLY), 
                                                         CIRCULATING_SUPPLY=mean(CIRCULATING_SUPPLY),
                                                         TOTAL_SUPPLY=mean(TOTAL_SUPPLY),
                                                         MARKET_CAP=mean(MARKET_CAP),
                                                         VOLUME_24H=mean(VOLUME_24H),
                                                         VOLUME_CHANGE_24H =mean(VOLUME_CHANGE_24H ),
                                                         PERCENT_CHANGE_1H =mean(PERCENT_CHANGE_1H ),
                                                         PERCENT_CHANGE_24H=mean(PERCENT_CHANGE_24H),
                                                         PERCENT_CHANGE_7D =mean(PERCENT_CHANGE_7D ),
                                                         PERCENT_CHANGE_30D=mean(PERCENT_CHANGE_30D),
                                                         PERCENT_CHANGE_60D=mean(PERCENT_CHANGE_60D),
                                                         PERCENT_CHANGE_90D=mean(PERCENT_CHANGE_90D))
  
  df <- full_join(df1,df2, by="SYMBOL")
  
  df$ValorConta <- df$N_Moedas*df$PRICE
  df$ValorDes <- df$Lucro+df$N_Moedas*df$PRICE-df$Custo #Valorizacao ou Desvalorizacao
  df$LorP <- ifelse(df$ValorDes<0,"PREJUIZO", "LUCRO")
  saveRDS(df, "df.Rds")
  t0 <- t1
  saveRDS(t1, file = "./time0.rds")
}

df <- readRDS("df.Rds")
