rm(list = ls())
cat("\014")
#library("data.table")
library("tidyverse")
library("googlesheets4")
library("googledrive")
library("lubridate")

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "fernando.bastos@ufv.br")
gs4_auth(token = drive_token())
arq <- "https://docs.google.com/spreadsheets/d/12Eij3jGBKshx01hStY0odE7RXdzY-lWlI2X7_PQEp3A/edit?usp=sharing"
df1 <- read_sheet(arq)
# ------------------------------------------------------------------------------

names(df1) <- c("DataHora",
                "Residencia",
                "Estado",
                "PrimeiraVez",
                "Facilidade",
                "Curso",
                "AnoInicio",
                "Semestre",
                "Assistencia",
                "Idade",
                "HorasSono",
                "EstudoAntecipado",
                "HorasEstudo",
                "Creditos",
                "InteresseAreaAcademica",
                "ResultadoEST")
df1 <- tidyr::separate(df1, DataHora, c("Data", "Hora"), sep = " ")
names(df1) <- abjutils::rm_accent(names(df1))
