rm(list = ls())
cat("\014")
#library("data.table")
library("tidyverse")
library("googlesheets4")
library("googledrive")
library("lubridate")

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
#drive_auth(cache = ".secrets", email = "darah.moreira@ufv.br")
drive_auth(cache = ".secrets", email = "fernando.bastos@ufv.br")
gs4_auth(token = drive_token())
arq <- "https://docs.google.com/spreadsheets/d/12Eij3jGBKshx01hStY0odE7RXdzY-lWlI2X7_PQEp3A/edit?usp=sharing"
df <- read_sheet(arq)
# ------------------------------------------------------------------------------

names(df) <- c("DataHora",
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
                "ResultadoEST",
                "SalarioHoje",
                "SalarioPosFormado")
df <- tidyr::separate(df, DataHora, c("Data", "Hora"), sep = " ")
names(df) <- abjutils::rm_accent(names(df))
names(df) <- str_to_upper(names(df))
df$RESIDENCIA            <- abjutils::rm_accent(df$RESIDENCIA            ) 
df$ESTADO                <- abjutils::rm_accent(df$ESTADO                )
df$PRIMEIRAVEZ           <- abjutils::rm_accent(df$PRIMEIRAVEZ           ) 
df$FACILIDADE            <- abjutils::rm_accent(df$FACILIDADE            ) 
df$CURSO                 <- abjutils::rm_accent(df$CURSO                 ) 
df$ANOINICIO             <- abjutils::rm_accent(df$ANOINICIO             )
df$SEMESTRE              <- abjutils::rm_accent(df$SEMESTRE              ) 
df$ASSISTENCIA           <- abjutils::rm_accent(df$ASSISTENCIA           ) 
df$IDADE                 <- abjutils::rm_accent(df$IDADE                 ) 
df$HORASSONO             <- abjutils::rm_accent(df$HORASSONO             )
df$ESTUDOANTECIPADO      <- abjutils::rm_accent(df$ESTUDOANTECIPADO      ) 
df$HORASESTUDO           <- abjutils::rm_accent(df$HORASESTUDO           ) 
df$CREDITOS              <- abjutils::rm_accent(df$CREDITOS              ) 
df$INTERESSEAREAACADEMICA<- abjutils::rm_accent(df$INTERESSEAREAACADEMICA)
df$RESULTADOEST          <- abjutils::rm_accent(df$RESULTADOEST)

df$RESIDENCIA            <- str_to_upper(df$RESIDENCIA            )
df$ESTADO                <- str_to_upper(df$ESTADO                ) 
df$PRIMEIRAVEZ           <- str_to_upper(df$PRIMEIRAVEZ           ) 
df$FACILIDADE            <- str_to_upper(df$FACILIDADE            )
df$CURSO                 <- str_to_upper(df$CURSO                 ) 
df$ANOINICIO             <- str_to_upper(df$ANOINICIO             ) 
df$SEMESTRE              <- str_to_upper(df$SEMESTRE              )
df$ASSISTENCIA           <- str_to_upper(df$ASSISTENCIA           )
df$IDADE                 <- str_to_upper(df$IDADE                 ) 
df$HORASSONO             <- str_to_upper(df$HORASSONO             )
df$ESTUDOANTECIPADO      <- str_to_upper(df$ESTUDOANTECIPADO      ) 
df$HORASESTUDO           <- str_to_upper(df$HORASESTUDO           ) 
df$CREDITOS              <- str_to_upper(df$CREDITOS              )
df$INTERESSEAREAACADEMICA<- str_to_upper(df$INTERESSEAREAACADEMICA) 
df$RESULTADOEST          <- str_to_upper(df$RESULTADOEST          )
df$TURMA <- "EST106"
df[is.na(df)] <- 0
#-------------------------------------------------------------------------------
###Correcao da coluna RESIDENCIA
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="BH","BELO HORIZONTE",as.character(RESIDENCIA)))
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="TEXAS","TEIXEIRAS",as.character(RESIDENCIA)))
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="TEIXIERAS","TEIXEIRAS",as.character(RESIDENCIA)))
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="VICOSA MG","VICOSA",as.character(RESIDENCIA)))

df <- df %>% mutate(ESTADO=ifelse(as.character(ESTADO)=="MG","MINAS GERAIS",as.character(ESTADO)))
###Correcao da coluna CURSO
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="CIENCIAS DA COMPUTACAO","CIENCIA DA COMPUTACAO",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="AGRONOMIA.","AGRONOMIA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="ADMINSTRACAO","ADMINISTRACAO",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="EDUCACAO FISIC","EDUCACAO FISICA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="EDUCACAO FISICA - LICENCIATURA.","EDUCACAO FISICA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="EDUCACAO FISICA LICENCIATURA","EDUCACAO FISICA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="MAF 169. ELEMENTOS DE ESTATISTICA.","EDUCACAO FISICA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM EDUCACAO FISICA","EDUCACAO FISICA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM CIENCIAS BIOLOGICAS","CIENCIAS BIOLOGICAS",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM FISICA","FISICA",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="TECNOLOGIA DA GESTAO AMBIENTAL","GESTAO AMBIENTAL",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="TGA","GESTAO AMBIENTAL",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="TECNOLOGIA EM GESTAO AMBIENTAL","GESTAO AMBIENTAL",as.character(CURSO)))
#df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM QUIMICA","QUIMICA",as.character(CURSO)))
###Correcao da coluna IDADE
df$IDADE <- stringr::str_sub(df$IDADE, 1, 2)
df$IDADE <- as.numeric(df$IDADE)
###Correcao da coluna RESIDENCIA
df$RESIDENCIA <- stringr::str_replace_all(df$RESIDENCIA, "[.-]", " ")#Retirar pontos
df$RESIDENCIA <- ifelse(stringr::str_detect(df$RESIDENCIA, pattern = "BH")=="TRUE", "BELO HORIZONTE", df$RESIDENCIA)
###Correcao da coluna ESTADO
df$ESTADO <- stringr::str_replace_all(df$ESTADO, "[.-]", " ")#Retirar pontos e tracos
df$ESTADO <- ifelse(stringr::str_detect(df$ESTADO, pattern = "MINAS GERAIS")=="TRUE", "MG", df$ESTADO)
df$ESTADO <- ifelse(stringr::str_detect(df$ESTADO, pattern = "BAHIA")=="TRUE", "BA", df$ESTADO)
df$ESTADO <- ifelse(stringr::str_detect(df$ESTADO, pattern = "SAO PAULO")=="TRUE", "SP", df$ESTADO)
df$ESTADO <- ifelse(stringr::str_detect(df$ESTADO, pattern = "RIO DE JANEIRO")=="TRUE", "RJ", df$ESTADO)
###Correcao da coluna PRIMEIRAVEZ
df$PRIMEIRAVEZ <- str_replace_all(df$PRIMEIRAVEZ, "NAO, TERCEIRA VEZ!", "TERCEIRA")
df$PRIMEIRAVEZ <- str_replace_all(df$PRIMEIRAVEZ, "NAO, SEGUNDA VEZ!", "SEGUNDA")
df$PRIMEIRAVEZ <- str_replace_all(df$PRIMEIRAVEZ, "SIM", "PRIMEIRA")
###Correcao da coluna OPINIAOPERIODOSREMOTOS
df$OPINIAOPERIODOSREMOTOS <- str_replace_all(df$OPINIAOPERIODOSREMOTOS, 
                                             "DEPENDE DA DISCIPLINA, EM GERAL, E MELHOR O REMOTO!", 
                                             "DEPENDE, EM GERAL, REMOTO")
df$OPINIAOPERIODOSREMOTOS <- str_replace_all(df$OPINIAOPERIODOSREMOTOS, 
                                             "PREFERE PRESENCIAL!", 
                                             "PRESENCIAL")
df$OPINIAOPERIODOSREMOTOS <- str_replace_all(df$OPINIAOPERIODOSREMOTOS, 
                                             "MELHOR DO QUE PRESENCIAL!", 
                                             "REMOTO")
df$OPINIAOPERIODOSREMOTOS <- str_replace_all(df$OPINIAOPERIODOSREMOTOS, 
                                             "DEPENDE DA DISCIPLINA, EM GERAL, E MELHOR O PRESENCIAL!", 
                                             "DEPENDE, EM GERAL, PRESENCIAL")
df$OPINIAOPERIODOSREMOTOS <- str_replace_all(df$OPINIAOPERIODOSREMOTOS, 
                                             "NAO SENTIU A DIFERENCA ENTRE O PRESENCIAL OU O REMOTO!", 
                                             "INDIFERENTE")
###Correcao da coluna SEMESTRE
#df$SEMESTRE#Jogar no excel ou refazer a pesquisa!
df$SEMESTRE <- str_replace_all(df$SEMESTRE, c("5"="5",                                                                                                                   
                                              "3"="3",                                                                                                                   
                                              "8º"="8",                                                                                                                  
                                              "8"="8",                                                                                                                   
                                              "3º"="3",                                                                                                                  
                                              "9"="9",                                                                                                                   
                                              "terceiro"="3",                                                                                                            
                                              "5º semestre"="5",                                                                                                         
                                              "Entre sexto e sétimo"="6",                                                                                                
                                              "Terceiro"="3",                                                                                                            
                                              "6"="6",                                                                                                                   
                                              "7o período"="7",                                                                                                          
                                              "9°"="9",                                                                                                                  
                                              "6º"="6",                                                                                                                  
                                              "5° semestre"="5",                                                                                                         
                                              "Não sou regular. Entre o sexto e sétimo"="6",                                                                             
                                              "Último"="",                                                                                                              
                                              "7"="7",                                                                                                                   
                                              "7 período"="7",                                                                                                           
                                              "7° período"="7",                                                                                                          
                                              "3 e 4º"="3",                                                                                                             
                                              "5° período"="5",                                                                                                          
                                              "5°"="5",                                                                                                                  
                                              "Nono"="9",                                                                                                                
                                              "5° período (1/2019)"="5",                                                                                                 
                                              "4"="4",                                                                                                                   
                                              "Sétimo"="7",                                                                                                              
                                              "8."="8",                                                                                                                  
                                              "2"="2",                                                                                                                   
                                              "9º"="9",                                                                                                                  
                                              "Terceiro Semestre"="3",                                                                                                   
                                              "4 período"="4",                                                                                                           
                                              "No sétimo período"="7",                                                                                                   
                                              "Iniciando o 3"="3",                                                                                                       
                                              "Quinto"="5",                                                                                                              
                                              "Setimo"="7",                                                                                                              
                                              "Ultimo"="",                                                                                                              
                                              "Sétimo semestre"="7",                                                                                                     
                                              "5º"="5",                                                                                                                  
                                              "7°"="7",                                                                                                                  
                                              "Sexto"="9",                                                                                                               
                                              "Estaria no 6 mas parei no 3 devido a pandemia"="3",                                                                       
                                              "9 (últumo)"="9",                                                                                                          
                                              "8°"="8",                                                                                                                  
                                              "Caloura de 2020"="3",                                                                                                     
                                              "Nem sei mais, teoricamente 7º"="7",                                                                                       
                                              "3º período (irregular), parei  no 3º período em 2020"="3",                                                                
                                              "Acho que o 3º. Não sei ao certo pois como entrei em 2020, fiz matérias isoladas de vários períodos."="3",                 
                                              "5 periodo"="5",                                                                                                           
                                              "Não tem período certo. Estou em vários períodos"="",                                                                     
                                              "Espero me formar nesse semestre, não tenho ideia de qual semestre eu estou, ingressei no primeiro semestre de 2016. "="11",
                                              "Quarto com um pé no terceiro"="4",                                                                                        
                                              "4º semestre"="4",                                                                                                         
                                              "teoricamente no sexto, porem algumas materias atrasadas"="6",                                                             
                                              "7º"="7",                                                                                                                  
                                              "9semestre"="9",                                                                                                           
                                              "2º Semestre"="2",                                                                                                         
                                              "10"="10",                                                                                                                  
                                              "8º periodo"="8",                                                                                                          
                                              "sétimo"="7",                                                                                                              
                                              "6º período"="9",                                                                                                          
                                              "5 período"="5",                                                                                                           
                                              "Segundo"="2",                                                                                                             
                                              "5° Período."="5",                                                                                                         
                                              "5º período"="5",                                                                                                          
                                              "3°"="3",                                                                                                                  
                                              "6°"="9",                                                                                                                  
                                              "3ºperíodo"="3",                                                                                                           
                                              "4°"="4",                                                                                                                  
                                              "3° Periodo"="3",                                                                                                          
                                              "Em geral, todos"="",                                                                                                     
                                              "Terceiro semestre"="3",                                                                                                   
                                              "Terceiro período."="3",                                                                                                   
                                              "3° período"="3"))
df$SEMESTRE <- str_replace_all(df$SEMESTRE,"Acho que o 3. Não sei ao certo pois como entrei em 2020, fiz matérias isoladas de vários períodos.","3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("5 (1/2019)"),"5")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3 Semestre"),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("7 semestre"),"7")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("9 (últumo)"),"9")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3 período (irregular), parei  no 3 período em 2020"),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("Acho que o 3. Não sei ao certo pois como entrei em 2020, fiz matérias isoladas de vários períodos."),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("Quarto com um pé no 3"),"4")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("8periodo"),"8")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("6 período"),"6")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("5 Período."),"5")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3período"),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3 Periodo"),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3 semestre"),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3 período."),"3")
df$SEMESTRE <- str_replace_all(df$SEMESTRE,fixed("3 período"),"3")
df$SEMESTRE <- as.numeric(df$SEMESTRE)
saveRDS(df, "censo.Rds")
df <- readRDS("censo.Rds")


#Teste Rstudio Cloud