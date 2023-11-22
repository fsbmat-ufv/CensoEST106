rm(list = ls())
cat("\014")
#library("data.table")
library("tidyverse")
library("googlesheets4")
library("googledrive")
library("lubridate")

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "darah.moreira@ufv.br")
#drive_auth(cache = ".secrets", email = "fernando.bastos@ufv.br")
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
names(df1) <- str_to_upper(names(df1))
<<<<<<< HEAD:CodeInit.R
df1$DATA                  <- abjutils::rm_accent(df1$DATA                  ) 
df1$HORA                  <- abjutils::rm_accent(df1$HORA                  ) 
df1$RESIDENCIA            <- abjutils::rm_accent(df1$RESIDENCIA            ) 
df1$ESTADO                <- abjutils::rm_accent(df1$ESTADO                )
df1$PRIMEIRAVEZ           <- abjutils::rm_accent(df1$PRIMEIRAVEZ           ) 
df1$FACILIDADE            <- abjutils::rm_accent(df1$FACILIDADE            ) 
df1$CURSO                 <- abjutils::rm_accent(df1$CURSO                 ) 
df1$ANOINICIO             <- abjutils::rm_accent(df1$ANOINICIO             )
df1$SEMESTRE              <- abjutils::rm_accent(df1$SEMESTRE              ) 
df1$ASSISTENCIA           <- abjutils::rm_accent(df1$ASSISTENCIA           ) 
=======
df1$DATA                  <- abjutils::rm_accent(df1$DATA                  )
df1$HORA                  <- abjutils::rm_accent(df1$HORA                  ) 
df1$RESIDENCIA            <- abjutils::rm_accent(df1$RESIDENCIA            )
df1$ESTADO                <- abjutils::rm_accent(df1$ESTADO                ) 
df1$PRIMEIRAVEZ           <- abjutils::rm_accent(df1$PRIMEIRAVEZ           ) 
df1$FACILIDADE            <- abjutils::rm_accent(df1$FACILIDADE            )
df1$CURSO                 <- abjutils::rm_accent(df1$CURSO                 ) 
df1$ANOINICIO             <- abjutils::rm_accent(df1$ANOINICIO             ) 
df1$SEMESTRE              <- abjutils::rm_accent(df1$SEMESTRE              )
df1$ASSISTENCIA           <- abjutils::rm_accent(df1$ASSISTENCIA           )
>>>>>>> b50239179bb9265b4ef38696139ad9a2cede8e91:CodeInit2.R
df1$IDADE                 <- abjutils::rm_accent(df1$IDADE                 ) 
df1$HORASSONO             <- abjutils::rm_accent(df1$HORASSONO             )
df1$ESTUDOANTECIPADO      <- abjutils::rm_accent(df1$ESTUDOANTECIPADO      ) 
df1$HORASESTUDO           <- abjutils::rm_accent(df1$HORASESTUDO           ) 
<<<<<<< HEAD:CodeInit.R
df1$CREDITOS              <- abjutils::rm_accent(df1$CREDITOS              ) 
df1$INTERESSEAREAACADEMICA<- abjutils::rm_accent(df1$INTERESSEAREAACADEMICA)
df1$RESULTADOEST          <- abjutils::rm_accent(df1$RESULTADOEST)
df1$CURSO                  <- str_to_upper(df1$CURSO                 )
df1$PROVEDOR               <- str_to_upper(df1$PROVEDOR              )
df1$RESIDENCIA             <- str_to_upper(df1$RESIDENCIA            )
df1$ESTADO                 <- str_to_upper(df1$ESTADO                )
df1$PRIMEIRAVEZ            <- str_to_upper(df1$PRIMEIRAVEZ           )
df1$FACILIDADEEXATAS       <- str_to_upper(df1$FACILIDADEEXATAS      )
df1$OPINIAOPERIODOSREMOTOS <- str_to_upper(df1$OPINIAOPERIODOSREMOTOS)
=======
df1$CREDITOS              <- abjutils::rm_accent(df1$CREDITOS              )
df1$INTERESSEAREAACADEMICA<- abjutils::rm_accent(df1$INTERESSEAREAACADEMICA) 
df1$RESULTADOEST          <- abjutils::rm_accent(df1$RESULTADOEST          ) 
df1$DATA                  <- str_to_upper(df1$DATA                  )
df1$HORA                  <- str_to_upper(df1$HORA                  ) 
df1$RESIDENCIA            <- str_to_upper(df1$RESIDENCIA            )
df1$ESTADO                <- str_to_upper(df1$ESTADO                ) 
df1$PRIMEIRAVEZ           <- str_to_upper(df1$PRIMEIRAVEZ           ) 
df1$FACILIDADE            <- str_to_upper(df1$FACILIDADE            )
df1$CURSO                 <- str_to_upper(df1$CURSO                 ) 
df1$ANOINICIO             <- str_to_upper(df1$ANOINICIO             ) 
df1$SEMESTRE              <- str_to_upper(df1$SEMESTRE              )
df1$ASSISTENCIA           <- str_to_upper(df1$ASSISTENCIA           )
df1$IDADE                 <- str_to_upper(df1$IDADE                 ) 
df1$HORASSONO             <- str_to_upper(df1$HORASSONO             )
df1$ESTUDOANTECIPADO      <- str_to_upper(df1$ESTUDOANTECIPADO      ) 
df1$HORASESTUDO           <- str_to_upper(df1$HORASESTUDO           ) 
df1$CREDITOS              <- str_to_upper(df1$CREDITOS              )
df1$INTERESSEAREAACADEMICA<- str_to_upper(df1$INTERESSEAREAACADEMICA) 
df1$RESULTADOEST          <- str_to_upper(df1$RESULTADOEST          )
>>>>>>> b50239179bb9265b4ef38696139ad9a2cede8e91:CodeInit2.R
df1$TURMA <- "MAF105T1"
#-------------------------------------------------------------------------------
###Correcao da coluna CURSO
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="CIENCIAS DA COMPUTACAO","CIENCIA DA COMPUTACAO",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="AGRONOMIA.","AGRONOMIA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="ADMINSTRACAO","ADMINISTRACAO",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="EDUCACAO FISIC","EDUCACAO FISICA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="EDUCACAO FISICA - LICENCIATURA.","EDUCACAO FISICA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="EDUCACAO FISICA LICENCIATURA","EDUCACAO FISICA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="MAF 169. ELEMENTOS DE ESTATISTICA.","EDUCACAO FISICA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM EDUCACAO FISICA","EDUCACAO FISICA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM CIENCIAS BIOLOGICAS","CIENCIAS BIOLOGICAS",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM FISICA","FISICA",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="TECNOLOGIA DA GESTAO AMBIENTAL","GESTAO AMBIENTAL",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="TGA","GESTAO AMBIENTAL",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="TECNOLOGIA EM GESTAO AMBIENTAL","GESTAO AMBIENTAL",as.character(CURSO)))
df <- df %>% mutate(CURSO=ifelse(as.character(CURSO)=="LICENCIATURA EM QUIMICA","QUIMICA",as.character(CURSO)))
###Correcao da coluna PROVEDOR
df$PROVEDOR <- str_trim(df$PROVEDOR)#ajuda removendo os espaços excedentes antes e depois da string.
df$PROVEDOR <- ifelse(str_detect(df$PROVEDOR, pattern = "GMAIL")=="TRUE", "GMAIL", df$PROVEDOR)
df$PROVEDOR <- ifelse(str_detect(df$PROVEDOR, pattern = "UFV")=="TRUE", "UFV", df$PROVEDOR)
df$PROVEDOR <- ifelse(str_detect(df$PROVEDOR, pattern = "ICLOUD")=="TRUE", "ICLOUD", df$PROVEDOR)
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