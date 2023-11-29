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

##########----------------------------------------------------------------------
#####Distribuicao de Frequencia - RESIDENCIA
##########----------------------------------------------------------------------
tab1 <- df %>% group_by(RESIDENCIA) %>% summarise(Freq=n(), .groups = 'drop' )
tab1$FreqR <- round(tab1$Freq/sum(tab1$Freq), digits = 2)
tab1$FreqACM <- cumsum(tab1$Freq)
tab1$FreqRACM <- cumsum(tab1$FreqR)
Total <- c("Total", sum(tab1$Freq), sum(tab1$FreqR), "-", "-")
tab1 <- rbind(tab1, Total)

tab1%>% 
  addHtmlTableStyle(col.rgroup = c("none", "#F9FAF0"),
                    col.columns = c("none", "#F1F0FA")) %>% 
  htmlTable(total = TRUE, rnames = FALSE,
            caption = "Tabela de Frequencia")

###Correcao da coluna RESIDENCIA
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="BH","BELO HORIZONTE",as.character(RESIDENCIA)))
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="TEXAS","TEIXEIRAS",as.character(RESIDENCIA)))
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="TEIXIERAS","TEIXEIRAS",as.character(RESIDENCIA)))
df <- df %>% mutate(RESIDENCIA=ifelse(as.character(RESIDENCIA)=="VICOSA MG","VICOSA",as.character(RESIDENCIA)))

#Tabela Corrigida

tab1 <- df %>% group_by(RESIDENCIA) %>% summarise(Freq=n(), .groups = 'drop' )
tab1$FreqR <- round(tab1$Freq/sum(tab1$Freq), digits = 2)
tab1$FreqACM <- cumsum(tab1$Freq)
tab1$FreqRACM <- cumsum(tab1$FreqR)
Total <- c("Total", sum(tab1$Freq), sum(tab1$FreqR), "-", "-")
tab1 <- rbind(tab1, Total)

tab1%>% 
  addHtmlTableStyle(col.rgroup = c("none", "#F9FAF0"),
                    col.columns = c("none", "#F1F0FA")) %>% 
  htmlTable(total = TRUE, rnames = FALSE,
            caption = "Tabela de Frequencia")

#Grafico de barra
# %>% ctrl+shift+m printa o comando piple
plot <- df %>% 
  group_by(RESIDENCIA) %>% 
  summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(RESIDENCIA, count), 
             text = paste("Curso: ", 
                          RESIDENCIA, "<br>",
                          "Número de estudantes: ", 
                          count), 
             count, 
             fill=RESIDENCIA)) + 
  geom_col(position = "dodge", show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) +
  coord_flip() + xlab("Residência") + ylab("Nº de Estudantes")

plot %>% plotly::ggplotly(tooltip = "text") %>% 
  plotly::layout(showlegend=FALSE)


##########----------------------------------------------------------------------
#####Distribuicao de Frequencia - ESTADO
##########----------------------------------------------------------------------
tab1 <- df %>% group_by(ESTADO) %>% summarise(Freq=n(), .groups = 'drop' )
tab1$FreqR <- round(tab1$Freq/sum(tab1$Freq), digits = 2)
tab1$FreqACM <- cumsum(tab1$Freq)
tab1$FreqRACM <- cumsum(tab1$FreqR)
Total <- c("Total", sum(tab1$Freq), sum(tab1$FreqR), "-", "-")
tab1 <- rbind(tab1, Total)

tab1%>% 
  addHtmlTableStyle(col.rgroup = c("none", "#F9FAF0"),
                    col.columns = c("none", "#F1F0FA")) %>% 
  htmlTable(total = TRUE, rnames = FALSE,
            caption = "Tabela de Frequencia")

##########----------------------------------------------------------------------
#####Distribuicao de Frequencia - CURSO
##########----------------------------------------------------------------------
tab1 <- df %>% group_by(CURSO) %>% summarise(Freq=n(), .groups = 'drop' )
tab1$FreqR <- round(tab1$Freq/sum(tab1$Freq), digits = 2)
tab1$FreqACM <- cumsum(tab1$Freq)
tab1$FreqRACM <- cumsum(tab1$FreqR)
Total <- c("Total", sum(tab1$Freq), sum(tab1$FreqR), "-", "-")
tab1 <- rbind(tab1, Total)

tab1%>% 
  addHtmlTableStyle(col.rgroup = c("none", "#F9FAF0"),
                    col.columns = c("none", "#F1F0FA")) %>% 
  htmlTable(total = TRUE, rnames = FALSE,
            caption = "Tabela de Frequencia")

##########----------------------------------------------------------------------
#####Distribuicao de Frequencia - ANOINICIO
##########----------------------------------------------------------------------
tab1 <- df %>% group_by(ANOINICIO) %>% summarise(Freq=n(), .groups = 'drop' )
tab1$FreqR <- round(tab1$Freq/sum(tab1$Freq), digits = 2)
tab1$FreqACM <- cumsum(tab1$Freq)
tab1$FreqRACM <- cumsum(tab1$FreqR)
Total <- c("Total", sum(tab1$Freq), sum(tab1$FreqR), "-", "-")
tab1 <- rbind(tab1, Total)

tab1%>% 
  addHtmlTableStyle(col.rgroup = c("none", "#F9FAF0"),
                    col.columns = c("none", "#F1F0FA")) %>% 
  htmlTable(total = TRUE, rnames = FALSE,
            caption = "Tabela de Frequencia")


##########----------------------------------------------------------------------
#####Distribuicao de Frequencia - SEMESTRE
##########----------------------------------------------------------------------
tab1 <- df %>% group_by(SEMESTRE) %>% summarise(Freq=n(), .groups = 'drop' )
tab1$FreqR <- round(tab1$Freq/sum(tab1$Freq), digits = 2)
tab1$FreqACM <- cumsum(tab1$Freq)
tab1$FreqRACM <- cumsum(tab1$FreqR)
Total <- c("Total", sum(tab1$Freq), sum(tab1$FreqR), "-", "-")
tab1 <- rbind(tab1, Total)

tab1%>% 
  addHtmlTableStyle(col.rgroup = c("none", "#F9FAF0"),
                    col.columns = c("none", "#F1F0FA")) %>% 
  htmlTable(total = TRUE, rnames = FALSE,
            caption = "Tabela de Frequencia")

str(df$SEMESTRE)
df$SEMESTRE <- as.numeric(as.character(df$SEMESTRE))
df[is.na(df)] <- 0




df1 <- df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop' )
barplot(height=df1$count, names=df1$CURSO, col="#69b3a2")
barplot(height=df1$count, names=df1$CURSO, col="#69b3a2", horiz=T , las=1)
barplot(height=df1$count, names=df1$CURSO, col="#69b3a2", horiz=T , las=1)
df1Order <- df1[order(df1$count, decreasing=T),]
par(mar = c(5, 25, 5, 5))
barplot(df1Order$count,
        names.arg=df1Order$CURSO, 
        col="#69b3a2", 
        horiz=T , 
        las=1)

df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop' ) %>% 
  ggplot(aes(CURSO, count)) + geom_col()

df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop' ) %>% 
  ggplot(aes(CURSO, count, fill=CURSO)) + geom_col()

df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(CURSO, count, fill=CURSO)) + geom_col(show.legend = F)

df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(CURSO, count), count, fill=CURSO)) + geom_col(show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(CURSO, count), count, fill=CURSO)) + geom_col(show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))


plot1 <- df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(CURSO, count), text = paste("Curso: ", CURSO, "<br>",
                                                 "Número de estudantes: ", count), count, fill=CURSO)) + geom_col(show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) + 
  ylab("Quantidade de Estudantes") + 
  xlab("Cursos") + 
  ggtitle("Cursos participantes e Número de Estudantes\n da Disciplina Estatística")
plotly::ggplotly(plot1, tooltip = "text") %>% 
  plotly::layout(showlegend=FALSE)

plot2 <- df %>% group_by(CURSO) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(CURSO, count), text = paste("Curso: ", CURSO, "<br>",
                                                 "Número de estudantes: ", count), count, fill=CURSO)) + 
  geom_col(position = "dodge", show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) +
  coord_flip() + xlab("Cursos") + ylab("Nº de Estudantes")
plotly::ggplotly(plot2, tooltip = "text") %>% 
  plotly::layout(showlegend=FALSE)

############################################
plot1 <- df %>% group_by(PROVEDOR) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(PROVEDOR, count), text = paste("Provedor: ", PROVEDOR, "<br>",
                                                    "Número de estudantes: ", count), count, fill=PROVEDOR)) + geom_col(show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) + 
  ylab("Quantidade de Estudantes") + 
  xlab("Provedor de Email") + 
  ggtitle("Provedores de Email Utilizados pelos Estudantes")
plotly::ggplotly(plot1, tooltip = "text") %>% 
  plotly::layout(showlegend=FALSE)
###

df1 <- df %>% group_by(PROVEDOR) %>% 
  summarise(count=n(), .groups = 'drop')
df1 <- df1 %>% mutate(Variavel="PROVEDOR")
ggplot(df1, aes(x = Variavel, y = count, fill = PROVEDOR)) +
  geom_col() +
  coord_flip()+
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Frequência") +
  guides(fill=guide_legend(" "))+
  xlab(NULL)+
  ggtitle("Gráfico de Barras Empilhadas")+
  theme(legend.position="bottom")
##
df1 <- df %>% 
  group_by(PROVEDOR) %>% 
  summarise(frequencia = 100*(n()/140), .groups = 'drop')
df1 <- df1 %>% mutate(Variavel="Provedor")

ggplot(df1, aes(x = Variavel, y = frequencia, fill = PROVEDOR)) +
  geom_col() +
  coord_flip()+
  geom_text(aes(label = paste0(round(frequencia, digits = 2), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Frequência") +
  guides(fill=guide_legend(" "))+
  xlab(NULL)+
  ggtitle("Gráfico de Barras Empilhadas")+
  theme(legend.position="bottom")
###
contagem = table(df$PROVEDOR)
nomes = levels(df$PROVEDOR)
dados = data.frame(round(contagem/sum(contagem)*100,2))
dados <- within(dados, {
  Var <- factor(Var1, labels=c('Gmail','ICLOUD','PROTONMAIL','UFV'))
})
attach(dados)
dados <- dados[order(Freq),] 
detach(dados)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


library(scales)
pizza<-ggplot(dados, aes(x="", y=Freq, fill=Var))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y")+ggtitle("Provedores Utilizados")

pizza + scale_fill_brewer(palette="Dark2") +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/100)), data = dados, size=5)
###
ni<-table(df$PROVEDOR) # Calcula a tabela de frequências absolutas e armazena o resultado em 'mytab'
fi<-prop.table(ni) # Tabela de frequências relativas (f_i)
p_fi<-100*prop.table(ni) # Porcentagem (100 f_i)

# Adiciona linhas de total
ni<-c(ni,sum(ni)) 
fi<-c(fi,sum(fi))
p_fi<-c(p_fi,sum(p_fi))
names(ni)[5]<-"Total"
df2<-cbind(ni,fi=round(fi,digits=2),p_fi=round(p_fi,digits=2))
labs<-paste(1:4,"(",df2[1:4,1],";",round(df2[1:4,3],1),"%)",sep="")
pie(table(df$PROVEDOR),labels=labs, main = "Gráfico em setores para Provedores.",
    sub = "Fonte: Professor")
#title("Figura 2.3: Gráfico em setores para a variável Y: grau de instrução")
legend(-1.1,-0.8,legend=c("1=Gmail, 2=ICLOUD, 3=PROTONMAIL", "4=UFV"),border=NA,box.col=NA)

#####################################################

ggplot(df) +
  aes(x = RESIDENCIA, fill = RESIDENCIA) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Cidades",
    y = "Número de Estudantes",
    title = "Cidade de Residência dos Estudantes"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

plot1 <- df %>% group_by(RESIDENCIA) %>% summarise(count=n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(RESIDENCIA, count), text = paste("Munícipio de Residência: ", RESIDENCIA, "<br>",
                                                      "Número de estudantes: ", count), count, fill=RESIDENCIA)) + 
  geom_col(position = "dodge", show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) +
  coord_flip() + xlab("Cursos") + ylab("Nº de Estudantes")
plotly::ggplotly(plot1, tooltip = "text") %>% 
  plotly::layout(showlegend=FALSE)

df1 <- df %>% group_by(RESIDENCIA) %>% summarise(count=n(), .groups = 'drop')
#dfOrder <- arrange(df1, RESIDENCIA)
dfOrder <- df1 %>% mutate(RESIDENCIA = fct_rev(RESIDENCIA))

plot1 <- df1 %>% mutate(RESIDENCIA = fct_rev(RESIDENCIA)) %>% ggplot(aes(RESIDENCIA, text = paste("Munícipio de Residência: ", RESIDENCIA, "<br>",
                                                                                                  "Número de estudantes: ", count), count, fill=RESIDENCIA)) + 
  geom_col(position = "dodge", show.legend = F) + 
  geom_text(aes(label = count), nudge_y = 1, size=5) +
  coord_flip()+ xlab("Cursos") + ylab("Nº de Estudantes")
plotly::ggplotly(plot1, tooltip = "text") %>% 
  plotly::layout(showlegend=FALSE)
