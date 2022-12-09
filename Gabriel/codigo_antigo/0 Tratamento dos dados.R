#How to disable scientific notation
#options(scipen=999)
#options(digits = 15)


library(readxl)
CEIS <- read_excel("dados/CEIS.xlsx")
head(CEIS)

# corrigindo os nomes das variaveis
nomes<-names(CEIS)
apelidos<-nomes
apelidos<-tolower(apelidos)
apelidos<-gsub(" ","_",apelidos)
apelidos<-gsub("ç","c",apelidos)
apelidos<-gsub("ã","a",apelidos)
apelidos<-gsub("õ","o",apelidos)
apelidos<-gsub("ó","o",apelidos)
apelidos<-gsub("ê","e",apelidos)
apelidos<-gsub("â","a",apelidos)
apelidos<-gsub("ú","u",apelidos)
colnames(CEIS)<-apelidos
colnames(CEIS)[17]<-"abragencia"
  
# colocando tudo em minuscula
N<-22

for(i in 1:N){
  CEIS[[i]]<-tolower(CEIS[[i]])
  cat("\r", i, "of", N)
}

head(CEIS)
names(CEIS)
head(CEIS$tipo_sancao)

# removendo os caracteres especiais
CEIS$tipo_sancao<-chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
                         "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc",
                         CEIS$tipo_sancao)

CEIS$orgao_sancionador<-chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
                         "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc",
                         CEIS$orgao_sancionador)
head(CEIS$orgao_sancionador)


# construção da variável resposta
CEIS$tipo_orgao <- ifelse(grepl("do estado", CEIS$orgao_sancionador, ignore.case = T), "Estado", 
                   ifelse(grepl("estadual",  CEIS$orgao_sancionador, ignore.case = T), "Estado",
                   ifelse(grepl("estaduais",  CEIS$orgao_sancionador, ignore.case = T), "Estado",
                   ifelse(grepl("distrito federal",  CEIS$orgao_sancionador, ignore.case = T), "Estado",
                   ifelse(grepl("cemig",  CEIS$orgao_sancionador, ignore.case = T), "Estado",
                   ifelse(grepl("policia",  CEIS$orgao_sancionador, ignore.case = T), "Estado",
                   ifelse(grepl("pge",  CEIS$orgao_sancionador, ignore.case = T), "Estado",

                   ifelse(grepl("prefeitura",  CEIS$orgao_sancionador, ignore.case = T), "Municipio",
                   ifelse(grepl("municipal",  CEIS$orgao_sancionador, ignore.case = T), "Municipio",
                   
                   ifelse(grepl("federal",  CEIS$orgao_sancionador, ignore.case = T), "Federal",
                   ifelse(grepl("uniao",  CEIS$orgao_sancionador, ignore.case = T), "Federal",
                   ifelse(grepl("ministerio",  CEIS$orgao_sancionador, ignore.case = T), "Federal",
                   ifelse(grepl("banco do brasil",  CEIS$orgao_sancionador, ignore.case = T), "Federal",
                   ifelse(grepl("nacional",  CEIS$orgao_sancionador, ignore.case = T), "Federal",                                 
                   ifelse(grepl("brasileiro",  CEIS$orgao_sancionador, ignore.case = T), "Federal",                                 
                   ifelse(grepl("brasileira",  CEIS$orgao_sancionador, ignore.case = T), "Federal",  
                   ifelse(grepl("tribunal regional eleitoral",  CEIS$orgao_sancionador, ignore.case = T), "Estado",
                   "Outros")))))))))))))))))
# outros ~= institutos
table(CEIS$tipo_orgao)

# teste temporario
CEIS_temporario<- CEIS
CEIS_temporario<-CEIS_temporario[CEIS_temporario$tipo_orgao=="Outros",]
table(CEIS_temporario$orgao_sancionador)


#------------------------------------------------------------------------------
# diversos tipos de variavel resposta
CEIS$tipo_sancao_2 <- ifelse(grepl("proibicao", CEIS$tipo_sancao, ignore.case = T), "Proibição", 
                      ifelse(grepl("impedimento",  CEIS$tipo_sancao, ignore.case = T), "Impedimento",
                      ifelse(grepl("suspensao",  CEIS$tipo_sancao, ignore.case = T), "Suspensão",
                      ifelse(grepl("inidoneidade",  CEIS$tipo_sancao, ignore.case = T), "Inidoneidade",
                      ifelse(grepl("decisao judicial",  CEIS$tipo_sancao, ignore.case = T), "Decisão judicial",
                      ifelse(grepl("requisicao",  CEIS$tipo_sancao, ignore.case = T), "Requisição",
                             "Outros"))))))
table(CEIS$tipo_sancao_2)

CEIS$tipo_sancao_3 <- ifelse(grepl("Inidoneidade", CEIS$tipo_sancao_2, ignore.case = T), 1, 0)
table(CEIS$tipo_sancao_3)

CEIS$tipo_sancao_4 <- ifelse(grepl("Inidoneidade", CEIS$tipo_sancao_2, ignore.case = T), "2 Inidoneidade",
                      ifelse(grepl("Impedimento", CEIS$tipo_sancao_2, ignore.case = T), "1 Impedimento",
                      "0 Outros"))                               
table(CEIS$tipo_sancao_4)

CEIS$tipo_sancao_5 <- ifelse(grepl("Inidoneidade", CEIS$tipo_sancao_2, ignore.case = T), "3 Inidoneidade",
                      ifelse(grepl("Impedimento", CEIS$tipo_sancao_2, ignore.case = T), "2 Impedimento",
                     ifelse(grepl("Suspensão", CEIS$tipo_sancao_2, ignore.case = T), "1 Suspensão",
                                    "0 Proibição/Requisição/Decisão judicial")))
table(CEIS$tipo_sancao_5)

table(CEIS$abragencia_definida_em_decisao_judicial)

CEIS$abragencia<-gsub("do órgão sancionador","",CEIS$abragencia)

2002/12/30

#http://material.curso-r.com/lubridate/
CEIS$ano <- zoo::year(CEIS$data)

#Para anos:
CEIS$tempo<-difftime(hojes,dados$datas,units="days")/365.25


# Fazer: 
# 1 Região Norte, Nordeste, etc
# 2 Modelo logit/probit vs modelo machine learning
# 3 matriz de confusão ou curva ROC
# 4 analise exploratoria de dados


