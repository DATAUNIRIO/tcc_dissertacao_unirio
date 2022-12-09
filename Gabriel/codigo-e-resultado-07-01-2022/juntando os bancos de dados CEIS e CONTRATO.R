
# banco de contratos
#banco_completo <- readRDS("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_completo.Rds")
banco_completo<- readRDS('/home/steven/Downloads/banco_completo.Rds')

names(banco_completo)
# selecionar as variaveis importantes
banco_completo <-banco_completo[,c(1:3,7:10,12:18)]

# banco CEIS
library(readr)
library(janitor)
#banco_CEIS <- read_delim("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/dados/CEIS.csv", 
#                         ";" ,escape_double = FALSE, trim_ws = TRUE) %>% clean_names()
banco_CEIS <- read_delim("/home/steven/Downloads/CEIS.csv", 
                         ";" ,escape_double = FALSE, trim_ws = TRUE) %>% clean_names()

# selecionar as variaveis importantes
names(banco_CEIS)
banco_CEIS <- banco_CEIS[,c(1,2,4,7,17:18)]

# separar o CPF e CNPJ da base CEIS
library(dplyr)
banco_CEIS_PJ <- banco_CEIS %>% filter(tipo_de_pessoa=="J")
#banco_CEIS_PF <- banco_CEIS %>% filter(tipo_de_pessoa=="F")
remove(banco_CEIS)

# separar o CPF e o CNPJ da base completa
library(tidyr)
banco_completo_PJ <-banco_completo %>% drop_na(`CNPJ Contratada`)
#banco_completo_PF <-banco_completo %>% drop_na(`CPF Contratada`)
remove(banco_completo)


# tirar o tipo de pessoa
banco_completo_PJ<-banco_completo_PJ[,c(1:7,9:14)]

#CNPJ CEIS = 92136704000104
#CNPJ CEIS = 92.136.704/0001-04
#CNPJ CONTRATOS = Fornecedor 03.701.471/0001-15: ORGAL VIGILANCIA E SEGURANCA LTDA

# jogando fora a palavra "Fornecedor"
banco_completo_PJ$cnpj <- banco_completo_PJ$`CNPJ Contratada`
banco_completo_PJ$cnpj <- gsub("Fornecedor","",banco_completo_PJ$cnpj)  

# dividindo o cnpj e  o nome da empresa
banco_completo_PJ<-banco_completo_PJ %>% separate(cnpj, c("cnpj", "nome"),sep=":")
# tirando o "." o "-" e a "/"
banco_completo_PJ$cnpj <- gsub("\\.","",banco_completo_PJ$cnpj)
banco_completo_PJ$cnpj <- gsub("\\-","",banco_completo_PJ$cnpj)
banco_completo_PJ$cnpj <- gsub("\\/","",banco_completo_PJ$cnpj)

banco_CEIS_PJ <-rename(banco_CEIS_PJ, cnpj = cpf_ou_cnpj_do_sancionado)

#verificando a classe
class(banco_completo_PJ$cnpj)
class(banco_CEIS_PJ$cnpj)

banco_completo_PJ$cnpj <- gsub("\\W", "", banco_completo_PJ$cnpj)

# juntar as duas bases de dados
BANCO <- banco_completo_PJ %>% left_join(banco_CEIS_PJ)
table(BANCO$tipo_de_pessoa)

#Empresas Inidôneas e Suspensas - EIS
BANCO$EIS <- ifelse(is.na(BANCO$tipo_de_pessoa),0,BANCO$tipo_de_pessoa)
BANCO$EIS <- ifelse(BANCO$EIS=="J",1,0)
table(BANCO$EIS)

#BANCO$resposta <- ifelse(BANCO$tipo_de_pessoa=="J","1. Suspenso","2. Nao suspenso")

remove(banco_CEIS_PF,banco_CEIS_PJ,banco_completo_PF,banco_completo_PJ)

# tratamento da variável "valor"
BANCO <-rename(BANCO, valor = `Valor inicial`)
BANCO$valor <- gsub("R\\$", "", BANCO$valor)
BANCO$valor <- gsub("\\.", "", BANCO$valor)
BANCO$valor <- gsub("\\,", ".", BANCO$valor)
BANCO$valor <-as.double(BANCO$valor)
summary(BANCO$valor)

BANCO <-rename(BANCO,aditivos =`Número de Aditivos`)
BANCO$aditivos<-as.numeric(BANCO$aditivos)
BANCO$aditivos <- ifelse(is.na(BANCO$aditivos),0,BANCO$aditivos)
summary(BANCO$aditivos)
BANCO <-rename(BANCO,modalidade =`Modalidade da Licitação`)

# dividindo o cnpj e  o nome da empresa
#table(BANCO$UASG) %>% data.frame() %>% View()

BANCO<-BANCO %>% separate(UASG, c("num_uasg", "uasg"),sep=":")

BANCO$uasg2 <-ifelse(BANCO$num_uasg>15000, 'EDUCACAO',
              ifelse(BANCO$num_uasg>13000, 'AGRICULTURA',
              ifelse(BANCO$num_uasg>12000, 'MILITAR',
              ifelse(BANCO$num_uasg>110096,'ADMINISTRACAO',
              ifelse(BANCO$num_uasg>110062 ,'PROCURADORIA',
              'OUTROS')))))
table(BANCO$uasg2)

#EIS
#valor
#num aditivos
#uasg2
#modalidae
