
# banco de contratos
banco_completo <- readRDS("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_completo.Rds")
names(banco_completo)
# selecionar as variaveis importantes
banco_completo <-banco_completo[,c(1:3,7:10,12:18)]

# banco CEIS
library(readr)
library(janitor)
banco_CEIS <- read_delim("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/dados/CEIS.csv", 
                         ";" ,escape_double = FALSE, trim_ws = TRUE) %>% clean_names()

# selecionar as variaveis importantes
banco_CEIS <- banco_CEIS[,c(1:5,7:14,17:19)]

# separar o CPF e CNPJ da base CEIS
library(dplyr)
banco_CEIS_PJ <- banco_CEIS %>% filter(tipo_de_pessoa=="J")
banco_CEIS_PF <- banco_CEIS %>% filter(tipo_de_pessoa=="F")
remove(banco_CEIS)

# separar o CPF e o CNPJ da base completa
banco_completo_PJ <-banco_completo %>% drop_na(`CNPJ Contratada`)
banco_completo_PF <-banco_completo %>% drop_na(`CPF Contratada`)
remove(banco_completo)

#CNPJ CEIS = 92136704000104
#CNPJ CEIS = 92.136.704/0001-04
#CNPJ CONTRATOS = Fornecedor 03.701.471/0001-15: ORGAL VIGILANCIA E SEGURANCA LTDA

# jogando fora a palavra "Fornecedor"
banco_completo_PJ$cnpj <- banco_completo_PJ$`CNPJ Contratada`
banco_completo_PJ$cnpj <- gsub("Fornecedor","",banco_completo_PJ$cnpj)  

# dividindo o cnpj e  o nome da empresa
library(tidyr)
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


#BANCO$resposta <- ifelse(BANCO$tipo_de_pessoa=="J","1. Suspenso","2. Nao suspenso")
BANCO$resposta <- ifelse(BANCO$tipo_de_pessoa=="J",1,0)
BANCO$resposta <- ifelse(is.na(BANCO$resposta),0,BANCO$resposta)
table(BANCO$resposta)


table(BANCO$resposta)
remove(banco_CEIS_PF,banco_CEIS_PJ,banco_completo_PF,banco_completo_PJ)


BANCO <-rename(BANCO, valor = `Valor inicial`)
BANCO$valor <- gsub("R\\$", "", BANCO$valor)
BANCO$valor <- gsub("\\.", "", BANCO$valor)
BANCO$valor <- gsub("\\,", ".", BANCO$valor)
BANCO$valor <-as.double(BANCO$valor)
summary(BANCO$valor)

# modelo logit
modelo  <- glm(resposta ~ valor , family = binomial(link = "logit"), data = BANCO)
summary(modelo)

# modelo randomForest
library(randomForest)
floresta <- randomForest(resposta ~ valor , data = BANCO)
table(BANCO$resposta)




#0. COLETA DE DADOS  = 90% FEITO
#1. CRIAÇÃO DA BASE DE DADOS 
#2. TRATAMENTO (AGRUPAR AS UASG)
#3. ROSE OU SMOTE
#4. APRENDIZADO DE MAQUINAS
#5. INTERPRETAR RESULTADOS
#6. ESCREVER TEXTO DO TCC

  




