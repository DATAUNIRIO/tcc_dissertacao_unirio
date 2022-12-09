
# IMPACTO DO COVID EM PESSOAS VUNERAVEIS
# https://github.com/Gabriel-Assuncao/PNADcIBGE-RDPC/blob/main/PNADcIBGE-RDPC.R
# https://rpubs.com/BragaDouglas/335574
# https://rpubs.com/gabriel-assuncao-ibge/pnadc


# baixa escolaridade, baixa renda, desempregado,
#trabalhador informal, pessoa que faz bico, domestico
#

# Aumento do desemprego para pessoas 
# queda da renda
# aumento do trabalho informal


# RENDA MÉDIA por Ano (2019 2021)
# OCUPAÇÃO por Ano (2019 2021)
# Taxa informalidade por Ano (2019 2021)

#informalidade =  (0 = qualquer tipo ,
#                  1= trabalhador por conta própria)

# Militar, CLT, empregador, trabalhador por conta própria, dona de casa

#ifelse(V4012==6,"informal","nao informal")
#mean(renda ~informal) em 2019 
#mean(renda ~informal) em 2021 

#------------------------------------------------

# periodo de analise
#fim de 2019 e fim de 2021

library(PNADcIBGE) 
library(survey)
library(convey)

# Carregar a base de dados
#                                                                              RENDA   EDUCA            
PNAD_antes = get_pnadc(year = 2019, quarter = 4,vars = c("VD4001", "VD4002","VD4020","VD3001","UF"))
PNAD_depois = get_pnadc(year = 2021, quarter = 3,vars = c("VD4001", "VD4002","VD4020","VD3001","UF"))

# comparar a renda média antes e depois
mediarendaantes <- svymean(~VD4020, PNAD_antes, na.rm = T)
mediarendaantes
mediarendadepois <- svymean(~VD4020, PNAD_depois, na.rm = T)
mediarendadepois

# comparar a renda média antes e depois por UF
renda_antes  <- svyby(~VD4020, by = ~UF, PNAD_antes, svymean, na.rm  =  TRUE)
renda_depois <- svyby(~VD4020, by = ~UF, PNAD_depois, svymean, na.rm  =  TRUE)
library(deflateBR)
deflate(2907.85, as.Date("2019-12-01"), "12/2019", "ipca")

# comparar o desemprego antes e depois
tx_desemprego_antes <- svymean(~VD4002, PNAD_antes, na.rm = T)
tx_desemprego_antes
tx_desemprego_depois <- svymean(~VD4002, PNAD_depois, na.rm = T)
tx_desemprego_depois

# comparar o desemprego antes e depois por UF
tx_desemprego_antes_UF  <- svyby(~VD4002, by = ~UF, PNAD_antes, svymean, na.rm  =  TRUE)
tx_desemprego_depois_UF <- svyby(~VD4002, by = ~UF, PNAD_depois, svymean, na.rm  =  TRUE)

#----------------------------------------------------------------------------------------
#     sem  O PESO
#----------------------------------------------------------------------------------------
PNAD_antes_SEM  = get_pnadc(year = 2019, quarter = 4,vars = c("VD4001", "VD4002","VD4020","VD3001","UF"),design = FALSE)
PNAD_depois_SEM = get_pnadc(year = 2021, quarter = 3,vars = c("VD4001", "VD4002","VD4020","VD3001","UF"),design = FALSE)

library(dplyr)
# PESSOAS_COM_RENDA_ABAIXO_DE_1100
baixa_renda <- PNAD_antes_SEM %>% filter(VD4020 <= 1100)
table(baixa_renda$VD4001)
















#NÃO FUNCIONOU!
# 
# ANTES   <- svymean(~VD4020, subset(PNAD_antes, VD4002 == "Pessoas desocupadas"), na.rm = T)
# ANTES
# DEPOIS  <- svymean(~VD4020, subset(PNAD_depois, VD4002 == "Pessoas desocupadas"), na.rm = T)
# DEPOIS
# 
# 
# tx_desemprego_depois <- svymean(~VD4002, subset(PNAD_depois, VD4020 < 1000) , na.rm = T)
# tx_desemprego_depois
# 
# 
# 
# 
# txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", 
#                         subset(PNAD_antes, VD4020 < 5000) , na.rm = T)
# txdesocup25
# 
# txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho",  PNAD_antes,na.rm = T)
# txdesocup25
# 
# PNAD_antes_v <- subset(PNAD_antes, VD4020 < 1000) 
# txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho",  PNAD_antes_v,na.rm = T)
# txdesocup25
# 
# 
# PNAD_antes<-subset(PNAD_antes,x>4)
# 
# 
# 
# #giniUF <- svyby(~VD4020, by = ~UF, dadosPNADc, svygini, na.rm  =  TRUE)
# #giniUF
# 
# 
# 
# 
# # comparar a  desigualdade antes e depois
gini_antes  <- svygini(~VD4020, PNAD_antes, na.rm  =  TRUE)
gini_depois <- svygini(~VD4020, PNAD_depois, na.rm  =  TRUE)
# 
