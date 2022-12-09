
#source('1. Tratamento.R')

library(dplyr)
dados <- dados %>% select(-SH_H2O_SAFE,-SH_SAN_SAFE)

nomes <- dados$pais
dados <-dados %>% select(-pais)
row.names(dados)<-nomes

#library(Amelia)
#am<-amelia(dados,  k=1)  

#install.packages(c("zoo","xts","quantmod")) ## and perhaps mode
#install.packages("C:/Users/Hp/Downloads/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
#library(DMwR)
#dados_KNN <- knnImputation(dados)

#install.packages("mice")
library(mice)
imputed_Data <- mice(dados, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

dados_mice <- complete(imputed_Data,2)

saveRDS(dados_mice, file="C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/dados/dados_Liliana_com_imputacao_17_05_2022.RDS")




