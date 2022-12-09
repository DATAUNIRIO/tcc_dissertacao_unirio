load("C:/Users/Hp/Desktop/rian_uff/environment5.RData")
## 75% of the sample size
smp_size <- floor(0.75 * nrow(peds))

## set the seed to make your partition reproducible
set.seed(12345)
train_ind <- sample(seq_len(nrow(peds)), size = smp_size)

train <- peds[train_ind, ]
test  <- peds[-train_ind, ]

remove(train_ind,smp_size)

# PROBLEMA
# desequilibrio entre as categorias
# ileso só tem 1,38%
# isso dá uma má qualidade de ajuste
# sugestao: somar as categorias ileso e lesoes leves

table(peds$estado_fisico)
prop.table(table(peds$estado_fisico))*100
library(nnet)
modelo_treino <- multinom(formula = estado_fisico ~ tipo_veiculo + fase_dia + 
                     uso_solo + sexo + tipo_pista + ano + regiao,
                   data = train)

predicted <- predict(modelo_treino, type="class", newdata=test)
predicted <- data.frame(predicted)
test$predicted<-predicted

sum(is.na(test$predicted))
sum(is.na(test$estado_fisico))

table(test$estado_fisico)
table(test$predicted)

table(test$estado_fisico,test$predicted)

#----------------------------------------------------------------------------
#Calculation of variable importance for regression and classification models
#A generic method for calculating variable importance for objects produced 
#by train and method specific methods
#----------------------------------------------------------------------------
caret::varImp(modelo_treino)

#----------------------------------------------------------------------------
# colinearidade
#----------------------------------------------------------------------------
car::vif(modelo_treino)


