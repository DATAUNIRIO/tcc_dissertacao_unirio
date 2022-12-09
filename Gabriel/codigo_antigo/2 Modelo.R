library(nnet)
library(Hmisc)
library(MASS)

#SALVA O MODELO
modelo <- multinom(formula = tipo_sancao_5 ~ tipo_orgao+abragencia , data = CEIS)

summary(modelo)

library(sjPlot)
plot_model(modelo)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(CEIS))

## set the seed to make your partition reproducible
set.seed(12345)
train_ind <- sample(seq_len(nrow(CEIS)), size = smp_size)

train <- CEIS[train_ind, ]
test  <- CEIS[-train_ind, ]

remove(train_ind,smp_size)

# PROBLEMA

table(CEIS$tipo_sancao_2)
prop.table(table(CEIS$tipo_sancao_2))*100

library(nnet)
modelo_treino <- multinom(formula = tipo_sancao_2 ~ tipo_orgao+uf_orgao_sancionador+abragencia,data = train)

predicted <- predict(modelo_treino, type="class", newdata=test)
predicted <- data.frame(predicted)
test$predicted<-predicted

sum(is.na(test$predicted))
sum(is.na(test$tipo_sancao_2))

table(test$tipo_sancao_2)
table(test$predicted)

table(test$tipo_sancao_2,test$predicted,useNA = "no")

#----------------------------------------------------------------------------
#Calculation of variable importance for regression and classification models
#A generic method for calculating variable importance for objects produced 
#by train and method specific methods
#----------------------------------------------------------------------------
caret::varImp(modelo)

#----------------------------------------------------------------------------
# colinearidade
#----------------------------------------------------------------------------
car::vif(modelo)

