

load(file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/RF.Data')

yhat=predict(RF,newdata=test)
RF_predicted <- data.frame(yhat)
RF_predicted$EIS<-test$EIS
library(caret)
confusionMatrix(RF_predicted$yhat, RF_predicted$EIS)

# o melhor modelo é floresta aleatoria!

predict(RF,newdata=data.frame(modalidade="7: INEXIGIBILIDADE DE LICITAÇÃO",
                     aditivos=7,
                     valor=3000000,
                     uasg2="MILITAR"))

predict(RF,newdata=data.frame(modalidade="2: TOMADA DE PREÇOS",
                              aditivos=1,
                              valor=30000,
                              uasg2="EDUCACAO"))