source('/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/juntando os bancos de dados CEIS e CONTRATO.R')
# # https://topepo.github.io/caret/model-training-and-tuning.html
# https://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
# https://www.kaggle.com/pritamjena/testing-the-imbalanced-data-using-rose-package

library(ROSE)
banco <- ROSE::ovun.sample(formula = EIS ~ valor+aditivos+modalidade+uasg2, data = BANCO, p=0.5, seed = 1, method = "over")$data

table(BANCO$EIS)
table(banco$EIS)
remove(BANCO)
banco$EIS<-as.factor(banco$EIS)

#----------------------------------------------------------------------------------------
# Treino e Teste
#----------------------------------------------------------------------------------------
set.seed(12345)
dim(banco)

smp_size <- floor(0.8 * nrow(banco))
train_ind <- sample(seq_len(nrow(banco)), size = smp_size)
train <- banco[train_ind, ]
test  <- banco[-train_ind, ]
train<-na.omit(train)
test<-na.omit(test)
remove(train_ind,smp_size,banco)

#----------------------------------------------------------------------------------------
# Modelo 1 - logit
#----------------------------------------------------------------------------------------
modelo1  <- glm(EIS ~ valor+aditivos+modalidade+uasg2 , family = binomial(link = "logit"), data = train)
summary(modelo1)

yhat=predict(modelo1,newdata=test,type = "response")
predicted <- data.frame(yhat)
remove(yhat)
predicted$EIS<-test$EIS

#----------------------------------------------------------------------------
#Calculation of variable importance for regression and classification models
#A generic method for calculating variable importance for objects produced 
#by train and method specific methods
#----------------------------------------------------------------------------
caret::varImp(modelo1)
#----------------------------------------------------------------------------
# colinearidade
#----------------------------------------------------------------------------
car::vif(modelo1)
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
# Modelo 2 - randomForest
#----------------------------------------------------------------------------------------
library(caret)
trctrl <- trainControl(method = "cv")
# we will just set up 5-fold cross validation
trctrl <- trainControl(method = "cv",number=5)
# 10 folds cross validation repeat 3 times
trctrl <- trainControl(method='repeatedcv', 
                       number=10, 
                       repeats=3)
#----------------------------------------------------------------------------
# we will now train random forest model
RF <- train(EIS~.,data = train, 
            method = "ranger", #"ranger" = randomForest)
            #method = 'bayesglm',
            #method = 'adaboost',
            #method='rf', 
            trControl=trctrl,
            maxit = 10)
# The tuning parameter for ranger is mtry; the number of randomly selected predictors at each cut in the tree).
# The argument mtry=13 indicates that all 13 predictors should be considered for each split of the tree. 
#save(RF,file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/RF.Data')
load(file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/RF.Data')

yhat=predict(RF,newdata=test)
RF_predicted <- data.frame(yhat)
RF_predicted$EIS<-test$EIS

# predict the outcome on a test set
#test$predicted<-data.frame(RF_predicted)

# compare predicted outcome and true outcome
table(RF_predicted$yhat,RF_predicted$EIS)
confusionMatrix(RF_predicted$yhat, RF_predicted$EIS)

#----------------------------------------------------------------------------------------
#  Modelo 3 -REDES NEURAIS
# we will now train neural net model
#----------------------------------------------------------------------------------------
library(caret)
rede_neural <- train(EIS~., 
                     data = train, 
                     method = "nnet",
                     trControl=trctrl,
                     # this is maximum number of weights
                     # needed for the nnet method
                     MaxNWts=2000) 

#save(rede_neural,file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_rede_neural.Data')
load(file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_rede_neural.Data')

plot(varImp(rede_neural),top=10)

yhat=predict(rede_neural,newdata=test)
predicted <- data.frame(yhat)
predicted$EIS<-test$EIS

confusionMatrix(predicted$yhat,predicted$EIS)
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
# Modelo 4 - Naive Bayes
#----------------------------------------------------------------------------------------
library(e1071)
library(caret)
modelo4<-naiveBayes(EIS ~ valor+aditivos+modalidade+uasg2 , data = train)
summary(modelo4)
modelo4

yhat4=predict(modelo4,newdata=test)
predicted4 <- data.frame(yhat4)
predicted4$EIS <-test$EIS
confusionMatrix(predicted4$yhat4,predicted4$EIS)

#save(modelo4,file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_naiveBayes.Data')
load(file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_naiveBayes.Data')


#----------------------------------------------------------------------------------------
## Modelo 5 - Stochastic Gradient Boosting 
#----------------------------------------------------------------------------------------

Stochastic_Gradient_Boosting <- train(EIS ~ ., data = train, 
                                      method = "gbm", 
                                      trControl = trctrl,
                                      ## This last option is actually one
                                      ## for gbm() that passes through
                                      verbose = TRUE)

#save(Stochastic_Gradient_Boosting,file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_Stochastic_Gradient_Boosting.Data')
load(file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_Stochastic_Gradient_Boosting.Data')


yhat=predict(Stochastic_Gradient_Boosting,newdata=test)
predicted <- data.frame(yhat)
predicted$EIS<-test$EIS

confusionMatrix(predicted$yhat,predicted$EIS)

#----------------------------------------------------------------------------------------
# Modelo 6 - Decision tree
#----------------------------------------------------------------------------------------
# load libraries
library(rpart)
library(rattle)

rpart <- rpart(EIS ~ ., data=train, method="class")
rpart

#save(rpart,file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_decision_tree.Data')
load(file = '/home/steven/Área de Trabalho/MESTRADO_E_TCC/Gabriel/modelo_decision_tree.Data')

# plot decision tree
fancyRpartPlot(rpart, main="ss")
plot(rpart)
text(rpart,pretty=0)
yhat=predict(rpart,newdata=test)
predicted <- data.frame(yhat)
predicted$EIS<- test$EIS
confusionMatrix(predicted$yhat,predicted$EIS)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# NAO FUNCIONOU
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
# Modelo 3 - SVM 
#----------------------------------------------------------------------------------------
# Fit the model 
svm3 <- train(EIS~.,data = train, method = "svmRadial", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10,
              maxit = 10,verbose = TRUE)

#----------------------------------------------------------------------------------------
# Modelo 5 - KNN
#----------------------------------------------------------------------------------------
library(class)
previsoes <- knn(train = train[,-4, drop = FALSE], test= test[,-4, drop = FALSE],cl= train[,4],k=1)
head(previsoes)

#0. COLETA DE DADOS  = 100% FEITO
#1. CRIAÇÃO DA BASE DE DADOS = 90% FEITO
#2. TRATAMENTO (AGRUPAR AS UASG) = 100% FEITO
#3. ROSE OU SMOTE = 100% FEITO
#4. APRENDIZADO DE MAQUINAS = 100% FEITO
#5. INTERPRETAR RESULTADOS= 0% FEITO
#6. ESCREVER TEXTO DO TCC = 0% FEITO
