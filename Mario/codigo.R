
library(readxl)
library(janitor)
BO <- read_excel("C:/Users/Hp/Desktop/TCC UNIRIO/Mario/obitos_infantis_brasil.xlsx") %>% clean_names()
names(BO)
#View(BO)

library(tidyr)
BO_long<-BO %>%
  pivot_longer(-uf, names_to = "ano", values_to = "tx_obitos")

library(dplyr)
BO_long$ano<- gsub("x","",BO_long$ano)

BO_long_BR <- BO_long %>%
  filter(uf=="BRASIL")

serie_BR<-BO_long_BR[,3]
serie_BR <- ts(serie_BR,start=1997)
# https://google.github.io/CausalImpact/CausalImpact.html

#---------------------------------------
# Estrategia 1 - regressao linear
#---------------------------------------

reg<-lm(BO_long_BR$tx_obitos~level)
summary(reg)
# como nao temos a pespectiva de serie temporal, a regressao mostra a associacao linear

#---------------------------------------
# Estrategia 2 - Causal impact
#---------------------------------------
library(CausalImpact)

pre.period <- c(1997, 2010)
post.period <- c(2011, 2019)
impact <- CausalImpact(serie_BR, pre.period, post.period)
plot(impact)

summary(impact)
summary(impact, "report")

library(zoo)
teste <- xts(x = BO_long_BR$tx_obitos, order.by = BO_long_BR$ano)
serie_BR<-as.ts(serie_BR)

library(ggplot2)
ggplot(data = serie_BR)+
  geom_line(color = "#00AFBB", size = 2)

ggplot(BO_long_BR,aes(y = BO_long_BR$tx_obitos)) +
  geom_line(BO_long_BR$tx_obitos)

#---------------------------------------
# Estrategia 3 - analise de intervenção 
#---------------------------------------

parte1<-rep(0,13)
parte2<-rep(1,10)
level<-append(parte1,parte2)
length(level)
library(forecast)
model_1 <- Arima(serie_BR, order = c(4,0,1), 
               seasonal = list(order = c(0,0,0), period=1), 
      xreg = level, include.mean = TRUE)  
summary(model_1) 

library(lmtest)
coeftest(model_1) 
checkresiduals(model_1)  
  
# plot(impact$model$bsts.model, "coefficients")
# Brodersen KH, Gallusser F, Koehler J, Remy N, Scott SL. Inferring causal impact using Bayesian structural time-series models. Annals of Applied Statistics, 2015, Vol. 9, No. 1, 247-274. http://research.google.com/pubs/pub41854.html

library(tseries)
adf.test(serie_BR)

plot(y=BO_long_BR$tx_obitos, x=BO_long_BR$ano, type='l',ylim = c(0,60))

#identify arima process
acf(BO_long_BR$tx_obitos)
pacf(BO_long_BR$tx_obitos)
# AR3 + NA 1

#estimate arima model
mod.1 <- arima(BO_long_BR$tx_obitos, order=c(4,0,1))
mod.1

#diagnose arima model
acf(mod.1$residuals)
pacf(mod.1$residuals)
Box.test(mod.1$residuals)
tsdiag(model_1)


#---------------------------------------
# Estrategia 4 - regressao dinamica
#---------------------------------------

## Examples 7.11/7.12 from Greene (1993)
library(dynlm)
dfm1 <- dynlm(BO_long_BR$tx_obitos ~ level)
plot(y=BO_long_BR$tx_obitos, x=BO_long_BR$ano, type='l',ylim = c(0,60),
lines(fitted(dfm1), col = 2)
)
summary(dfm1)

#---------------------------------------
# Estrategia 5 - Splines
#---------------------------------------
BO_long_BR$ano<-as.integer(BO_long_BR$ano)
# Fitting a GAM in R
library(gam)
library(mgcv)
model <- gam(tx_obitos  ~ s(ano), 
             data = BO_long_BR,           # your data
             #method = 'REML',                # or 'ML'
             family = gaussian)              
summary(model)
plot(model, shade = TRUE, residuals = TRUE, pch = 19)
gam.check(model)

coef(model)


#---------------------------------------
# Estrategia 6 - Controle Sintetico
#---------------------------------------
# devtools::install_github('xuyiqing/panelView')   # if not already installed
library(gsynth)
library(panelView)
BO_long$D<-ifelse(BO_long$ano <2010,0,1)
summary(BO_long)
panelView(tx_obitos ~ D, data = BO_long,  index = c("uf","ano"), pre.post = TRUE) 
panelView(tx_obitos ~ D, data = BO_long,  index = c("uf","ano"), type = "outcome") 

system.time(
  out <- gsynth(tx_obitos ~ D, data = BO_long, index = c("uf","ano"), force = "none", r = 0)
)

BO_long_BR$tx_obitos<-as.numeric(BO_long_BR$tx_obitos)
BO_long$tx_obitos<-as.numeric(BO_long$tx_obitos)

gsub("\\d", "", BO_long$uf)
BO_long$uf2<- gsub("\\D", "", BO_long$uf)
BO_long<-BO_long %>% filter(uf!="BRASIL")

BO_long$uf2<-as.numeric(BO_long$uf2)
class(BO_long$uf2)

dataprep.out<-
  dataprep(
    foo = BO_long,
    predictors.op = "mean",
    dependent = "y",
    unit.variable = "unit.num",
    treatment.identifier = 17,
    controls.identifier = c(33,35),
    time.variable = "ano",
    time.predictors.prior = c(1997:2010),
    time.optimize.ssr = c(2011:2019))

data(synth.data)
head(synth.data)

names(BO_long)
colnames(BO_long)[1] <-"unit.num"
colnames(BO_long)[3] <-"y"
#---------------------------------------
# Estrategia 7 - Microsimulacao
#---------------------------------------
