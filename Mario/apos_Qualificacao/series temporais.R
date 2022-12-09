# https://otexts.com/fpp2/intro.html
#ggseasonplot(a10) 
#ggseasonplot(a10, polar = T)
#ggsubseriesplot(beer)

library(forecast)
library(fpp2)
library(janitor)
library(readxl)
library(ggfortify)
banco_original <- read_excel("C:/Users/Hp/Desktop/TCC UNIRIO/Mario/apos_Qualificacao/tb1.xlsx", 
                    sheet = "variaveis_sdr") %>% clean_names()

names(banco_original)
summary(banco_original)
banco_relativo<-banco_original[,c(3,5,7,9,10,11,12)]
banco_absoluto<-banco_original[,c(4,6,8,10,11,12)]

colnames(banco_relativo)

banco_relativo<- ts(banco_relativo, start=2000,frequency=1)
banco_absoluto<- ts(banco_absoluto, start=2000,frequency=1)

autoplot(banco_relativo[,2:4])+
  ggtitle("titulo 1") +
  xlab("Ano") +
  ylab("Percentual do PIB")

autoplot(banco_relativo[,2:4],facets=TRUE)

ggAcf(banco_relativo[,2])
ggAcf(banco_relativo[,3])
ggAcf(banco_relativo[,4])

ggPacf(banco_relativo[,2])
ggPacf(banco_relativo[,3])
ggPacf(banco_relativo[,4])

# Set training data from 2000 to 2020
ec29 <- window(banco_relativo[,2],start=2000,end=2020)
ec95 <- window(banco_relativo[,3],start=2000,end=2020)
ec86 <- window(banco_relativo[,4],start=2000,end=2020)
banco_relativo_y <- window(banco_relativo[,2:4],start=2000,end=2020)

# Plot some forecasts
autoplot(ec29) +
  autolayer(meanf(ec29, h=16),
            series="Média", PI=FALSE) +
  autolayer(naive(ec29, h=16),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(ec29, drift=TRUE, h=16),
            series="Drift", PI=FALSE) +
  ggtitle("Previsão média e naive") +
  xlab("Ano") + ylab("Piso - Percentual do PIB EC29")
  guides(colour=guide_legend(title="Previsão"))

autoplot(ec95) +
    autolayer(meanf(ec95, h=16),
              series="Média", PI=FALSE) +
    autolayer(naive(ec95, h=16),
              series="Naïve", PI=FALSE) +
    autolayer(rwf(ec95, drift=TRUE, h=16),
            series="Drift", PI=FALSE) +
    ggtitle("Previsão média e naive") +
    xlab("Ano") + ylab("Teto - Percentual do PIB EC95")
  guides(colour=guide_legend(title="Previsão"))

(lambda <- BoxCox.lambda(ec95))
autoplot(BoxCox(ec95,lambda))    

ec95_v2 <- rwf(ec95, drift=TRUE, lambda=-0.1994862, h=16, level=90,biasadj=TRUE)
ec95_v3 <- rwf(ec95, drift=TRUE, lambda=-0.1994862, h=16, level=90,biasadj=FALSE)
autoplot(ec95) +
  autolayer(ec95_v2, series="Bias adjusted", PI=FALSE)+
  autolayer(ec95_v3, series="adjusted", PI=FALSE)+
  guides(colour=guide_legend(title="Forecast"))

res <- residuals(ec95_v3)
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from method")
gghistogram(res) + ggtitle("Histogram of residuals")

#For the Google stock price example, the naïve model has no parameters, so K=0
# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
Box.test(res,lag=10, fitdf=0, type="Lj")
checkresiduals(naive(ec95))
checkresiduals(naive(ec29))
checkresiduals(naive(ec86))


colnames(banco_relativo_y)
colnames(banco_relativo)

modelo95<-tslm(teto_ec95_perc_pib ~ projecao_pib+percentual_idosos+ipca_saude, data=banco_relativo)
summary(modelo95)
anova(modelo95)

tendencia_ec95 <- holt(ec95, damped=TRUE, h=16)
tendencia_ec29 <- holt(ec29, damped=TRUE, h=16)
tendencia_ec86 <- holt(ec86, damped=TRUE, h=16)

autoplot(tendencia_ec95)
autoplot(tendencia_ec29)
autoplot(tendencia_ec86)

fit95 <- ets(ec95)
fit29 <- ets(ec29)
fit86 <- ets(ec86)
summary(fit95)
summary(fit29)
summary(fit86)

autoplot(fit95)
autoplot(fit29)
autoplot(fit86)

fit95 %>% forecast(h=16) %>% autoplot() +   ylab("EC 95")
fit86 %>% forecast(h=16) %>% autoplot() +   ylab("EC 86")
fit29 %>% forecast(h=16) %>% autoplot() +   ylab("EC 29")

ec95 %>%Box.test(lag=10, type="Ljung-Box")
ec29 %>%Box.test(lag=10, type="Ljung-Box")
ec86 %>%Box.test(lag=10, type="Ljung-Box")

ec95 %>% diff() %>% log() %>% Box.test(type="Ljung-Box")
ec29 %>% diff() %>% Box.test(type="Ljung-Box")
ec86 %>% diff() %>% Box.test(type="Ljung-Box")

library(urca)
ec95 %>% ur.kpss() %>% summary()
ec29 %>% ur.kpss() %>% summary()
ec86 %>% ur.kpss() %>% summary()
#the null hypothesis is that the data are stationary, 
#and we look for evidence that the null hypothesis is false. 
#small p-values (e.g., less than 0.05) suggest that differencing is required. 


arima95 <- auto.arima(ec95, seasonal=FALSE)
arima86 <- auto.arima(ec86, seasonal=FALSE)
arima29 <- auto.arima(ec29, seasonal=FALSE)

arima95 %>% forecast(h=16) %>% autoplot(include=80)

colnames(banco_relativo)


arima_com_regressao<-Arima(banco_relativo[,"teto_ec95_perc_pib"], xreg=banco_relativo[,5:7], order=c(0,2,1))
checkresiduals(arima_com_regressao)

fcast <- forecast(arima_com_regressao, xreg=banco_relativo[21:37,5:7])
autoplot(fcast) + xlab("Year") +
  ylab("Arima com regressao")

checkresiduals(arima_com_regressao)


# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(
  AdLag0 = banco_relativo[,"teto_ec95_perc_pib"],
  AdLag1 = stats::lag(banco_relativo[,"teto_ec95_perc_pib"],-1),
  AdLag2 = stats::lag(banco_relativo[,"teto_ec95_perc_pib"],-2),
  AdLag3 = stats::lag(banco_relativo[,"teto_ec95_perc_pib"],-3)) %>%
  head(NROW(banco_relativo))

# Restrict data so models use same fitting period
fit1 <- auto.arima(banco_relativo[1:17,1], xreg=Advert[1:17,1],
                   stationary=TRUE)
fit2 <- auto.arima(banco_relativo[1:17,1], xreg=Advert[1:17,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(banco_relativo[1:17,1], xreg=Advert[1:17,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(banco_relativo[1:17,1], xreg=Advert[1:17,1:4],
                   stationary=TRUE)
#Next we choose the optimal lag length for advertising based on the AICc.
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

#The best model (with the smallest AICc value) has two lagged predictors; 
