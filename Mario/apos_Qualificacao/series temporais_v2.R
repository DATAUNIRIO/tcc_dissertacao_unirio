# https://otexts.com/fpp2/intro.html
# https://www.maxwell.vrac.puc-rio.br/13919/13919_5.PDF


# Carrega a base de dados
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
names(banco_relativo)
#banco_absoluto<-banco_original[,c(4,6,8,10,11,12)]

banco_relativo2021_grafico<- ts(banco_relativo[1:22,2:4], start=2000,frequency=1)
autoplot(banco_relativo2021_grafico)+
  ggtitle("Gráfico 1 - Despesas em ASPS como percentual do PIB") +
  xlab("Ano") +
  ylab("Percentual do PIB")

autoplot(banco_relativo2021_grafico,facets=TRUE)

#----------------------------------------------------------------------------
# Montar a base de dados no formato "serie temporal"
banco_relativo_2000_2021<- ts(banco_relativo[1:22,], start=2000,frequency=1)
preditores_lineares_2022_2036 <- ts(banco_relativo[23:37,], start=2022,frequency=1)
#----------------------------------------------------------------------------

# FAC e FACp
# para que serve? para decidir quantos parametros vamos usar
# ajuda a decidir se vamos estimar 1, 2 ou 3 parametros

# EC 29
ggAcf(banco_relativo_2000_2021[,2]) # MA 0
ggPacf(banco_relativo_2000_2021[,2]) # AR 1

# EC 95
ggAcf(banco_relativo_2000_2021[,3]) #  MA 0
ggPacf(banco_relativo_2000_2021[,3]) # AR 1

#AR
#MA
#ARMA não vamos usar
#ARIMA não vamos usar
#SARIMA não vamos usar


# EC86
ggAcf(banco_relativo_2000_2021[,4]) #  MA 1, MA 2
ggPacf(banco_relativo_2000_2021[,4]) # AR 0 

# Para EC29 vamos usar um AR(1)
# Para EC95 vamos usar um AR(1)
# Para EC86 vamos usar um MA(1), MA(2)

#----------------------------------------------------------------------------
# Avaliando se é estacionário
#----------------------------------------------------------------------------
library(urca)
summary(ur.kpss(banco_relativo_2000_2021[,2]))
summary(ur.kpss(banco_relativo_2000_2021[,3]))
summary(ur.kpss(banco_relativo_2000_2021[,4]))

# H0: É ESTACIONARIO
# H1: NÃO É ESTACIONARIo
# ALPHA = 0,05
# P-VALOR = 0,463
# P-VALOR > ALPHA NÃO REJ H0
  
# EC 29 É ESTACIONARIA
# EC 95 É ESTACIONARIA
# EC 86 É ESTACIONARIA
#the null hypothesis is that the data are stationary, 
#and we look for evidence that the null hypothesis is false. 
#small p-values (e.g., less than 0.05) suggest that differencing is required. 

#-------------------------------------------------------------------------------------
# AR com regressao
modelo95<-Arima(banco_relativo_2000_2021[,"teto_ec95_perc_pib"], # EC 95 
                xreg=banco_relativo_2000_2021[,5:7],            # parte regressao
                order=c(1,0,0))                                 # parte serie temporal      

res <- residuals(modelo95)
shapiro.test(res)
# residuos com distribuição normal
# modelo é valido
checkresiduals(modelo95) # mudar para AR(2)
modelo95<-Arima(banco_relativo_2000_2021[,"teto_ec95_perc_pib"], # EC 95 
                xreg=banco_relativo_2000_2021[,5:7],            # parte regressao
                order=c(2,0,0))                                 # parte serie temporal      
checkresiduals(modelo95) # mudar para AR(2)

# é normal e estacionário...... e vamos usar AR(2)

summary(modelo95)

previsao95<-forecast(modelo95,h=15,xreg=preditores_lineares_2022_2036[,5:7])
plot(previsao95, main="EC 95")
previsao95$mean[15]



#----------------------------------------------------------------------------
modelo29<-Arima(banco_relativo_2000_2021[,"piso_ec29_perc_pib"], 
                xreg=banco_relativo_2000_2021[,5:7], 
                order=c(1,0,0))
res <- residuals(modelo29)
shapiro.test(res)
#  H0: OS dados são normais
#  H1: os dados não sao normais
# alpha = 0,05
#SE pvalor < alpha rej H0
#SE pvalor > alpha Nâo rej H0
#p-value > alpha Nao rej h0
checkresiduals(modelo29)

summary(modelo29)
previsao29<-forecast(modelo29,h=15,
                     xreg=preditores_lineares_2022_2036[,5:7])
plot(previsao29, main="EC 29")
previsao29$mean[15]
previsao95$mean[15]

700000000*1.851868
700000000*0.413693

par(mfrow=c(2,1))
plot(previsao29)
plot(previsao95)
par(mfrow=c(1,1))


#----------------------------------------------------------------------------
modelo86m1<-Arima(banco_relativo_2000_2021[,"piso_ec86_perc_pib"], 
                xreg=banco_relativo_2000_2021[,5:7], 
                order=c(0,0,1))

modelo86m2<-Arima(banco_relativo_2000_2021[,"piso_ec86_perc_pib"], 
                  xreg=banco_relativo_2000_2021[,5:7], 
                  order=c(0,0,2))

resm1 <- residuals(modelo86m1)
shapiro.test(resm1)
resm2 <- residuals(modelo86m2)
shapiro.test(resm2)
# DEcidimos pelo modelo M2
checkresiduals(modelo86m2)

summary(modelo86m2)

previsao86<-forecast(modelo86m2,h=15,xreg=preditores_lineares_2022_2036[,5:7])
plot(previsao86, main="EC 86")

par(mfrow=c(3,1))
plot(previsao29)
plot(previsao86)
plot(previsao95)

previsao29$mean[15]
previsao86$mean[15]
previsao95$mean[15]

dados_previsao<-data.frame(previsao29,previsao86,previsao95)
# A continuidade da EC 95 significa reduzir 
# as despesas em ASPS em 4,5 vezes
# comparação a EC 29...




dados_previsao_grafico<- ts(dados_previsao, start=2022,frequency=1)
autoplot(dados_previsao_grafico)

resumo<-dados_previsao[,c("Point.Forecast","Point.Forecast.1","Point.Forecast.2")]
colnames(resumo)<-c("EC_29","EC 86","EC 95")

resumo_grafico<- ts(resumo, start=2022,frequency=1)
autoplot(resumo_grafico)


names(banco_relativo)
autoplot(banco_relativo_2000_2021[,"despesa_empenhada_asps_perc_pib"]) +
  autolayer(previsao29) +
  autolayer(previsao86) +
  autolayer(previsao95) 


autoplot(previsao29)+
  autolayer(previsao86) +
  autolayer(previsao95) 

autoplot(previsao95, colour = 'red', predict.colour = 'red')+
  autolayer(previsao86, predict.colour = 'green') +
  autolayer(previsao29) 

dados_previsao$Time<-rownames(dados_previsao)
names(dados_previsao)

colnames(dados_previsao)<-c("EC29","Low80EC29",           
 "High80EC29","Low95EC29", "High95EC29","EC86",
 "Low80EC86","High80EC86", "Low95EC86","High95EC86","EC95", 
 "Low80EC95", "High80EC95","Low95EC95","High95EC95","Time" )

dados_previsao

ggplot(dados_previsao, aes(x = Time)) + 
  geom_ribbon(aes(ymin = Low95EC29, ymax = High95EC29, fill = "95EC29",group = 1, alpha=0.5)) +
  geom_ribbon(aes(ymin = Low95EC86, ymax = High95EC86, fill = "95EC86",group = 2, alpha=0.5)) +
  geom_ribbon(aes(ymin = Low95EC95, ymax = High95EC95, fill = "95EC95",group = 3, alpha=0.5)) +
  geom_line(aes(y = EC29, group = 1, colour = "EC29"), size = 0.75) +
  geom_line(aes(y = EC86, group = 2, colour = "EC86"), size = 0.75) +
  geom_line(aes(y = EC95, group = 3, colour = "EC95"), size = 0.75) +
  scale_colour_brewer(name = "Legenda", type = "qual") +
  scale_fill_brewer(name = "EC", type = "qual") +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  theme_bw(base_size = 14)+
  labs(title="previsão",x="Tempo",y="Previsão",caption = "Fonte Mário")


summary(modelo95)
summary(modelo29)
summary(modelo86m2)


writexl::write_xlsx(dados_previsao,path = "C:/Users/Hp/Desktop/TCC UNIRIO/Mario/apos_Qualificacao/resuktados/previsoes_realizadas.xlsx")
save(dados_previsao,file = "C:/Users/Hp/Desktop/TCC UNIRIO/Mario/apos_Qualificacao/resuktados/previsoes_realizadas.RData")
