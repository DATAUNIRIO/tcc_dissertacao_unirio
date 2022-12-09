library(httr)

# http://compras.dados.gov.br/docs/home.html
# https://www.gov.br/compras/pt-br/images/ultimas_noticias/Lista-UASGsa_SISG.pdf

#link<-'http://compras.dados.gov.br/contratos/v1/contratos.csv?offset=500'
#link<-'http://compras.dados.gov.br/contratos/v1/contratos.csv'

#dados<-httr::GET(link,encode = "csv")
#dados2<-content(dados)
#rbind(dados2,dados2)

banco<-c()
for(i in seq(528500,900000,500)){
  link<-paste0('http://compras.dados.gov.br/contratos/v1/contratos.csv?offset=',i)  
  dados<-httr::GET(link,encode = "csv")
  dados2<-content(dados)
  banco<-rbind(banco,dados2)
  cat("\r", i, "of", 600000)
}

# 528500      
# banco1<-banco
# banco1<-rbind(banco1,banco)

save(banco1,file = "C:/Users/Hp/Desktop/TCC UNIRIO/Gabriel/banco_gabriel_parte8.RData")

