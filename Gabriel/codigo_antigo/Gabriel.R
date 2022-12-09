
library(httr)

# http://compras.dados.gov.br/docs/home.html
# https://www.gov.br/compras/pt-br/images/ultimas_noticias/Lista-UASGsa_SISG.pdf

link3<-'http://compras.dados.gov.br/contratos/v1/contratos.json?uasg=20001&offset=500'
link3<-'http://compras.dados.gov.br/contratos/v1/contratos.json?offset=500'
dados3<-httr::GET(link3,encode = "json")
dados4<-content(dados3)
dados5<-dados4[["_embedded"]][["contratos"]]


library(jsonlite)
fromJSON(dados2)
fromJSON(temp) %>% as.data.frame