library(readxl)
banco <- read_excel("01_02_2020/GEM2016.xlsx")
nome_paises <- read_excel("01_02_2020/nome_paises.xlsx")

#Comandos no Rstudio para Cluster Não Hierárquico
banco2 <- banco[, c(2,6,12:24,37:41,47:52)] #criação de um banco de dados somente com as perguntas selecionadas
banco2 <- na.omit(banco2) #exclusão dos dados faltantes
PAISES <- data.frame(banco2$NES_COUNTRY)
banco2 <- banco2[, c(2:26)]
colnames(nome_paises)[1]<-"banco2.NES_COUNTRY"

library(dplyr)
PAISES <- PAISES %>% left_join(nome_paises)

#pacotes: #carregar pacotes necessários
library(FactoMineR)
library(factoextra)
library(cluster)
k_means<- kmeans(banco2, 3) #criar um conjunto com o kmeans igual a 3


fviz_cluster(k_means, data=banco2) #gerar o gráfico com os agrupamentos gerados
fviz_nbclust(banco2, kmeans, method ="gap_stat") #gerar número ótimo de clusters com “gap_stat”
fviz_nbclust(banco2, kmeans, method = "silhouette") #gerar número ótimo de clusters com o método silhueta.
fviz_nbclust(banco2, kmeans, method = "wss") #gerar o número ótimo de clusters com método do cotovelo.

k_means<- kmeans(banco2, 2) #criar um conjunto com o kmeans igual a 3
grupos<-k_means$cluster
grupos<-data.frame(grupos)
banco3<-merge(banco2,grupos$grupos,by="row.names",all.x=TRUE) 
colnames(banco3)[1]<-"ID"
PAISES$ID<-row.names(PAISES)

banco4<-merge(banco3,PAISES,by="ID",all.x=TRUE) 
table(banco4$NES16_A03_9,banco4$y)
# grupo 1 = sem subsidios do governo
# grupo 2 = com subsidios do governo

table(banco4$NES16_B02_9,banco4$y)
#grupo 1 =  sem apoio a novas empresas
#grupo 2 =  com apoio a novas empresas

table(banco4$`nome paises portugues`,banco4$y)
prop.table(table(banco4$`nome paises portugues`,banco4$y),1)*100


#______________________________________________________________
#Cluster Hierárquico
#______________________________________________________________
rownames(banco2)<-PAISES$label
d1 <- dist(banco2, method = "euclidean") #calculo a distância euclidiana

hc <- hclust(d1, method = "complete" ) #crio os clusters
plot(hc, cex = 0.5) #gero o dendograma
dendrograma<-as.dendrogram(hc)

# Cut tree into 2 grupos
dois_grupos <- cutree(as.hclust(hc), k = 2)
dados_completos<-merge(banco2,dois_grupos,by="row.names",all.x=TRUE)

table(dados_completos$NES16_A03_9,dados_completos$y)
# grupo 1 = sem subsidios do governo
# grupo 2 = com subsidios do governo

table(dados_completos$NES16_B02_9,dados_completos$y)
#grupo 1 =  sem apoio a novas empresas
#grupo 2 =  com apoio a novas empresas
colnames(dados_completos)[1]<-"ID"
dados_completos<-merge(dados_completos,PAISES,by="ID",all.x=TRUE) 


table(dados_completos$`nome paises portugues`,dados_completos$y)
prop.table(table(dados_completos$`nome paises portugues`,dados_completos$y),1)*100

dados_completos_hierarquico<-dados_completos
dados_completos_nao_hierarquico<-banco4
save(dados_completos_hierarquico,dados_completos_nao_hierarquico,PAISES, file="dados_completos.RDATA")
