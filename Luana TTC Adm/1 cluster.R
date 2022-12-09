library(haven)

banco<-read_spss("C:/Users/Hp/Desktop/TCC UNIRIO/Luana TTC Adm/GEM 2016 NES TOTAL NATIONA INDIVIDUAL LEVEL_including Jordan_4.sav")

nomes<-names(banco)
nomes <- data.frame(nomes)

rotulos_perguntas<-lapply(banco, function(x) attributes(x)$label)
rotulos_perguntas <- data.frame(matrix(unlist(rotulos_perguntas), nrow=length(rotulos_perguntas), byrow=T))
dicionario<-data.frame(nomes,rotulos_perguntas)

selecao<- c("NES_COUNTRY", "NES16_A03_9", "NES16_B01_9", "NES16_B02_9", "NES16_B03_9", "NES16_B04_9", "NES16_B05_9", "NES16_B06_9", "NES16_B07_9", "NES16_C01_9", "NES16_C02_9", "NES16_C03_9", "NES16_C04_9", "NES16_C05_9", "NES16_C06_9", "NES16_E01_9", "NES16_E02_9", "NES16_E03_9", "NES16_E04_9", "NES16_E05_9", "NES16_E06_9", "NES16_F01_9", "NES16_F02_9", "NES16_F03_9", "NES16_F04_9", "NES16_F05_9", "NES16_G06_9", "NES16_H01_9", "NES16_H02_9", "NES16_H03_9", "NES16_H04_9", "NES16_H05_9", "NES_GENDER", "NES_AGE", "NES_EDU", "NES_SPE_1", "NES_SPE_3")

banco<-banco[,selecao]
table(banco$NES_COUNTRY)
paises<-data.frame(print_labels(banco$NES_COUNTRY))

# contar o número de valores ausentes
paste0("Número de dados faltantes (Missing Values): ", sum(is.na(banco)))

library(visdat)
vis_dat(banco)
vis_miss(banco)

#selecao<- c("NES16_G06_9","NES_SPE_1", "NES_SPE_3")
selecao<- c("NES16_B01_9", "NES16_B02_9", "NES16_B03_9", "NES16_B04_9", "NES16_B05_9", "NES16_B06_9", "NES16_B07_9", "NES16_C01_9", "NES16_C02_9", "NES16_C03_9", "NES16_C04_9", "NES16_C05_9", "NES16_C06_9", "NES16_E01_9", "NES16_E02_9", "NES16_E03_9", "NES16_E04_9", "NES16_E05_9", "NES16_E06_9", "NES16_F01_9", "NES16_F02_9", "NES16_F03_9", "NES16_F04_9", "NES16_F05_9", "NES16_H01_9", "NES16_H02_9", "NES16_H03_9", "NES16_H04_9", "NES16_H05_9")

banco<-banco[, selecao]
banco<-na.omit(banco)

#----------------------------------------------
# Hierarquico
#----------------------------------------------
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# Dissimilarity matrix
d <- dist(banco, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)

# Cut tree into 6 grupos
seis_grupos <- cutree(as.hclust(hc1), k = 6)
dados_completos<-merge(banco,seis_grupos,by="row.names",all.x=TRUE)

k_means <- kmeans(banco, 6) # k = 6
fviz_cluster(k_means, data = banco)



fviz_nbclust(banco, FUN = hcut, method = "wss")
fviz_nbclust(banco, FUN = hcut, method = "silhouette")

gap_stat <- clusGap(banco, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

library("factoextra")
set.seed(12345)
fviz_nbclust(banco, kmeans, method = "wss")
