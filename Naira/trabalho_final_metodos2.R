
library(readxl)
ADINS_01_12_2022 <- read_excel("C:/Users/Hp/Downloads/ADINS_01_12_2022.xlsx")
head(ADINS_01_12_2022)


#ADINS_01_12_2022 <- read.csv("C:/Users/09115310760/Downloads/ADINS_01_12_2022.csv", encoding="UTF-8", sep=";")
#ADINS_01_12_2022 <- ADINS_01_12_2022 %>% rename(Carmén Lúcia = C<c1>RMEN L<da>CIA)
table(ADINS_01_12_2022$procedente)

ADINS_01_12_2022$procedente2 = ifelse(ADINS_01_12_2022$procedente==FALSE,"Improcedente","Procedente")

pie(table(ADINS_01_12_2022$procedente),col=c("red","blue"))

contagem = table(ADINS_01_12_2022$procedente2)
nomes = c("Improcedente","Procedente")
porcent = round(contagem/sum(contagem)*100,2)
rotulo = paste(nomes,"\n",porcent,"%",sep="")

pie(table(ADINS_01_12_2022$procedente2),col=c("red","blue"),labels=rotulo)

library(stringr)
library(janitor)
ADINS_01_12_2022 = ADINS_01_12_2022 %>% clean_names()

temporario = str_split(ADINS_01_12_2022$data_de_publicacao, "/" , simplify = TRUE)
ADINS_01_12_2022$ano_publicacao = temporario[,3]


names(ADINS_01_12_2022)

table(ADINS_01_12_2022$ano_publicacao)
barplot(table(ADINS_01_12_2022$ano_publicacao),col="red")


ADINS_01_12_2022$gestao = 
  ifelse(ADINS_01_12_2022$ano_publicacao<=2018,"Temer",
  ifelse(ADINS_01_12_2022$ano_publicacao<=2015,"Dilma",
  ifelse(ADINS_01_12_2022$ano_publicacao<=2010,"Lula 2",
  ifelse(ADINS_01_12_2022$ano_publicacao<=2006,"Lula 1",         
  ifelse(ADINS_01_12_2022$ano_publicacao<=2002,"FHC 2",
  ifelse(ADINS_01_12_2022$ano_publicacao<=1998,"FHC 1",
  ifelse(ADINS_01_12_2022$ano_publicacao<=1995,"Itamar",
  ifelse(ADINS_01_12_2022$ano_publicacao<=1992,"Collor",
  ifelse(ADINS_01_12_2022$ano_publicacao<1990,"Sarney",'jb'))))))))) 

ADINS_01_12_2022$gestao = ifelse(ADINS_01_12_2022$ano_publicacao<1990,"1.Sarney",ifelse(ADINS_01_12_2022$ano_publicacao<=1992,"2.Collor",ifelse(ADINS_01_12_2022$ano_publicacao<=1995,"3.Itamar",ifelse(ADINS_01_12_2022$ano_publicacao<=1998,"4.FHC 1",ifelse(ADINS_01_12_2022$ano_publicacao<=2002,"5.FHC 2",ifelse(ADINS_01_12_2022$ano_publicacao<=2006,"6.Lula 1",ifelse(ADINS_01_12_2022$ano_publicacao<=2010,"7.Lula 2",ifelse(ADINS_01_12_2022$ano_publicacao<=2015,"8.Dilma",ifelse(ADINS_01_12_2022$ano_publicacao<=2018,"9.Temer",'91.JB'))))))))) 
table(ADINS_01_12_2022$gestao)

barplot(table(ADINS_01_12_2022$gestao),col="blue",ylim = c(0,20000))


library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)

# colocar tudo em caixa baixa
ADINS_01_12_2022$ementa = tolower(ADINS_01_12_2022$ementa)

# função para retirar os caracteres especiais
ADINS_01_12_2022$ementa = chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
                          "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc",ADINS_01_12_2022$ementa)


banco1<-str_split(ADINS_01_12_2022$ementa, boundary("word"))
banco2<-ADINS_01_12_2022 %>% filter(gestao=='2.Collor') 
banco2<- str_split(banco2$ementa, boundary("word"))

banco3<-ADINS_01_12_2022 %>% filter(gestao=='91.JB') 
banco3<- str_split(banco3$ementa, boundary("word"))

auxCorpus <- Corpus(VectorSource(banco1))
auxCorpus <- tm_map(auxCorpus, PlainTextDocument)

auxCorpus2 <- Corpus(VectorSource(banco2))
auxCorpus2 <- tm_map(auxCorpus2, PlainTextDocument)

auxCorpus3 <- Corpus(VectorSource(banco3))
auxCorpus3 <- tm_map(auxCorpus3, PlainTextDocument)


#Então, vamos remover toda a pontuação e palavras irrelevantes. Stopwords são comumente usadas no português, como:  eu, meu e etc.

auxCorpus <- tm_map(auxCorpus, removePunctuation) 
#auxCorpus <- tm_map(auxCorpus, removeWords, stopwords('pt')) 
auxCorpus <- iconv(tm_map(auxCorpus, removeWords, stopwords('pt')), "latin1", "latin2", "") 

auxCorpus2 <- tm_map(auxCorpus2, removePunctuation) 
auxCorpus2 <- iconv(tm_map(auxCorpus2, removeWords, stopwords('pt')), "latin1", "latin2", "") 

auxCorpus3 <- tm_map(auxCorpus3, removePunctuation) 
auxCorpus3 <- iconv(tm_map(auxCorpus3, removeWords, stopwords('pt')), "latin1", "latin2", "") 

brewer.pal(9,"Blues")
brewer.pal(9,"Reds")
brewer.pal(9,"Purples")
wordcloud(auxCorpus,max.words=100,colors=brewer.pal(9,"Blues"))
wordcloud(auxCorpus,max.words=100,colors=brewer.pal(9,"Reds"))
wordcloud(auxCorpus,max.words=100,colors=brewer.pal(9,"Purpless"))                          
                          
                          
                          
                                 



