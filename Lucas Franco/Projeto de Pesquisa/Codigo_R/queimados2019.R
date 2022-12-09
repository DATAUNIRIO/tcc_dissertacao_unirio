# Carregar a base de dados
library(readtext)
PD_queimados_2019 <-readtext("C:/Users/Luc/Desktop/Projeto e TCC/tcc/Planos Diretores/TXT/queimados_2019.txt",encoding="UTF-8")

# Jogar fora o excesso
PD_queimados_2019 <- PD_queimados_2019$text

#  Colocar em minusculo
PD_queimados_2019 <- tolower(PD_queimados_2019)

# Substituir o caracter especial
library(stringr)
banco<-str_trim(PD_queimados_2019)
head(banco)
#banco2<-str_split(banco, boundary("word"))
#head(banco2)
library(dplyr)
library(quanteda)

# Criação do objeto corpus
corp   <- corpus(banco)

#----------------------------------------------------------------
#                 lista de palavras banidas
#----------------------------------------------------------------

palavras_banidas<-c("\ni","i","ii","iii","iv","v","vi","vii","viii","ix","art","x","xi","xii","xiii","iii-","i-","ii-","iv-","xiv","n-","e-","ponto","coordenadas","utm","deste","seguindo","direção","linha","reta","metros","excluída","partindo","complementar","paragrafo","capítulo","anexo","conforme","disposto", 
                    "e","é", "de", "o", "a", "com", "que", "do", "um", "para", "uma", "no", "em", "os", "da", "pelo", "ao", "mas", "nos", "na", "ser", "as", "por","c","d","b","m","faz","-as","-0","-a","-os","-são",
                    "tambem","-",";",":","alem","so","ate","pra",".","'","1,0","2,0","3,0","4,0","5.0","1º","2º","3º","4º","5º","6º","7º","8º","9º","nº","100m")
aspas<-'"'
virgula<-","
palavras_banidas2<-stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,palavras_banidas2)
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
remove(palavras_banidas2,aspas,virgula)

palavras <- tokens(corp,"word",
                   remove_numbers = T,
                   remove_symbols = T,
                   remove_punct = T,
                   remove_separators = T,
                   remove_hyphens = F) %>% 
  tokens_remove(pattern = palavras_banidas)

#----------------------------------------------------------------
#                       DFM
#----------------------------------------------------------------

# criando o document-feature matrix
dfm1 <- dfm(palavras,remove = palavras_banidas) %>%
  dfm_trim(min_termfreq = 3) 


#----------------------------------------------------------------
#----------------------------------------------------------------
#               NUVEM DE PALAVRAS
library(quanteda.textplots)
nuvem<-textplot_wordcloud(dfm1)

inaugFeatures <- topfeatures(dfm1, 100)
# Create a data.frame for ggplot
topDf <- data.frame(
  list(
    term = names(inaugFeatures),
    frequency = unname(inaugFeatures)
  )
)

library(ggplot2)
topDf %>% filter(frequency>30) %>% 
  ggplot(aes(x = frequency)) +
  geom_bar(aes(y = reorder(term, frequency)),stat = "identity",color='white',fill='steelblue')+
  labs(x="frequência", y="palavra")
