# Carregar a base de dados
library(readtext)
PD_nikiti_2019 <-readtext("C:/Users/Hp/Desktop/TCC UNIRIO/Lucas Franco/Projeto de Pesquisa/Planos Diretores/TXT/niteroi_2019.txt",encoding="UTF-8")


# tratamento dos dados
# Jogar fora o excesso
PD_nikiti_2019 <- PD_nikiti_2019$text

#  Colocar em minusculo
PD_nikiti_2019 <- tolower(PD_nikiti_2019)
# corrigir
PD_nikiti_2019 <- gsub("meio ambiente","meio_ambiente",PD_nikiti_2019)
PD_nikiti_2019<- gsub("públicos","público",PD_nikiti_2019)


# Substituir o caracter especial
library(stringr)
banco<-str_trim(PD_nikiti_2019)
head(banco)
#banco2<-str_split(banco, boundary("word"))
#head(banco2)
library(dplyr)
library(quanteda)

# Cria??o do objeto corpus
corp   <- corpus(banco)

#----------------------------------------------------------------
#                 lista de palavras banidas
#----------------------------------------------------------------

palavras_banidas<-c("\ni","i","ii","iii","iv","v","vi","vii","viii","ix","art","x","xi","xii","xiii","iii-","i-","ii-","iv-","n-","e-","ponto","coordenadas","utm","deste","seguindo","dire??o","linha","reta","metros","exclu?da","partindo",
                    "e","?", "de", "o", "a", "com", "que", "do", "um", "para", "uma", "no", "em", "os", "da", "pelo", "ao", "mas", "nos", "na", "ser", "as", "por","c","d","b","m","faz",
                    "parágrafo","tambem","-",";",":","alem","so","ate","pra",".","'","1,0","2,0","3,0","4,0","5.0","1?","2?","3?","4?","5?","6?","7?","8?","9?","n?","100m")
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
#remotes::install_github("DATAUNIRIO/lemmar")
library(lemmar)
lemmatize_words_pt(topDf$term)

library(ptstem)
ptstem(topDf$term, algorithm = "rslp", complete = TRUE)
ptstem(topDf$term, algorithm = "porter", complete = TRUE)
ptstem(topDf$term, algorithm = "hunspell",, complete = TRUE)
ptstem(topDf$term, algorithm = "modified-hunspell", complete = TRUE)

library(ggplot2)
topDf %>% filter(frequency>100) %>% 
  ggplot(aes(x = frequency)) +
  geom_bar(aes(y = reorder(term, frequency)),stat = "identity",color='white',fill='steelblue')+
  labs(x="frequ?ncia", y="palavra")



