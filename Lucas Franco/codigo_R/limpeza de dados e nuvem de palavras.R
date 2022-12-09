# Carregar a base de dados
library(readtext)
PD_RJ_2011 <-readtext("C:/Users/Hp/Desktop/TCC UNIRIO/Lucas Franco/Planos Diretores/txt/rio_de_janeiro_2011.txt",encoding="UTF-8")

# Jogar fora o excesso
PD_RJ_2011 <- PD_RJ_2011$text

#  Colocar em minusculo
PD_RJ_2011 <- tolower(PD_RJ_2011)
# Substituir o caracter especial

library(stringr)
banco<-str_trim(PD_RJ_2011)
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

palavras_banidas<-c("\ni" ,"ii","iii","iv","^v$","vi","vii","viii","ix","art ",
                    "e", "de", "o", "a", "com", "que", "do", "um", "para", "uma", "no", "em", "os", "da", "pelo", "ao", "mas", "nos", "na", "ser", "as", "por",
                    "tambem","-",";",":","alem","so","ate","pra",".","'",")","(")
aspas<-'"'
virgula<-","
palavras_banidas2<-stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,palavras_banidas2)
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
remove(palavras_banidas2,aspas,virgula)

palavras <- tokens(corp,"word",
                   #remove_numbers = T,
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
#               NUVEM DE PALAVRAS
#----------------------------------------------------------------
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
topDf %>% filter(frequency>100) %>% 
  ggplot(aes(x = frequency)) +
  geom_bar(aes(y = reorder(term, frequency)),stat = "identity",color='white',fill='steelblue')+
  labs(x="frequência", y="palavra")


