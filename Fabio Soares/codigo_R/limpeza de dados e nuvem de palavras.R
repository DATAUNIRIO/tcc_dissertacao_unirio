# Carregar a base de dados
library(readxl)
base_pop <- read_excel("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Fabio Soares/Base Completa - Fabio Soares.xlsx",
                       sheet ="tweets")
View(base_pop)

# Jogar fora o excesso
base_pop <- base_pop$Text

#  Colocar em minusculo
base_pop <- tolower(base_pop)


# Substituir o caracter especial
library(stringr)
banco<-str_trim(base_pop)
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
dfm1 <- dfm(palavras) %>% dfm_remove(palavras_banidas) %>%
  dfm_trim(min_termfreq = 5) 


#----------------------------------------------------------------
#               NUVEM DE PALAVRAS
#----------------------------------------------------------------
library(quanteda.textplots)
nuvem<-textplot_wordcloud(dfm1,min_count = 10)


#----------------------------------------------------------------
#               FREQ DE PALAVRAS
#----------------------------------------------------------------
inaugFeatures <- topfeatures(dfm1, 100)
# Create a data.frame for ggplot
topDf <- data.frame(
  list(
    term = names(inaugFeatures),
    frequency = unname(inaugFeatures)
  )
)

library(ggplot2)
topDf %>% filter(frequency>888) %>% 
  ggplot(aes(x = frequency)) +
  geom_bar(aes(y = reorder(term, frequency)),stat = "identity",color='white',fill='steelblue')+
  labs(x="frequência", y="palavra")


