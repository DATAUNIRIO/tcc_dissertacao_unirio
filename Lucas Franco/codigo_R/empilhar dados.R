


#----------------------------------------------------------------
#                 lista de palavras banidas
#----------------------------------------------------------------

palavras_banidas<-c("\ni","i","ii","iii","iv","v","vi","vii","viii","ix","art","x","xi","xii","xiii","iii-","i-","ii-","iv-","xiv","n-","e-","ponto","coordenadas","utm","deste","seguindo","dire??o","linha","reta","metros","exclu?da","partindo","complementar","paragrafo","cap?tulo","anexo","conforme","disposto", 
                    "e","?", "de", "o", "a", "com", "que", "do", "um", "para", "uma", "no", "em", "os", "da", "pelo", "ao", "mas", "nos", "na", "ser", "as", "por","c","d","b","m","faz","-as","-0","-a","-os","-s?o",
                    "o","par?grafo","desta","municipal","600m","artigo","tambem","-",";",":","alem","so","ate","pra",".","'","1,0","2,0","3,0","4,0","5.0","1?","2?","3?","4?","5?","6?","7?","8?","9?","n?","100m")
aspas<-'"'
virgula<-","
palavras_banidas2<-stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,palavras_banidas2)
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
remove(palavras_banidas2,aspas,virgula)


library(forcats)
library(dplyr)
#library(janeaustenr)
library(tidytext)

library(readtext)
PD_mesquita_2006 <-readtext("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/Mesquita2006.txt",encoding="UTF-8")
PD_São_Gonçalo_2009 <-readtext("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/São_Gonçalo_2009.txt",encoding="UTF-8")
PD_RJ_2011 <-readtext("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/rio_de_janeiro_2011.txt",encoding="UTF-8")
PD_Meriti_2006 <-readtext("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/São_João_de_Meriti_2006.txt",encoding="UTF-8")

# limpeza
# substituir sao gonca por sao_gonca
PD_São_Gonçalo_2009$text<-gsub("São Gonçalo","São_Gonçalo",PD_São_Gonçalo_2009$text)

# Jogar fora o excesso
PD_mesquita_2006 <- PD_mesquita_2006$text
PD_São_Gonçalo_2009 <- PD_São_Gonçalo_2009$text
PD_RJ_2011<-PD_RJ_2011$text
PD_Meriti_2006<-PD_Meriti_2006$text

#  Colocar em minusculo
PD_mesquita_2006 <- tolower(PD_mesquita_2006)
PD_São_Gonçalo_2009 <- tolower(PD_São_Gonçalo_2009)
PD_RJ_2011<- tolower(PD_RJ_2011)
PD_Meriti_2006<- tolower(PD_Meriti_2006)

PD_mesquita_2006<-tibble(PD_mesquita_2006)
colnames(PD_mesquita_2006)<-"txt"

PD_São_Gonçalo_2009<-tibble(PD_São_Gonçalo_2009)
colnames(PD_São_Gonçalo_2009)<-"txt"

PD_RJ_2011<- tibble(PD_RJ_2011)
colnames(PD_RJ_2011)<-"txt"

PD_Meriti_2006<- tibble(PD_Meriti_2006)
colnames(PD_Meriti_2006)<-"txt"


# IMPORTANTE- tidytext nao funciona com NA (missing)
PD_mesquita_2006 <-na.omit(PD_mesquita_2006)
PD_São_Gonçalo_2009 <-na.omit(PD_São_Gonçalo_2009)
PD_RJ_2011 <-na.omit(PD_RJ_2011)
PD_Meriti_2006 <-na.omit(PD_Meriti_2006)

tidy_ngram<-PD_mesquita_2006 %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

palavras_banidas_DT<-tibble(palavras_banidas)
colnames(palavras_banidas_DT)<-"word"
remove(palavras_banidas)

library(tidyr)
bigrama<-tidy_ngram %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, sort = TRUE)
bigrama

#write.csv2(bigrama,file="bigrama.csv")

PD_mesquita_2006$book<-"Mesquita"
PD_São_Gonçalo_2009$book<-"Sao Goncalo"
PD_Meriti_2006$book<-"São João de Meriti"
PD_RJ_2011$book<-"Rio de Janeiro"
PD<- PD_mesquita_2006 %>% add_row(PD_São_Gonçalo_2009)
PD<- PD %>% add_row(PD_Meriti_2006)
PD<- PD %>% add_row(PD_RJ_2011)

book_words <- PD %>%
  unnest_tokens(word, txt) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(ggplot2)
book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)