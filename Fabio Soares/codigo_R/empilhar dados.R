


#----------------------------------------------------------------
#                 lista de palavras banidas
#----------------------------------------------------------------

palavras_banidas<-c("bigrama","auvdmtuvs2","r0rp00mbmh","cnaxqgrwjc","informenewsrj","ottrj","ott",
                    "\ni" ,"ii","iii","iv","^v$","vi","vii","viii","ix","art ",
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

aspas<-'"'
virgula<-","
library(quanteda)
palavras_banidas2<-stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,palavras_banidas2)
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
remove(palavras_banidas2,aspas,virgula)


library(forcats)
library(dplyr)
library(tidytext)

library(readxl)
base_pop <- read_excel("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Fabio Soares/Base Completa - Fabio Soares.xlsx",
                       sheet ="tweets")
load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Fabio Soares/Fabio_Soares.RData")
remove(marca_GM,marca_pc,marca_pm,roubando,assaltando,furtando)

# Jogar fora o excesso
base_pop <- base_pop$Text
GMRio <- GMRio$text
pcerj <- pcerj$text
pmerj <- pmerj$text

#  Colocar em minusculo
base_pop <- tolower(base_pop)
GMRio <- tolower(GMRio)
pcerj <- tolower(pcerj)
pmerj <- tolower(pmerj)


base_pop <- tibble(base_pop)
GMRio <- tibble(GMRio)
pcerj <- tibble(pcerj)
pmerj <- tibble(pmerj)

colnames(base_pop)<-"txt"
colnames(GMRio)<-"txt"
colnames(pcerj)<-"txt"
colnames(pmerj)<-"txt"

# IMPORTANTE- tidytext nao funciona com NA (missing)
base_pop <- na.omit(base_pop)
GMRio <- na.omit(GMRio)
pcerj <- na.omit(pcerj)
pmerj <- na.omit(pmerj)

# modificando palavras
base_pop$txt <- gsub("belford roxo","belford_roxo",base_pop$txt)
base_pop$txt <- gsub("padre miguel","padre_miguel",base_pop$txt)
base_pop$txt <- gsub("vila kennedy","vila_kennedy",base_pop$txt)


palavras_banidas_DT<-tibble(palavras_banidas)
colnames(palavras_banidas_DT)<-"word"
remove(palavras_banidas)


base_pop$book<-"População"
GMRio$book<-"Guarda Municipal"
pcerj$book<-"Polícia Civil"
pmerj$book<-"Polícia Militar"
PD<- base_pop %>% add_row(GMRio)
PD<- PD %>% add_row(pcerj)
PD<- PD %>% add_row(pmerj)


PD <- PD %>%  anti_join(palavras_banidas_DT)

book_words <- PD %>%
  unnest_tokens(word, txt) %>%
  count(book, word, sort = TRUE)

book_words <- book_words %>% anti_join(palavras_banidas_DT) 

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

# ordenando os dados
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


tidy_ngram<-base_pop %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

library(tidyr)
bigrama<-tidy_ngram %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, sort = TRUE)
bigrama
View(bigrama)

tidy_ngram_PM<-pmerj %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

bigrama_PM<-tidy_ngram_PM %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, sort = TRUE)
View(bigrama_PM)

#write.csv2(bigrama,file="bigrama.csv")
