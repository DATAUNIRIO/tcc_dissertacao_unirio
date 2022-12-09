corpus_Publico
corpus_Misto 
palavras_publico <- tokens(corpus_Publico,"word",
                   #remove_numbers = T,
                   remove_symbols = T,
                   remove_punct = T,
                   remove_separators = T,
                   remove_hyphens = F) %>% 
  tokens_remove(pattern = palavras_banidas)

palavras_Misto <- tokens(corpus_Misto,"word",
                           #remove_numbers = T,
                           remove_symbols = T,
                           remove_punct = T,
                           remove_separators = T,
                           remove_hyphens = F) %>% 
  tokens_remove(pattern = palavras_banidas)

#----------------------------------------------------------------

#----------------------------------------------------------------
#                       DFM
#----------------------------------------------------------------

# criando o document-feature matrix
dfm_publico <- dfm(palavras_publico,remove = palavras_banidas) %>%
  dfm_trim(min_termfreq = 3) 

dfm_misto <- dfm(palavras_Misto,remove = palavras_banidas) %>%
  dfm_trim(min_termfreq = 3) 

#----------------------------------------------------------------
#               NUVEM DE PALAVRAS
#----------------------------------------------------------------
# basic wordcloud
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/nuvem_publico.png",width = 800, height = 600, units = "px")
nuvem_publico<-textplot_wordcloud(dfm_publico)
dev.off()
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/nuvem_misto.png",width = 800, height = 600, units = "px")
nuvem_misto<-textplot_wordcloud(dfm_misto,  color = "red")
dev.off()


publico <- topfeatures(dfm_publico, 100)
# Create a data.frame for ggplot
topDf_publico <- data.frame(
  list(
    term = names(publico),
    frequency = unname(publico)))

misto <- topfeatures(dfm_misto, 100)
# Create a data.frame for ggplot
topDf_misto <- data.frame(
  list(
    term = names(misto),
    frequency = unname(misto)))

# Sort by reverse frequency order
topDf_misto$term <- with(topDf_misto, reorder(term, -frequency))
colnames(topDf_misto)<-c("palavra","frequencia")
write.csv2(topDf_misto,file = "C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/100palavrasmisto.csv")
write.csv2(topDf_publico,file = "C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/100palavraspublico.csv")

# criando a rede
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/rede25_publico.png",width = 800, height = 600, units = "px")
dfm_trim(dfm_publico,
         min_termfreq = 25,
         termfreq_type = "rank") %>% 
  textplot_network(edge_size = 0.6,edge_color="grey",
                   vertex_color = c("blue"))+
  theme_minimal()
dev.off()
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/rede25_misto.png",width = 800, height = 600, units = "px")
dfm_trim(dfm_misto,
         min_termfreq = 25,
         termfreq_type = "rank") %>% 
  textplot_network(edge_size = 0.6,edge_color="grey",
                   vertex_color = c("red"))+
  theme_minimal()
dev.off()


corpus_Publico
corpus_Misto 
#Plot word keyness
docvars(corpus_Publico, "PALAVRA") <- paste("PALAVRA", 1:ndoc(corpus_Publico), sep = "")
docvars(corpus_Misto, "PALAVRA") <- paste("PALAVRA", 1:ndoc(corpus_Misto), sep = "")

# png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/keyness_publico.png",width = 800, height = 600, units = "px")
# corpus_Publico %>%  corpus_subset(PALAVRA %in% c("ética","integridade")) %>%
#   dfm(groups = "PALAVRA", remove = palavras_banidas) %>%   
#   textstat_keyness(target = "ética") %>%   
#   textplot_keyness(color = c("blue", "yellow"))
# dev.off()
# 
# png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/keyness2.png",width = 800, height = 600, units = "px")
# corpus %>%  corpus_subset(TIPO %in% c("Público","Econ mista")) %>%
#   dfm(groups = "TIPO", remove = palavras_banidas) %>%   
#   textstat_keyness(target = "Público") %>%   
#   textplot_keyness()
# dev.off()
#----------------------------------------------------------------
#               BIGRAMA
#----------------------------------------------------------------
dados_publico<-dados[dados$TIPO=="Público",]
dados_misto<-  dados[dados$TIPO=="Econ mista",]

library(tidytext)
e<-dados_publico$CONTEXTO
e<-tibble(e)
colnames(e)<-"txt"
# IMPORTANTE- tidytext nao funciona com NA (missing)
e <-na.omit(e)
e %>% unnest_sentences(word, txt)

frases<- e %>%
  unnest_tokens(sentence, txt, token = "sentences")
head(frases)

tidy_ngram<-e %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

palavras_banidas_DT<-tibble(palavras_banidas)
colnames(palavras_banidas_DT)<-"word"

library(tidyr)
bigrama<-tidy_ngram %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, sort = TRUE)
bigrama

# TRIGRAMA
tidy_ngram2<-e %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 3)
trigrama<-tidy_ngram2 %>%
  separate(ngram, c("word1", "word2","word3" ), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word,
         !word3 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, word3, sort = TRUE)
trigrama

#-----------------------------------------------------------------------
#  PROXIMA PALAVRA (nextword)
#-----------------------------------------------------------------------

etica<-bigrama %>%   filter(grepl("^etica", word1)) %>% arrange(desc(n))%>% top_n(10)
integridade<-bigrama %>%   filter(grepl("^integri", word1)) %>% arrange(desc(n))%>% top_n(10)
compliance<-bigrama %>%   filter(grepl("^complianc", word1)) %>% arrange(desc(n))%>% top_n(10)


prox_palavra_publico<-list(e,bigrama,etica,integridade,compliance)
writexl::write_xlsx(prox_palavra_publico,path = "C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/prox_palavra_publico_Ingrid.xlsx")

#-----------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------


library(tidytext)
e<-dados_misto$CONTEXTO
e<-tibble(e)
colnames(e)<-"txt"
# IMPORTANTE- tidytext nao funciona com NA (missing)
e <-na.omit(e)
e %>% unnest_sentences(word, txt)

frases<- e %>%
  unnest_tokens(sentence, txt, token = "sentences")
head(frases)

tidy_ngram<-e %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

palavras_banidas_DT<-tibble(palavras_banidas)
colnames(palavras_banidas_DT)<-"word"

library(tidyr)
bigrama<-tidy_ngram %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, sort = TRUE)
bigrama

# TRIGRAMA
tidy_ngram2<-e %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 3)
trigrama<-tidy_ngram2 %>%
  separate(ngram, c("word1", "word2","word3" ), sep = " ") %>%
  filter(!word1 %in% palavras_banidas_DT$word,
         !word2 %in% palavras_banidas_DT$word,
         !word3 %in% palavras_banidas_DT$word) %>%
  count(word1, word2, word3, sort = TRUE)
trigrama

#-----------------------------------------------------------------------
#  PROXIMA PALAVRA (nextword)
#-----------------------------------------------------------------------

etica<-bigrama %>%   filter(grepl("^etica", word1)) %>% arrange(desc(n))%>% top_n(10)
integridade<-bigrama %>%   filter(grepl("^integri", word1)) %>% arrange(desc(n))%>% top_n(10)
compliance<-bigrama %>%   filter(grepl("^complianc", word1)) %>% arrange(desc(n))%>% top_n(10)


prox_palavra_misto<-list(bigrama,etica,integridade,compliance)
writexl::write_xlsx(prox_palavra_misto,path = "C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/prox_palavra_misto_Ingrid.xlsx")

