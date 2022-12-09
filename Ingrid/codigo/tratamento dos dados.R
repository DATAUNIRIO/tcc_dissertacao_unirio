
library(readxl)
dados <- read_excel("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/1. Mapeamento de Estatais/dados.xlsx", 
                    sheet = "texto")

#----------------------------------------------------------------
# Retirar os caracteres especiais
#----------------------------------------------------------------
names(dados)
dados$CONTEXTO<-chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
                                  "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc",dados$CONTEXTO)
head(dados$CONTEXTO)

#----------------------------------------------------------------
# Limpeza do banco de dados
#----------------------------------------------------------------
dados$CONTEXTO<-tolower(dados$CONTEXTO) 	# transforma todas as letras em minúsculo
dados$CONTEXTO<- gsub("\\!","", dados$CONTEXTO)
dados$CONTEXTO<- gsub("\\?","", dados$CONTEXTO)
dados$CONTEXTO<- gsub("\\:","", dados$CONTEXTO)
dados$CONTEXTO<- gsub("\\;","", dados$CONTEXTO)
dados$CONTEXTO<- gsub("\\,","", dados$CONTEXTO)

#----------------------------------------------------------------
#   Corrigindo o plural ???
#----------------------------------------------------------------
dados$CONTEXTO<- gsub("riscos","risco", dados$CONTEXTO)

#----------------------------------------------------------------
# Deletando as linhas sem comentarios (deletando as linhas em branco)
#----------------------------------------------------------------
library(tidyr)
dados<-dados %>% drop_na(CONTEXTO)

#----------------------------------------------------------------
# Trabalhando as variaveis 
#----------------------------------------------------------------
class(dados$ORGÃO)
table(dados$ORGÃO)

library(ggplot2)
ggplot(dados) +
  aes(x = ORGÃO, fill = PALAVRA) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "top",axis.text.x = element_text(angle = 90)) +
  facet_wrap(vars(TIPO)) +
  labs(x = "Empresas", y = "número de palavras", title = "grafico 1 ", subtitle = "meu texto", caption = "fonte dos dados: ingrid")

#----------------------------------------------------------------
#                       SUBSET
#----------------------------------------------------------------

Banco_BNDES <-dados[dados$ORGÃO=="BNDES",]
Banco_Petrobras <-dados[dados$ORGÃO=="Petrobrás",]

Banco_Publico <-dados[dados$TIPO=="Público",]
Banco_Misto <-dados[dados$TIPO=="Econ mista",]

#----------------------------------------------------------------
#                       CORPUS
#----------------------------------------------------------------

library(dplyr)
library(quanteda)
corpus       <- corpus(dados$CONTEXTO)
corpus_Publico <- corpus(Banco_Publico$CONTEXTO)
corpus_Misto <- corpus(Banco_Misto$CONTEXTO)

docvars(corpus, "PALAVRA") <- paste("PALAVRA", 1:ndoc(corpus), sep = "")
docvars(corpus, "TIPO") <- paste("TIPO", 1:ndoc(corpus), sep = "")
head(docvars(corpus))
corpus$PALAVRA <- paste(dados$PALAVRA)
corpus$TIPO <- paste(dados$TIPO)
table(corpus$PALAVRA)
#----------------------------------------------------------------
# PALAVRAS BANIDAS
#----------------------------------------------------------------
palavras_banidas<-stopwords(language = "pt")
palavras_banidas2<-c('sobre','sao','tambem','meio','desde')
palavras_banidas<-append(palavras_banidas,palavras_banidas2)

palavras <- tokens(corpus,"word",
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
dfm1 <- dfm(palavras,remove = palavras_banidas) %>%
  dfm_trim(min_termfreq = 3) 

#----------------------------------------------------------------
#               NUVEM DE PALAVRAS
#----------------------------------------------------------------
# basic wordcloud
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/nuvem1.png",width = 800, height = 600, units = "px")

nuvem1<-textplot_wordcloud(dfm1)
dev.off()

png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/nuvem2.png",width = 800, height = 600, units = "px")
nuvem2<-textplot_wordcloud(dfm1, rotation = 0.25,
                           color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
dev.off()



inaugFeatures <- topfeatures(dfm1, 100)
# Create a data.frame for ggplot
topDf <- data.frame(
  list(
    term = names(inaugFeatures),
    frequency = unname(inaugFeatures)
  )
)

# Sort by reverse frequency order
topDf$term <- with(topDf, reorder(term, -frequency))
colnames(topDf)<-c("palavra","frequencia")
write.csv2(topDf,file = "C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/100palavras.csv")


library(ggplot2)
# png("freq.png",width = 800, height = 600, units = "px")
 ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
   theme(axis.text.x=element_text(angle=90, hjust=1))
# dev.off()

# criando a rede
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/rede25.png",width = 800, height = 600, units = "px")
dfm_trim(dfm1,
         min_termfreq = 25,
         termfreq_type = "rank") %>% 
  textplot_network(edge_size = 0.6,edge_color="grey",
                   vertex_color = c("blue"))+
  theme_minimal()
dev.off()


#Plot word keyness
png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/keyness1.png",width = 800, height = 600, units = "px")
corpus %>%  corpus_subset(PALAVRA %in% c("ética","integridade")) %>%
  dfm(groups = "PALAVRA", remove = palavras_banidas) %>%   
  textstat_keyness(target = "ética") %>%   
  textplot_keyness()
dev.off()

png("C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/keyness2.png",width = 800, height = 600, units = "px")
corpus %>%  corpus_subset(TIPO %in% c("Público","Econ mista")) %>%
  dfm(groups = "TIPO", remove = palavras_banidas) %>%   
  textstat_keyness(target = "Público") %>%   
  textplot_keyness()
dev.off()
#----------------------------------------------------------------
#               BIGRAMA
#----------------------------------------------------------------

library(tidytext)
e<-dados$CONTEXTO
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


prox_palavra<-list(etica,integridade,compliance)
writexl::write_xlsx(prox_palavra,path = "C:/Users/Hp/Desktop/TCC UNIRIO/Ingrid/prox_palavra_Ingrid.xlsx")

