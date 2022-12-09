

aspas<-'"'
virgula<-","
digito<-"\\d"

library(quanteda)
palavras_banidas<-stopwords(language = "pt")
palavras_banidas<-append(palavras_banidas,aspas)
palavras_banidas<-append(palavras_banidas,virgula)
palavras_banidas<-append(palavras_banidas,digito)
remove(aspas,virgula,digito)

library(tidyr)
texto<-CEIS %>% drop_na(descricao_da_fundamentacao_legal)
corp <- corpus(texto$descricao_da_fundamentacao_legal)

#----------------------------------------------------------------
# assigning document variables to a corpus
docvars(corp, "tipo_sancao_3") <- paste("tipo_sancao_3", 1:ndoc(corp), sep = "")
head(docvars(corp))
corp$tipo_sancao_3 <- paste(texto$tipo_sancao_3)
table(corp$tipo_sancao_3)

docvars(corp, "tipo_orgao") <- paste("tipo_orgao", 1:ndoc(corp), sep = "")
head(docvars(corp))
corp$tipo_orgao <- paste(texto$tipo_orgao)
table(corp$tipo_orgao)


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
dfm1 <- dfm(palavras,remove = palavras_banidas) %>%
  dfm_trim(min_termfreq = 3) 

#----------------------------------------------------------------
#               NUVEM DE PALAVRAS
#----------------------------------------------------------------
# basic wordcloud
nuvem1<-textplot_wordcloud(dfm1)

#----------------------------------------------------------------
# 100 palagvras mais frequentes
#----------------------------------------------------------------

inaugFeatures <- topfeatures(dfm1, 100)
# Create a data.frame for ggplot
topDf <- data.frame(
  list(
    term = names(inaugFeatures),
    frequency = unname(inaugFeatures)
  )
)

#----------------------------------------------------------------
#               NUVEM DE PALAVRAS (AVANCADO)
#----------------------------------------------------------------
# comparison plot
dfm2 <- dfm(corpus_subset(corp, tipo_sancao_2 %in% c("Proibição", "Inidoneidade","Suspensão","Impedimento")),
            remove = c(stopwords(language = "pt"),palavras_banidas), remove_punct = TRUE, groups = "tipo_sancao_2") 

nuvem2<-textplot_wordcloud(dfm2, comparison = TRUE, max_words = 300,
                           #min_size = 1,
                           #max_size = 3,
                           color = c("blue", "red","green","black"))


dfm3 <- dfm(corpus_subset(corp, tipo_orgao %in% c("Estado", "Federal","Municipio","Outros")),
            remove = c(stopwords(language = "pt"),palavras_banidas), remove_punct = TRUE, groups = "tipo_orgao") 

nuvem3<-textplot_wordcloud(dfm3, comparison = TRUE, max_words = 300,
                           min_size = 2,
                           #max_size = 3,
                           color = c("blue", "red","green","black"))


