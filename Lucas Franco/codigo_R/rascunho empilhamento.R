# https://cran.r-project.org/web/packages/tidystopwords/vignettes/tidystopwords.html

# Carregar a base de dados
library(readtext)
PD_mesquita_2006 <-readtext("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/Mesquita2006.txt",encoding="UTF-8")
PD_São_Gonçalo_2009 <-readtext("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/São_Gonçalo_2009.txt",encoding="UTF-8")

#list.files("G:/.shortcut-targets-by-id/1R24odnjg3hB_Bip5FzB7IbtZBSkq-6CL/TCC_Lucas//Planos Diretores/txt/")

# Jogar fora o excesso
PD_mesquita_2006 <- PD_mesquita_2006$text
PD_São_Gonçalo_2009 <- PD_São_Gonçalo_2009$text
#  Colocar em minusculo
PD_mesquita_2006 <- tolower(PD_mesquita_2006)
PD_São_Gonçalo_2009 <- tolower(PD_São_Gonçalo_2009)

# Substituir o caracter especial
library(stringr)
PD_mesquita_2006<-str_trim(PD_mesquita_2006)
PD_São_Gonçalo_2009<-str_trim(PD_São_Gonçalo_2009)

#PD_mesquita_2006<-str_split(PD_mesquita_2006, boundary("word"))

library(dplyr)
library(quanteda)

# Criacao do objeto corpus
corp_mesquita   <- corpus(PD_mesquita_2006)
corp_SG         <- corpus(PD_São_Gonçalo_2009)

# saber a origem dos dados
docvars(corp_mesquita, "Munic") <- paste("Mesquita", 1:ndoc(corp_mesquita), sep = "")
corp_mesquita$munic <- paste("Mesquita")
table(corp_mesquita$munic)

docvars(corp_SG, "Munic") <- paste("Sao Goncalo", 1:ndoc(corp_SG), sep = "")
corp_SG$munic <- paste("Sao Goncalo")

corp <- rbind(corp_mesquita,corp_SG)
#corp3 <- corp1 + corp2

summary(corp)
summary(corp, n = 5)
head(corp)





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
dfm1 <- dfm(palavras) %>%  dfm_trim(min_termfreq = 3) %>%
  dfm_remove(palavras_banidas) %>%
  dfm_group(groups = c(1,2))

library(quanteda.textplots)
textplot_wordcloud(dfm1, comparison = TRUE, max_words = 300,
                           color = c("blue", "red"))

library(quanteda.textstats)
# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(dfm1, target = 1)

# Plot estimated word keyness
textplot_keyness(result_keyness) 






