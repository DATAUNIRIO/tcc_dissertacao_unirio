library(haven)


banco<-read_spss("C:/Users/Hp/Desktop/TCC UNIRIO/Luana TTC Adm/GEM 2016 NES TOTAL NATIONA INDIVIDUAL LEVEL_including Jordan_4.sav")
banco_paises<-read_spss("C:/Users/Hp/Desktop/TCC UNIRIO/Luana TTC Adm/CORRECTED_GEM 2015 NES NATIONAL LEVEL.sav")

writexl::write_xlsx(banco,path = "GEM2016.xlsx")
writexl::write_xlsx(banco,path = "GEM2015_paises.xlsx")

nomes<-names(banco)
nomes <- data.frame(nomes)

rotulos_perguntas<-lapply(banco, function(x) attributes(x)$label)
rotulos_perguntas <- data.frame(matrix(unlist(rotulos_perguntas), nrow=length(rotulos_perguntas), byrow=T))

dicionario<-data.frame(nomes,rotulos_perguntas)

writexl::write_xlsx(dicionario,path = "dicionario_GEM2016.xlsx")


table(banco$NES_COUNTRY)

paises<-data.frame(print_labels(banco$NES_COUNTRY))

