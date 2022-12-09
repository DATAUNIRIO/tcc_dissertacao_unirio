library(readxl)
library(janitor)
# Importar das bases de dados
Banco_lavouras <- read_excel("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Matheus_Abreu/Banco_de_dados.xlsx", 
                             sheet = "BASE TEmporaria", skip = 5) %>% clean_names()

Banco_Familiar <- read_excel("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Matheus_Abreu/Banco_de_dados.xlsx", 
                             skip = 5)%>% clean_names()

names(Banco_Familiar)

library(tidyr)
Banco_Familiar_wide <- Banco_Familiar %>%
  pivot_wider(names_from = grupos_de_atividade_economica, 
              values_from = c(sim_agricultura_familiar,     
                              nao_agricultura_familiar,
                              proporcao_familiar,
                              x6)) 
remove(Banco_Familiar)
 
#-----------------------------------------------------------------------------------
# install.packages("devtools")
#devtools::install_github("rpradosiqueira/brazilmaps")

#----------------------------------------------------------------------

#MAX output / input

#Indicador1 <-  Area Colhida / Área Plantada
#Indicador2 <-  Valor da produção /Area Colhida

#Indicador1 versus familiar/não familiar
#Indicador1 versus Rio Rural

#Indicador2 versus familiar/não familiar
#Indicador2 versus Rio Rural

#Indicador3 = Taxa de Crescimento

#O impacto da agricultura familiar na produtividade
#O impacto do Rio Rural na  produtividade

#----------------------------------------------------------------------

Ind_1_2016 <- Banco_lavouras$area_colhida_hectares_ano_2016_temporarias/(Banco_lavouras$area_plantada_hectares_ano_2016_temporarias)
Ind_2_2016 <- Banco_lavouras$valor_da_producao_mil_reais_ano_2016_temporarias/Banco_lavouras$area_colhida_hectares_ano_2016_temporarias            
#Ind_3_2016 <- (Banco_lavouras$area_colhida_hectares_ano_2019_temporarias - Banco_lavouras$area_colhida_hectares_ano_2016_temporarias)/Banco_lavouras$area_colhida_hectares_ano_2016_temporarias

library(tibble)
Ind_1_2016 <-tibble(Ind_1_2016)
Ind_1_2016$nome <- names(Banco_lavouras)[1]

# Colocar na metodologia
# Tentamos criar o ndicador1 (Area Colhida / Área Plantada), mas não funcionou
# Virou uma constante e não foi considerado na análise. 
# Então partimos para a construção do indicador 2....

names(Banco_lavouras)[1] <-'nome'
Ind_2_2016 <-tibble(Ind_2_2016)
Ind_2_2016$nome <- Banco_lavouras$nome

library(dplyr)
Banco_lavouras <- Banco_lavouras %>% left_join(Ind_2_2016)

Banco_lavouras$valor_da_producao_mil_reais_ano_2016_temporarias[91]
Banco_lavouras$area_colhida_hectares_ano_2016_temporarias[91]            
Banco_lavouras$valor_da_producao_mil_reais_ano_2016_temporarias[24]
Banco_lavouras$area_colhida_hectares_ano_2016_temporarias[24]            

# NAs (Missings) foram produzidos durante a construção desse indicar.
# O IBGE não informou o valor da produção nem a área colhida de municiípios como
# Comendador Levy Gasparian & São João de Meriti

# Juntar o indicador 2 ao banco de dados


boxplot(Banco_lavouras$Ind_2_2016 ~ Banco_lavouras$rio_rural,col=c("red","royalblue"))
boxplot(Banco_lavouras$Ind_2_2016 ~ Banco_lavouras$mesorregiao,col="skyblue")
boxplot(Banco_lavouras$Ind_2_2016 ~ Banco_lavouras$microrregiao,col="skyblue")

library(ggplot2)
ggplot(Banco_lavouras) +
 aes(x = rio_rural, y = Ind_2_2016) +
 geom_boxplot(fill = "#45A8A5") +
 geom_jitter() +
 theme_minimal() +
 facet_wrap(vars(mesorregiao))

# Colocar na analise de resultados
# Os municipios com rio rural tem uma mediana maior que os municípios sem
# rio rural em 2016. 
# Isso sugere, em uma primeira visão, que o Rio Rural tem impacto na produtividade.
# Todavia, ao olhar simultaneamente as variáveis "rio rural" e "meso-região"
# podemos observar que esse processo é bastante complexo.
# enquanto no Centro fluminense os municipios sem rio rural tem uma produtividade maior,
# no noroeste fluminense o rio rural tem um grande efeito.
# isso indica que a região onde foi implementado o rio rural tem um grande efeito no resultado final.
# vale a pena fazer mapa do indicador 2 por Rio rural..

library(brazilmaps)
rio_map <- get_brmap(geo = "City",
                     geo.filter = list(State = 33),
                     class = "sf")
#plot_brmap(rio_map)
Banco_lavouras$nome <- toupper(Banco_lavouras$nome)

invisible(Mapa_basededados <- merge(rio_map, Banco_lavouras, by.x = "nome", by.y = "nome", all = TRUE))

library(RColorBrewer)
RColorBrewer::display.brewer.all()

paleta <- colorQuantile("YlGnBu", Mapa_basededados$Ind_2_2016, n = 5)

library(leaflet)
leaflet(Mapa_basededados) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              label= Mapa_basededados$nome,
              labelOptions = labelOptions(noHide = T,#opacity=0.5,
                                          style = list("font-weight" = "bold"),
                                          textOnly = TRUE,
                                          textsize='10px'),
              fillColor = ~colorQuantile("YlGnBu", Ind_2_2016)(Ind_2_2016)
  )%>% addProviderTiles(providers$Esri.WorldImagery) %>%
  addLegend(pal = paleta, values = ~Ind_2_2016, opacity = 1)

#----------------------------------------------------------------------
names(Banco_Familiar_wide)
Banco_class_familiar <- Banco_Familiar_wide %>% select(municipio,x6_Total,`x6_Produção de lavouras temporárias`,`x6_Produção de lavouras permanentes`)  
names(Banco_class_familiar)
Banco_class_familiar <- Banco_class_familiar %>% rename(nome=municipio,
                                classificacao_total=x6_Total,
                                classificacao_temporaria=`x6_Produção de lavouras temporárias`,
                                classificacao_permanente=`x6_Produção de lavouras permanentes`)
Banco_class_familiar$nome <- toupper(Banco_class_familiar$nome)
Banco_lavouras <- Banco_lavouras %>% left_join(Banco_class_familiar,by = "nome")

boxplot(Banco_lavouras$Ind_2_2016 ~ Banco_lavouras$classificacao_total,col=c("red","royalblue"))
# Apesar do Maior  valor no indicador 2 ser "NÃO FAMILIAR",
# Esse é um outlier e olhando a mediana, temos que
# a agricultura familiar é superior em 2016.


library(ggplot2)
ggplot(Banco_lavouras) +
 aes(x = rio_rural, y = Ind_2_2016, fill = classificacao_total) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

tabela<-table(Banco_lavouras$classificacao_total, 
              Banco_lavouras$rio_rural)
barplot(tabela,
        beside = TRUE,
        legend.text = rownames(tabela),
        col=c('red','royalblue'))

# O rio rural alcançou tanto a agricultura familiar quanto a 
# agricultura NÃO familiar

#----------------------------------------------------------------
# Testes de Hipóteses
#----------------------------------------------------------------
# O que é mais importante para a produtividade?
# A política pública Rio Rural ou o ser um municipio de
# agricultura familiar?

wilcox.test(Banco_lavouras$Ind_2_2016 ~ Banco_lavouras$classificacao_total)
# A agricultura familiar mediana é melhor e significativa a 10%
wilcox.test(Banco_lavouras$Ind_2_2016 ~ Banco_lavouras$rio_rural)
# O Rio Rural mediano não é significativo


#----------------------------------------------
# 1. Revisão/Estudo do código
# 2. Fazer para 2017, 2018, 2019
# 3. Fazer para temporario e permanente
# 4. Escrever Capítulo de "Método"
#     #origem dos dados, 
      #operacionalização dos indicadores (como foram criados 
      #os indicadores,como foram criadas as variáveis rio-rural e Agr-familiar)
      # mapa, boxplot e teste wilcoxon
# 5. Escrever Capítulo de "Análise de Resultados" 

# em um outro momento
# Objetivos de pesquisa/Questão de pesquisa/Hipóteses 
# Introdução com justificativa (importância do tema agricultura familiar)
# Revisão da literatura
# Conclusão (+ pra frente)

#--------------------------------------------------
# próxima reunião 25/06 as 10:30








































# PIB AGRO












