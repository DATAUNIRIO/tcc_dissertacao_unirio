
library(readxl)
dados_para_mapa <- read_excel("C:/Users/Hp/Downloads/resultado para mapa.xlsx")
names(dados_para_mapa)


# MAPA MUNDI
require(rworldmap)
sPDF <- joinCountryData2Map(dados_para_mapa, joinCode = "ISO3", nameJoinColumn = "ISO3V10")
mapCountryData( sPDF, nameColumnToPlot="Cluster_Nao_Hierarquico",
                colourPalette = c("red","blue","grey"),mapTitle = "Cluster Não Hierárquico", oceanCol ="lightblue")

mapCountryData( sPDF, nameColumnToPlot="Cluster_Hierarquico",
                colourPalette = c("green","red"),mapTitle = "Cluster Hierárquico", oceanCol ="lightblue")

mapCountryData( sPDF, nameColumnToPlot="dois_metodos",
                colourPalette = c("purple","royalblue","yellow"),mapTitle = "Cluster Não Hierárquico & Cluster Hierárquico", oceanCol ="lightblue")
