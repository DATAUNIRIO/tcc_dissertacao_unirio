
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
