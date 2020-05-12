
library(brazilmaps)
library(ggmap)
library(tidyverse)
library(readr)
library(lubridate)
library(readr)


### GRÁFICO 1 - Mapa Interativo
covid<- read_csv("covidcidade.csv")
register_google(key="INSIRA AQUI SEU API")
brmap = get_brmap(geo="State")
covid$geocode = paste(covid$city, covid$state, "Brasil")
local = geocode(covid$geocode)
covid = as.tibble(covid)
covid = tibble(covid,local)

colorsul = brmap[c(27,22,21),]
colornorte= brmap[c(3,5,7,8,6),]
colorna = brmap[4,]
colornordeste = brmap[c(16,9,10,15,14,13,12,11),]
colorsudestecentro = brmap[c(2,1,24,23,25,26,17,20,19,18),]

covid %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data=colorsul, weight=1, color="#1f78b4") %>%
  addPolygons(data=colornordeste, weight=1, color="#a6cee3") %>%
  addPolygons(data=colorna, weight=1, color="black") %>%
  addPolygons(data=colorsudestecentro, weight=1, color="#33a02c") %>%
  addPolygons(data=colornorte, weight=1, color="#b2df8a") %>%
  addCircleMarkers(lng=~local$lon,
                   lat=~local$lat,
                   radius =covid$covid$confirmed/100,
                   color = "red",
                   label=paste(covid$covid$city,", ", covid$covid$state),
                   popup=paste("Cidade: ",covid$covid$city,", ",covid$covid$state,
                               "<br/>População: ",covid$covid$estimated_population_2019,
                               "<br/>Casos Confirmados: ",covid$covid$confirmed,
                               "<br/>Mortes Confirmadas: ",covid$covid$deaths)) %>%
  addLegend(colors=c("#1f78b4", "#33a02c", "#a6cee3","#b2df8a","black", "red"),
            label=c("Submercado Sul","Submercado Sudeste/Centro-Oeste", "Submercado Nordeste","Submercado Norte", "Não conectado ao SIN", "Casos de COVID-19 por cidade"))


  
  
  
  

