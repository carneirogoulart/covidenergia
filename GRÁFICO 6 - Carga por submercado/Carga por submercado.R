library(tidyverse)
library(plotly)
library(lubridate)
library(readr)

carganordeste = read_csv("carganordeste.csv")
cargasudeste = read_csv("cargasudeste.csv")
carganorte = read_csv("carganorte.csv")
cargasul = read_csv("cargasul.csv")

carganordeste = carganordeste[,-c(1,3)]
cargasudeste = cargasudeste[,-c(1,3)]
carganorte = carganorte[,-c(1,3)]
cargasul = cargasul[,-c(1,3)]


carga = bind_rows(carganordeste,cargasudeste,carganorte,cargasul)
carga$Subsistema = str_replace(carga$Subsistema,"Norte", "N")
carga$Subsistema = str_replace(carga$Subsistema,"Nordeste", "NE")
carga$Subsistema = str_replace(carga$Subsistema,"Sul", "S")
carga$Subsistema = str_replace(carga$Subsistema,"Sudeste/Centro-Oeste", "SE/CO")

carga = as_tibble(carga)

colnames(carga)[1] = "Data"
colnames(carga)[3] = "Carga"

glimpse(carga)

carga$Data = str_split_fixed(carga$Data, " ",n=2)
carga$Data = carga$Data[,1]
carga$Data = dmy(carga$Data)

cargamap = carga %>%
  ggplot(mapping=aes(x=Data,
                     y=Carga,
                     group=Subsistema,
                     color=Subsistema,
                     text=paste("Subsistema:",Subsistema)))+
  geom_line()+
  scale_y_log10()+
  scale_color_manual(values=c("#b2df8a", "#a6cee3","#1f78b4","#33a02c"))+
  labs(y="Carga de Energia (MWmed)")+
  ggtitle("Comportamento da Carga por Submercado")+
  geom_vline(xintercept = as.numeric(dmy("26-02-2020")),
             col="red",
             linetype ="dotted",
             size = 1,
             alpha=0.3) +
  annotate("text", x=dmy("26-02-2020"),y=50000, label="Primeiro Caso de COVID19", size=2, col = "red", alpha=0.6)+
  facet_wrap(~Subsistema)

ggplotly(cargamap, tooltip = c("x","text","y"))
