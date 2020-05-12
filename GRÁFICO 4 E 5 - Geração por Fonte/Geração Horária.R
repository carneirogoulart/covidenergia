library(tidyverse)
library(plotly)
library(lubridate)
library(readr)

geracaosolar = read_csv("geracaosolar.csv")
geracaoeolica = read_csv("geracaoeolica.csv")
geracaotermica = read_csv("geracaotermica.csv")
geracaohidreletrica = read_csv("geracaohidreletrica.csv")
geracaonuclear = read_csv("geracaonuclear.csv")

geracaoeolica = geracaoeolica[,c(1,5,8)]
geracaotermica = geracaotermica[,c(1,5,8)]
geracaosolar = geracaosolar[,c(1,5,8)]
geracaohidreletrica = geracaohidreletrica[,c(1,5,8)]
geracaonuclear = geracaonuclear[,c(1,5,8)]

geracao = bind_rows(geracaoeolica,geracaohidreletrica,geracaonuclear,geracaosolar, geracaotermica)
colnames(geracao)[1]="Data"
colnames(geracao)[2]="Fonte"
colnames(geracao)[3]="Geração"

geracao = geracao[-which(is.na(geracao$Data)),]

geracao$Data = dmy_hms(geracao$Data)

geracaomap = geracao %>%
  ggplot(mapping=aes(x=Data,
                     y=Geração,
                     group=Fonte,
                     color=Fonte))+
  geom_line(size = 0.3)+
  scale_color_manual(values=c("#e7298a","blue", "black","yellow","#a1d76a"))+
  labs(y="Geração de Energia (MWmed)")+
  ggtitle("Comportamento da Geração por Fonte")+
  geom_vline(xintercept = as.numeric(dmy_hms("26-02-2020 00:00:00")),
             col="red",
             linetype ="dotted",
             alpha=0.3)+
  annotate("text", x=dmy_hms("26-02-2020 00:00:00"),y=74000, label="Primeiro Caso", size=3, col = "red", alpha=0.6)

ggplotly(geracaomap, tooltip=c("x","y","group"))
