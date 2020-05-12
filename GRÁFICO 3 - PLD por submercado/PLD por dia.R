library(tidyverse)
library(plotly)
library(lubridate)
library(readr)


### GRÁFICO 3 - PLD por submercado
pld <- read_csv("plddia.csv")
pld = pld[,4:17]
pld$`Data Início` = dmy(pld$`Data Início`)
pld$`Data Fim` = dmy(pld$`Data Fim`)

Datas=as.tibble(seq.Date(pld[1,]$`Data Início`,pld[nrow(pld),]$`Data Fim`, by="days"))

pldtidy = pld %>% slice(rep(1:n(), each = 7))

pldtidy = tibble(Data=Datas$value, Pesado_SE=pldtidy$`Pesado SE`, Médio_SE=pldtidy$`Médio SE`,Leve_SE=pldtidy$`Leve SE`,Pesado_S=pldtidy$`Pesado S`,Médio_S=pldtidy$`Médio S`,Leve_S=pldtidy$`Leve S`,Pesado_NE=pldtidy$`Pesado NE`,Médio_NE=pldtidy$`Médio NE`,Leve_NE=pldtidy$`Leve NE`, Pesado_N = pldtidy$`Pesado N`, Médio_N=pldtidy$`Médio N`, Leve_N=pldtidy$`Leve N`)

pldtidy = pldtidy %>%
  pivot_longer(cols=2:13)

pldtidy = pldtidy %>%
  separate(col=2, sep = "_", into=c("Patamar", "Submercado"))

colnames(pldtidy)[4] = "Preço"
pldtidy[,4] = pldtidy$Preço/100
pldtidy$Submercado = str_replace(pldtidy$Submercado,"SE","SE/CO")

pldts = pldtidy %>%
  group_by(Submercado,Data) %>%
  summarise(PreçoMédio = round(mean(Preço),2)) %>%
  ungroup(Submercado,Data)


graphpldts = pldts %>%
  ggplot(mapping=aes(x=Data,
                     y=PreçoMédio,
                     group=Submercado,
                     fill=Submercado))+
  geom_area(alpha=0.8 , size=.5, colour="#525252")+
  scale_fill_manual(values=c("#b2df8a", "#a6cee3","#1f78b4","#33a02c"))+
  labs(y="Média de Preços (R$/MWh)")+
  ggtitle("Comportamento do PLD por Submercado")+
  geom_vline(xintercept = as.numeric(dmy("26-02-2020")),
             col="red",
             linetype ="dotted",
             size = 1,
             alpha =0.3) +
  annotate("text", x=dmy("26-02-2020"),y=1500, label="Primeiro Caso de COVID19", size=3, col = "red", alpha=0.6) +
  geom_vline(xintercept = as.numeric(dmy("28-03-2020")),
             col="red",
             linetype ="dotted",
             size = 1,
             alpha = 0.3)+
  annotate("text", x=dmy("28-03-2020"),y=800, label="PLD atinge o piso\nem todos submercados", size=3, col = "red", alpha=0.6)
            


ggplotly(graphpldts, tooltip = c("x","y","fill"))
