
library(tidyverse)
library(plotly)
library(lubridate)
library(readr)
library(dygraphs)
library(xts)

cargahoraria <- read_csv("diahidreletrica.csv")
glimpse(cargahoraria)
cargahoraria = cargahoraria[,-c(1,3,4,5,6,7)]
colnames(cargahoraria)[1] = "Dia"
colnames(cargahoraria)[2] = "Carga"
cargahoraria = cargahoraria[-which(is.na(cargahoraria$Dia)),]

cargahoraria$Dia=dmy(cargahoraria$Dia)

ts = xts(x = cargahoraria$Carga, order.by = cargahoraria$Dia)

cargahoraria %>%
  ggplot()+
  geom_histogram(aes(x=cargahoraria$Carga), fill="blue", alpha=0.6)+
  labs (y=" ", x="Geração Hidrelétrica")+
  ggtitle("Distribuição da Geração Hidrelétrica")
  
  
tsgraph = dygraph(ts,
                  main="Geração Diária Hidrelétrica no SIN",
                  ylab="(MWmed)") %>%
  dySeries("V1", label="Geração") %>%
  dyShading(from = "2020-2-26 00:00:00", to = "2020-4-6 23:00:00", color = "red") %>%
  dyShading(from = "2000-7-01", to = "2002-2-19", color = "blue") %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="#636363") %>%
  dyAnnotation("2020-2-26", text = "C", tooltip = "Primeiro Caso de COVID-19") %>%
  dyAnnotation("2000-7-01", text = "A", tooltip = "Crise do Apagão de 2001") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
