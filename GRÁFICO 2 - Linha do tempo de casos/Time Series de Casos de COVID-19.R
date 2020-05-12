library(tidyverse)
library(plotly)
library(lubridate)
library(readr)


##GRÁFICO 2 - Linha do tempo de casos
covidestadots <- read_csv("covidestadots.csv")
covidestadots = covidestadots[,c(1,2,5,6)]

glimpse(covidestadots)

i=1
for(i in 1:nrow(covidestadots)){
  
  if(covidestadots[i,]$state=="RS" | covidestadots[i,]$state=="PR" | covidestadots[i,]$state=="SC"){
    
    covidestadots[i,5] = "S"
  }
  
  if(covidestadots[i,]$state=="SP" | covidestadots[i,]$state=="RJ" | covidestadots[i,]$state=="ES" |covidestadots[i,]$state=="MG"|covidestadots[i,]$state=="GO"|covidestadots[i,]$state=="DF"|covidestadots[i,]$state=="MS"|covidestadots[i,]$state=="MT"|covidestadots[i,]$state=="RO"|covidestadots[i,]$state=="AC"){
    
    covidestadots[i,5] = "SE/CO"
  }
  
  if(covidestadots[i,]$state=="BA" | covidestadots[i,]$state=="PI" | covidestadots[i,]$state=="SE" |covidestadots[i,]$state=="AL"|covidestadots[i,]$state=="PE"|covidestadots[i,]$state=="PB"|covidestadots[i,]$state=="RN"|covidestadots[i,]$state=="CE"){
    
    covidestadots[i,5] = "NE"
  }
  
  if(covidestadots[i,]$state=="TO" | covidestadots[i,]$state=="MA" | covidestadots[i,]$state=="PA" |covidestadots[i,]$state=="AP"|covidestadots[i,]$state=="AM"){
    
    covidestadots[i,5] = "N"
  }
  
  if(covidestadots[i,]$state=="RR"){
    covidestadots[i,5] = "Não conectado ao SIN"
  }
  
}

colnames(covidestadots)[1]="Data"
colnames(covidestadots)[3]="Casos Confirmados"
colnames(covidestadots)[4]="Mortes Confirmadas"
colnames(covidestadots)[5]="Submercado"

covidestadots=covidestadots[-c(1,2),]

graphcovid = covidestadots %>%
  group_by(Data,Submercado)%>%
  summarise(Confirmados=sum(`Casos Confirmados`),
            Mortes=sum(`Mortes Confirmadas`)) %>%
  ggplot(mapping=aes(x=Data,
                     y=Confirmados,
                     group= Submercado,
                     text=paste("Mortes:",Mortes),
                     fill=Submercado))+
           geom_col(alpha=0.8 , size=.25, colour="#525252")+
  scale_fill_manual(values=c("#b2df8a","black","#a6cee3","#1f78b4", "#33a02c"))+
  ggtitle("Linha do tempo dos casos confirmados de COVID-19")

ggplotly(graphcovid, tooltip = c("x","y","text","fill"))
