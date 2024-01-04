##----TRABAJO PERSONAL CURSO "PROGRAMACION APLICADA CON R"
##----PROFESOR: GUSTAVO GONZALES BONORINO
##----ALUMNO: MARTIN LEON


##-------------------------------------------------------------------------
##-----ANALISIS COMPARATIVO DE DATOS ENERGETICOS ENTRE ARGENTINA Y COLOMBIA
##-------------------------------------------------------------------------

# Datos obtenidos de la página: https://data.worldbank.org/topic/energy-and-mining

##Librerías
library(tidyverse)
library(readxl)  
library(dplyr)
library(tidyr)
library(ggplot2)
library(sp)
library(sf)
library(raster)


rm(list = ls())

#Seteo el directorio de trabajo
setwd("/Users/martinleon/Desktop/Academicos/CENTRO REDES/R/Ejercicio final")


#Obtengo los datos a comparar
datos<-read_excel("Datos.xls", skip = 2)

colombia<-subset(datos, datos$`Country Name` == "Colombia")
argentina<-subset(datos, datos$`Country Name` == "Argentina")
datos_CA<-rbind(argentina,colombia)

#Exploración de los datos
head(datos_CA)
class(datos_CA)
dim(datos_CA)
names(datos_CA)

#Elimino columnas no útiles
datos_CA<-datos_CA[-c(2)]
datos_CA<-datos_CA[-c(3)]

#Renombre de columnas
names(datos_CA)[1]<- "Pais"
names(datos_CA)[2]<- "Indicador"


#Gráficos------------------------------------

#Series temporales:

#Argentina
tiempoARG<-datos_CA [37:46,]
tiempoARG<-tiempoARG[-c(10),]
tiempoARG<-tiempoARG[-c(7),]
tiempoARG<-tiempoARG[-c(6),]
tiempoARG<-tiempoARG[-c(3:4),]
tiempoARG<-tiempoARG[-c(2),]

transARG<-data.frame(t(tiempoARG[-1]))
transARG<-cbind(Año=rownames(transARG),transARG)
transARG<-transARG[-c(1),]

#Colombia
tiempoCOL<-datos_CA[87:96,]       
tiempoCOL<-tiempoCOL[-c(10),]
tiempoCOL<-tiempoCOL[-c(7),]
tiempoCOL<-tiempoCOL[-c(6),]
tiempoCOL<-tiempoCOL[-c(3:4),]
tiempoCOL<-tiempoCOL[-c(2),]

transCOL<-data.frame(t(tiempoCOL[-1]))
transCOL<-cbind(Año=rownames(transCOL),transCOL)
transCOL<-transCOL[-c(1),]

#Convertir los NA a ceros
haz.cero.na = function(x){
  ifelse(is.na(x),0,x)
}
transARG = data.frame(sapply(transARG, haz.cero.na))
transCOL = data.frame(sapply(transCOL, haz.cero.na))


#Transformo farmato a numerico las columnas correspondientes
transARG$X1<-as.numeric(transARG$X1)
transARG$X2<-as.numeric(transARG$X2)
transARG$X3<-as.numeric(transARG$X3)
transARG$X4<-as.numeric(transARG$X4)
transCOL$X1<-as.numeric(transCOL$X1)
transCOL$X2<-as.numeric(transCOL$X2)
transCOL$X3<-as.numeric(transCOL$X3)
transCOL$X4<-as.numeric(transCOL$X4)

#Redondeo valores a 2 decimales
transARG$X1<-round(transARG$X1,2)
transARG$X2<-round(transARG$X2,2)
transARG$X3<-round(transARG$X3,2)
transARG$X4<-round(transARG$X4,2)
transCOL$X1<-round(transCOL$X1,2)
transCOL$X2<-round(transCOL$X2,2)
transCOL$X3<-round(transCOL$X3,2)
transCOL$X4<-round(transCOL$X4,2)

#Renombrar columnas
names(transARG)[2]<-"% energía renovable"
names(transARG)[3]<-"% energía nuclear"
names(transARG)[4]<-"% energía hidroeléctrica"
names(transARG)[5]<-"% energía combustibles"
names(transCOL)[2]<-"% energía renovable"
names(transCOL)[3]<-"% energía nuclear"
names(transCOL)[4]<-"% energía hidroeléctrica"
names(transCOL)[5]<-"% energía combustibles"


#Gráfico temporal para Argentina
ggplot(data=transARG, aes(x = Año)) +
  geom_point(aes(y=`% energía renovable`), color=ifelse(transARG$`% energía renovable`==0,"transparent","green")) +
  geom_point(aes(y=`% energía nuclear`), color=ifelse(transARG$`% energía nuclear`==0,"transparent","violet")) +
  geom_point(aes(y=`% energía hidroeléctrica`), color=ifelse(transARG$`% energía hidroeléctrica`==0,"transparent","blue")) +
  geom_point(aes(y=`% energía combustibles`), color=ifelse(transARG$`% energía combustibles`==0,"transparent","red")) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 50, linetype="dashed") +
  theme_classic() +
  labs(title="Proporción de tipos de fuentes de energía en el tiempo",
       subtitle = "para Argentina",
       x="Año",
       y="Porcentaje (%)") +
  scale_x_discrete(breaks = c("1960","1980","2000","2020")) +
  annotate("text", x="1983", y=85, label="Combustibles fósiles", size=3,color="red") +
  annotate("text", x="1970", y=30, label="Hidroeléctrica", size=3,color="blue") +
  annotate("text", x="1990", y=20, label="Nuclear", size=3,color="violet") +
  annotate("text", x="2015", y=10, label="Renovable", size=3,color="green")


#Gráfico temporal para Colombia
ggplot(data=transCOL, aes(x = Año)) +
  geom_point(aes(y=`% energía renovable`), color=ifelse(transCOL$`% energía renovable`==0,"transparent","green")) +
  geom_point(aes(y=`% energía nuclear`), color=ifelse(transCOL$`% energía nuclear`==0,"transparent","violet")) +
  geom_point(aes(y=`% energía hidroeléctrica`), color=ifelse(transCOL$`% energía hidroeléctrica`==0,"transparent","blue")) +
  geom_point(aes(y=`% energía combustibles`), color=ifelse(transCOL$`% energía combustibles`==0,"transparent","red")) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 50, linetype="dashed") +
  theme_classic() +
  labs(title="Proporción de tipos de fuentes de energía en el tiempo",
       subtitle = "para Colombia",
       x="Año",
       y="Porcentaje (%)") +
  scale_x_discrete(breaks = c("1960","1980","2000","2020")) +
  annotate("text", x="1980", y=40, label="Combustibles fósiles", size=3,color="red") +
  annotate("text", x="1975", y=75, label="Hidroeléctrica", size=3,color="blue") +
  annotate("text", x="1970", y=15, label="No posee energía nuclear", size=3,color="violet") +
  annotate("text", x="2015", y=8, label="Renovable", size=3,color="green")


#Comparativo
transARG<-transARG %>% mutate(País="ARG")
transCOL<-transCOL %>% mutate(País="COL")
data<-rbind(transARG,transCOL)
data<-data[c(1,11,21,31,41,51,61,64,74,84,94,104,114,124),]


# Creación del gráfico de barras
ggplot(data, aes(x = Año, y = `% energía hidroeléctrica`, fill = País)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Proporción de fuentes de energía hidroeléctrica",
    x = "Año",
    y = "Porcentaje (%)",
    fill = "País"
  ) +
theme_classic() + 
geom_hline(yintercept = 0, linetype="dashed") +
geom_hline(yintercept = 50, linetype="dashed") 


ggplot(data, aes(x = Año, y = data$`% energía renovable`, fill = País)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Proporción de fuentes de energía renovable",
    x = "Año",
    y = "Porcentaje (%)",
    fill = "País"
  ) +
  theme_classic() + 
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 5, linetype="dashed") 


ggplot(data, aes(x = Año, y = `% energía combustibles`, fill = País)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Proporción de fuentes de energía de combustibles fósiles",
    x = "Año",
    y = "Porcentaje (%)",
    fill = "País"
  ) +
  theme_classic() + 
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 50, linetype="dashed") 


ggplot(data, aes(x = Año, y = data$`% energía nuclear`, fill = País)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Proporción de fuentes de energía nuclear",
    x = "Año",
    y = "Porcentaje (%)",
    fill = "País"
  ) +
  theme_classic() + 
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 15, linetype="dashed") +
  annotate("text", x="2010", y=13, label=c("(Colombia no posee)"), size=3,color="turquoise")


#Mapa de Córdoba, Argentina y sus lineas de aguas continentales
shpPais<-st_read("/Users/martinleon/Desktop/Academicos/CENTRO REDES/R/Ejercicio final/CORDOBA.shp")
head(shpPais)

shpAgua<-st_read("/Users/martinleon/Desktop/Academicos/CENTRO REDES/R/Ejercicio final/AGUAS_CORDOBA.shp")
head(shpAgua)

shpEmbalse<-st_read("/Users/martinleon/Desktop/Academicos/CENTRO REDES/R/Ejercicio final/EMBALSE_CORDOBA.shp")
head(shpEmbalse)


#Visualizacion de shapefiles
ggplot() +
  geom_sf(data = shpPais, fill = "grey", color = "black", alpha = 0.5) +
  geom_sf(data = shpAgua, fill = "blue", color = "blue", alpha = 0.7) +
  geom_sf(data = shpEmbalse, fill = "red", color = "black", alpha = 0.7) +
  labs(
    title="Mapa de Córdoba",
    subtitle="Corrientes de agua y embalses"
  ) +
  theme_minimal()
