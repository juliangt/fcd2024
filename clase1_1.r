# PARTE1-ejemplos de clase 1:AED
#  autor: SP
# fecha: 2024/06/10
#------------------
#las siguientes lineas son para limpieza
rm(list=ls())
gc()
##----------
#setwd("C:/Users/...............") recordar setear el lugar de trabajo!

library(readr) #incluye los siguientes:
#read_csv() lee archivos delimitados por coma, 
#read_csv2() lee archivos separados por punto y coma,
#read_delim() archivos con cualquier delimitador.
library(ggplot2) #para grÃ¡ficos lindos
library(dplyr) #para manejo de datos

####### EJEMPLO 1: ABALONE ############
#DATOS de la edad del abalone (especie de crustaceo) a partir de mediciones f?sicas. 
#La edad del abalone se determina cortando la concha a traves del cono, y contando el numero de anillos a traves del microscopio 
#datos en UCI: https://archive.ics.uci.edu/datasets

#datos <- read.table("abalone.txt", header = F, sep = ",") 
datos <- read.delim("abalone.txt", header = F, sep = ",") 
head(datos) #para que muestre las primeras 5 o 6 filas

colnames(datos)<-c("Genero", "Long","Diam","Altura","Peso","Peso1","Peso2","Peso3","N.anillos")
head(datos) #para que muestre las primeras 5 o 6 filas
dim(data)
datos$Genero<- as.factor(datos$Genero)
datos<-as.data.frame(datos)
View(datos)
#para ver estructura de los datos (usa library(dplyr))
glimpse(datos)

#GRAFICOS DE VARIABLES CUALITATIVAS O DISCRETAS
graf.barras<-ggplot(data = datos,aes(x = Genero)) +
  geom_bar(mapping = aes(x = Genero),fill=4) 

#GRAFICOS DE VARIABLE CONTINUA: Histogramas
graf.histo<-ggplot(datos, aes(x = Long, fill=Long)) + geom_histogram(aes(x=Long,y = ..density..),binwidth=0.1,
                                                                     fill = "skyblue", color = "blue") +
  labs(title="Histograma + densidad estimada")+
  geom_density(lwd=0.5)+ theme_light() #histograma de la longitud del abalone

##MEDIDAS RESUMEN:
summary(datos)
mean(datos$Long)
attach(datos)
mean(Long) #media de la muestra o promedio
sd(Long) #desvÃ­o muestral
var(Long) #varianza muestral

#se pueden calcular los percentiles:
percentil_Long = quantile(Long, c(0.25, 0.5, 0.75), na.rm = T)
df_p=data.frame(value=percentil_Long, percentile=c("Q1", "mediana", "Q3"))

### Graficar Histograma con los percentiles
graf.histo + geom_vline(data=df_p, aes(xintercept=value,  colour = percentile), show.legend = TRUE, linetype="dashed") + theme_light()

### BOXPLOTS ##
#bÃ¡sicos:
boxplot(datos$Long, las=1, col="darkseagreen2",
        horizontal=TRUE,
        xlab="Longitud")
#con ggplot:
ggplot(datos, aes(y=Long)) +  geom_boxplot() +  # theme_minimal()+
  ylab("Longitud") +labs(title="Boxplot de Longitud")

#boxplot para cada gÃ©nero:
graf.box<-ggplot(data = datos, aes(y = Long), colour = factor(Genero)) +
  geom_boxplot(aes(x = Genero, fill = factor(Genero))) +
  xlab("GÃ©nero") + ylab("Longitud") +labs(title="Boxplot de Longitud por gÃ©nero")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.line = element_line(colour = "royalblue", size = 0.5, linetype = "solid")) +
  labs(fill = "GÃ©nero") + scale_fill_brewer(palette="BuPu")


#CORRELACION entre dos variables cuantitativas
cor(Long,Diam)

#para hacerlo un poco mejor:
install.packages("tidyverse")
library(tidyverse)
#lo siguiente permite calcular correlaciones entre Diam y Long para cada grupo de gÃ©nero
datos %>%
  group_by(Genero) %>%
  summarize(r = cor(Diam, Long, use = "complete.obs"))

## para ver todas las CORRELACIONES de a pares:
library(corrplot)
#cuantis<-datos[,c("Long","Diam","Altura","Peso","Peso1","Peso2","Peso3")]
cuantis<- datos[,-c(1,9)] #equivalente a la anterior
plot(cuantis)

correlaciones<-cor(cuantis) #requiere corrplot. Las variables deben ser numÃ©ricas.
correlaciones
corrplot(correlaciones, method="number",tl.col="black",tl.cex=0.7,cl.cex=0.7,number.cex=0.6,insig ='blank',pch.cex=0.2)
corrplot(correlaciones, method="color",tl.col="black",tl.cex=0.7)

#GRAFICOS DE DISPERSION

ggplot(datos, aes(x = Long, y = Diam))+ geom_point()
#ahora separando por gÃ©nero:
ggplot(datos, aes(x = Long, y = Diam, colour = Genero))+ geom_point()

#Otra forma de hacer los graficos separando por genero
ggplot(datos, aes(x = Long, y = Diam))+ geom_point()+ facet_grid(~ Genero)

#ahora lo nombro para ir agregando capas de a poco
graf.Disp<-ggplot(datos, aes(x = Long, y = Diam, colour = Genero))+ geom_point() 

graf.Disp+ggtitle("Longitud y diametro segÃºn GÃ©nero") + 
  labs(x = "Longitud", 
       y = "DiÃ¡metro", 
       colour = "GÃ©nero") 

########### QQ-PLOT para ver normalidad ##########
ggplot(datos, aes(sample = Long)) +
  geom_qq() +
  geom_qq_line()

# otros grÃ¡ficos mÃ¡s lindos:
par(mfrow=c(1, 2))
require(car)  # Debe instalar antes el paquete car para qqPlot
qqPlot(datos$Long, pch=19, id=FALSE,
       main='QQplot para Longitud',
       xlab='Cuantiles teÃ³ricos',
       ylab='Cuantiles muestrales')
hist(datos$Long, freq=TRUE,
     main='Histograma',
     xlab='Longitud (cm)',
     ylab='Frecuencia')


########## EJEMPLO 1bis: otros grÃ¡ficos con datos de diamantes
library(tidyverse)
diamonds #dataset incorporado en el paquete
summary(diamonds)
View(diamonds)
count(diamonds,cut) #equivale a 
diamonds%>%count(cut)
# barras
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
#miro dos variables categÃ³ricas:
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color)) 
#mÃ¡s lindo
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# DISTRIBUCION DE UNA CONTINUA SEGUN FACTOR
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 1000)


