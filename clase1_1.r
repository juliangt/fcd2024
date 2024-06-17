# Clase 1 - PARTE 1 - ejemplos de clase 1 : AED
# autor: SP
# modified by jgt 
# fecha: 2024/06/10

# recomiendo leer algo como esto: https://www.uv.es/vcoll/conceptos-basicos.html 
# previo a cualquier trabajo con R 

# setwd -> set working directory, donde nos paramos para buscar archivos luego en forma relativa. 
setwd("./Documents/Esp Ciencia Datos/Fundamentos CD")

# se puede verificar working directory con getwd()
getwd()

# algunas librerías a utilizar en el programa. 
library(readr)
library(ggplot2) #para gráficos lindos
library(dplyr) #para manejo de datos


# las siguientes dos sentencias son para "dejar todo en blanco" y comenzar de cero
# Si bien no es necesario cuando comenzamos de cero, esto limpia variables y funciones en memoría
rm(list=ls())

# llama directamente a garbage collector, después investigo si no se hace automáticamente cada x tiempo. 
gc() 


####### EJEMPLO 1: ABALONE ############
# DATOS de la edad del abalone (especie de crustaceo) a partir de mediciones fisicas. 
# La edad del abalone se determina cortando la concha a traves del cono, 
# y contando el numero de anillos a traves del microscopio 
# datos en UCI: https://archive.ics.uci.edu/datasets


# Lectura de archivo
# cualquiera de las dos lineas parecen ser equivalentes, en la librería readr 
# podemos encontrar variantes, 
# Esto lee el archivo csv abalone.txt, lo convierte en una tabla y guarda en variable "datos"

datos <- read.table("abalone.txt", header = F, sep = ",") 
#datos <- read.delim("abalone.txt", header = F, sep = ",") 

# devuelve los primeras filas de variable datos. 
head(datos) 

# devuelve las últimas filas de variable datos. 
tail(datos)

# dim nos devuelve la dimención de la tabla, en este caso 4177 x 9 (filas x columnas)
dim(datos)

# setea nombre a las columnas de la tabla en variable datos. 
# la funcion c es de "concatenate", devuelve los componentes a utilizar como nombre de columnas en tabla datos.
colnames(datos) <- c("Genero", "Long", "Diam", "Altura", "Peso", "Peso1", "Peso2", "Peso3", "N.anillos")


# un factor es representa una variable categórica 
# asigna en columna Genero de la tabla datos el mismo valor como factor. 
datos$Genero <- as.factor(datos$Genero)

# valida si datos es un data frame, sino lo intenta convertir. luego lo pisa en datos. 
datos <- as.data.frame(datos)

View(datos)

# para ver estructura de los datos (usa library(dplyr))
glimpse(datos)


# De acá en adelante realiza gráficos pero me falta entender las librerías 
# la más utilizada es ggplot creada por https://en.wikipedia.org/wiki/Hadley_Wickham
# que es una implementación del libro
# Grammar of Graphics de https://en.wikipedia.org/wiki/Leland_Wilkinson 

# Recomiendo empezar leyendo https://verso.mat.uam.es/~joser.berrendero/R/introggplot2.html 
 



#GRAFICOS DE VARIABLES CUALITATIVAS O DISCRETAS
graf.barras <- ggplot(data = datos,aes(x = Genero)) + geom_bar(mapping = aes(x = Genero), fill=4) 

#GRAFICOS DE VARIABLE CONTINUA: Histogramas
graf.histo <- 
  ggplot(datos, aes(x = Long, fill = Long) ) + 
  geom_histogram(
    aes(x=Long,y = ..density..), 
    binwidth=0.1, 
    fill = "skyblue", 
    color = "blue") +
  labs(title= "Histograma + densidad estimada" ) +
  geom_density(lwd=0.5)+ theme_light() #histograma de la longitud del abalone

##MEDIDAS RESUMEN:
summary(datos)
mean(datos$Long)
attach(datos)
mean(Long) #media de la muestra o promedio
sd(Long) #desvío muestral
var(Long) #varianza muestral

#se pueden calcular los percentiles:
percentil_Long = quantile(Long, c(0.25, 0.5, 0.75), na.rm = T)
df_p=data.frame(value=percentil_Long, percentile=c("Q1", "mediana", "Q3"))

### Graficar Histograma con los percentiles
graf.histo + geom_vline(data=df_p, aes(xintercept=value,  colour = percentile), show.legend = TRUE, linetype="dashed") + theme_light()

### BOXPLOTS ##
#básicos:
boxplot(datos$Long, 
        las=1, 
        col="darkseagreen2",
        horizontal=TRUE,
        xlab="Longitud")

#con ggplot:
ggplot(datos, aes(y=Long)) +  geom_boxplot() +  # theme_minimal()+
  ylab("Longitud") +labs(title="Boxplot de Longitud")

#boxplot para cada género:
graf.box<-ggplot(data = datos, aes(y = Long), colour = factor(Genero)) +
  geom_boxplot(aes(x = Genero, fill = factor(Genero))) +
  xlab("Género") + ylab("Longitud") +labs(title="Boxplot de Longitud por género")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.line = element_line(colour = "royalblue", size = 0.5, linetype = "solid")) +
  labs(fill = "Género") + scale_fill_brewer(palette="BuPu")


#CORRELACION entre dos variables cuantitativas
cor(Long,Diam)

#para hacerlo un poco mejor:
install.packages("tidyverse")
library(tidyverse)
#lo siguiente permite calcular correlaciones entre Diam y Long para cada grupo de género
datos %>%
  group_by(Genero) %>%
  summarize(r = cor(Diam, Long, use = "complete.obs"))

## para ver todas las CORRELACIONES de a pares:
library(corrplot)
#cuantis<-datos[,c("Long","Diam","Altura","Peso","Peso1","Peso2","Peso3")]
cuantis<- datos[,-c(1,9)] #equivalente a la anterior
plot(cuantis)

correlaciones<-cor(cuantis) #requiere corrplot. Las variables deben ser numéricas.
correlaciones
corrplot(correlaciones, method="number",tl.col="black",tl.cex=0.7,cl.cex=0.7,number.cex=0.6,insig ='blank',pch.cex=0.2)
corrplot(correlaciones, method="color",tl.col="black",tl.cex=0.7)

#GRAFICOS DE DISPERSION

ggplot(datos, aes(x = Long, y = Diam))+ geom_point()
#ahora separando por género:
ggplot(datos, aes(x = Long, y = Diam, colour = Genero))+ geom_point()

#Otra forma de hacer los graficos separando por genero
ggplot(datos, aes(x = Long, y = Diam))+ geom_point()+ facet_grid(~ Genero)

#ahora lo nombro para ir agregando capas de a poco
graf.Disp<-ggplot(datos, aes(x = Long, y = Diam, colour = Genero))+ geom_point() 

graf.Disp+ggtitle("Longitud y diametro según Género") + 
  labs(x = "Longitud", 
       y = "Diámetro", 
       colour = "Género") 

########### QQ-PLOT para ver normalidad ##########
ggplot(datos, aes(sample = Long)) +
  geom_qq() +
  geom_qq_line()

# otros gráficos más lindos:
par(mfrow=c(1, 2))
require(car)  # Debe instalar antes el paquete car para qqPlot
qqPlot(datos$Long, pch=19, id=FALSE,
       main='QQplot para Longitud',
       xlab='Cuantiles teóricos',
       ylab='Cuantiles muestrales')

hist(datos$Long, freq=TRUE,
     main='Histograma',
     xlab='Longitud (cm)',
     ylab='Frecuencia')


########## EJEMPLO 1bis: otros gráficos con datos de diamantes
library(tidyverse)
diamonds #dataset incorporado en el paquete
summary(diamonds)
View(diamonds)
count(diamonds,cut) #equivale a 
diamonds%>%count(cut)
# barras
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
#miro dos variables categóricas:
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color)) 
#más lindo
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# DISTRIBUCION DE UNA CONTINUA SEGUN FACTOR
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 1000)



