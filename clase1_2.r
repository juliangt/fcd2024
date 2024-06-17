# PARTE2-ejemplos de clase 1:inferencia
#  autor: SP
# fecha: 2024/06/10
#------------------
#setwd("C:/Users/Silvia/...............") recordar setear el lugar de trabajo!
library(readxl)
library(ggplot2)
library(dplyr)
#library(PASWR)
library(MASS)

### EJEMPLO 2: datos mtcars

#PRUEBA PARA UNA MEDIA con varianza desconocida: t de Student.
data(mtcars) #de la biblioteca MASS
summary(mtcars)
View(mtcars)
mtcars <- rename(mtcars, rend = mpg) #cambio el nombre de la variable a "rend" por rendimiento
mtcars$am<- ifelse(mtcars$am=="0","automática","manual")
glimpse(mtcars)

#queremos probar hipótesis acerca del rendimiento medio.
#Verificamos supuestos:
# pruebas de normalidad
shapiro.test(mtcars$rend) #para muestras chicas es preferible a KS
library("tseries")
jarque.bera.test(mtcars$rend)
#library("nortest")
#lillie.test(mtcars$rend) #KS con corrección de Lilliefors, no es tan potente como los otros

par(mfrow=c(1, 2))
require(car)  # para qqPlot, más lindos
qqPlot(mtcars$rend, pch=19, id=FALSE,
       main='QQplot para rendimiento',
       xlab='Cuantiles teóricos',
       ylab='Cuantiles muestrales')
hist(mtcars$rend, freq=TRUE,
     main='Histograma',
     xlab='rendimiento',
     ylab='Frecuencia')

par(mfrow=c(1,1))

boxplot(mtcars$rend, col = "blue")
abline(h=20, col="blue")

#test bilateral de media!=20
t.test(x=mtcars$rend,mu=20,alternative = c("two.sided"),conf.level=0.99) #alternative = c("two.sided", "less", "greater")

#test unilateral H1:media>15
t.test(x=mtcars$rend,mu=15,alternative = c("greater"),conf.level=0.99)

# PRUEBA DE COMPARACION DE 2 MEDIAS INDEPENDIENTES
#queremos probar hipótesis acerca del rendimiento medio según la transmisión.
mtcars$am<-as.factor(mtcars$am) #para que lo trate como factor 
ggplot(mtcars, aes(factor(am), rend))+  
  geom_boxplot(aes(fill=am))+labs(title = "Boxplot de rendimiento")+xlab("transmisión")

#Verificamos supuestos:
#1.- pruebas de normalidad en cada grupo
shapiro.test(mtcars$ren[mtcars$am=="automática"])
shapiro.test(mtcars$ren[mtcars$am=="manual"])

#2.- prueba de homogeneidad de varianzas
#para ver si las varianzas son iguales o no:
var.test(rend ~ am, data=mtcars) ##si no se explicita alternative= c("two.sided")
#da un pvalor=0.06691 entonces....

#otra forma, sirve para mas de 2 grupos:
library(car)
car::leveneTest(rend ~ am, data=mtcars)

#3.- independencia
#podemos suponerla aquí por el tipo de datos

#prueba de MEDIAS: = contra distinto, menor, mayor...
# la sintaxis es: t.test(x, y = NULL,alternative = c("two.sided", "less", "greater"),mu = 0, 
#paired = FALSE, var.equal = FALSE,conf.level = 0.95, ...)
#prueba de mu_aut<mu_manual:
t.test(rend ~ am, alternative = c("less"),data=mtcars) #esto hace la prueba Welsh directamente
#otra forma, para explicitar si considero igual varianza o no:
t.test(rend ~ am, data=mtcars,alternative = c("less"),var.equal = F) #cambiar para probar con/sin suponer varianzas iguales


### PRUEBA DE COMPARACION DE MEDIAS APAREADAS #####
#Ejemplo 4: peso antes de la dieta y después de la dieta 
antes <- c(195, 213, 147, 201, 187, 210, 115, 146, 194, 210)
despues <- c(187, 195, 150, 205, 185, 197, 111, 131, 178, 215)
dif <- antes - despues  #verlos!
summary(dif)
#son pocas muestras, pero podemos ver el QQplot:
qqnorm(dif, pch=19, main='')
qqline(dif)
#veamos la normalidad (pocas muestras entonces Shapiro)
shapiro.test(dif)
#El test de medias sería:
t.test(x=antes, y=despues, alternative="greater", mu=0, 
       paired=TRUE, conf.level=0.95)

############# ANOVA ##############
# Ejemplo 5: comparar medias de presión arterial según ECG
##cardio<-read.table("Cardio_UCI.csv",sep=";",dec=",",header=T)
library(readxl)
cardio<-read_xlsx("Cardio_UCI.xlsx")
View(cardio)
summary(cardio)
cardio<-as.data.frame(cardio)
cardio$restecg<-as.factor(cardio$restecg)
levels(restecg)
cardio$sexo<-as.factor(cardio$sexo)
attach(cardio)
levels(restecg)
#para ver estructura de los datos (usa library(dplyr))
glimpse(datos)

#si las categorías no están en el orden que queremos podemos hacer:
#restecg<- ordered(restecg,levels = c("2", "1", "0")) 


#para ver el comportamiento
boxplot(trestbps ~ restecg, #data = cardio,
        xlab = "Resultados ECG", ylab = "presion arterial en reposo",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))



#Queremos ver si existe alguna diferencia significativa entre las medias de trespbs: ANOVA

#MODELO ANOVA: 
mod.aov <- aov(trestbps ~ restecg, data = cardio)
summary(mod.aov)

#supuestos:
#1- normalidad
# pruebaShapiro-Wilk
shapiro.test(x = aov_residuals ) #la Ho es normalidad

#2- homogeneidad de varianzas
library(car)
car::leveneTest(trestbps ~ restecg, data = cardio)

#3- independencia

#engeneral ya el modelo ANOVA da los gráficos necesarios:
par(mfrow=c(2, 2))
plot(mod.aov)
dev.off() #otra forma de resetear la disposición de plots
#par(mfrow=c(1, 1))

#OBS:
#si no hay homog se puede hacer el test unidireccional de Welch que no lo asume
oneway.test(trestbps ~ restecg, data = cardio)

#A POSTERIORI: SI EL ANOVA INDICA QUE EXISTEN DIFERENCIAS, quiero ver entre cuáles hay diferencias:
TukeyHSD(mod.aov) #testea diferencias de a pares y muestra IC
plot(TukeyHSD(mod.aov))

pairwise.t.test(trestbps, restecg, p.adjust.method = "bonferroni")


### ANOVA de 2 VÍAS #####
library(gridExtra)
#veamos el comportamiento de trestbps según dos factores
p1 <- ggplot(data = cardio, aes(x = restecg, y = colesterol)) + 
  geom_boxplot() + theme_bw()
p2 <- ggplot(data = cardio, aes(x = sexo, y = colesterol)) +
  geom_boxplot() + theme_bw()
p3 <- ggplot(data = cardio, aes(x = restecg, y = colesterol, colour = sexo)) +
  geom_boxplot() + theme_bw()

grid.arrange(p1, p2, ncol = 2)
p3

#para calcular medias en grupos
with(data = cardio,expr = tapply(colesterol, restecg, mean))

##Graficos para ver la interacción entre los factores
ggplot(data = cardio, aes(x = restecg, y = colesterol, colour = sexo,
                         group = sexo)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (colesterol)') +
  theme_bw()
# y en el otro sentido..
ggplot(data = cardio, aes(x = sexo, y = colesterol, colour = restecg,
                          group = restecg)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (colesterol)') +
  theme_bw()


## ajustamos modelo ANOVA
anova2 <- aov(trestbps ~ restecg * sexo, data = cardio)
summary(anova2) 

#no da significativa la interacción! probemos el modelo aditivo
anova2ad <- aov(trestbps ~ restecg +sexo, data = cardio)
summary(anova2ad)



