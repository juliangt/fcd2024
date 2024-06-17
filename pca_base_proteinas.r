library(readr)
library(ggcorrplot)
library(factoextra)
library(psych)
library(ggbiplot)
library(ggrepel)

#---Ejemplo - Cálculo de autovalores y autovectores----

datos <- data.frame(X1 = c(3.37, 1.82, 0.43, 1.73, 2.96, 
                        2.85, 2.41, 2.34, 2.59, 3.87),
                    X2 = c(3.97, 1.05, 0.19, 2.52, 2.71,
                        3.97, 3.59, 3.98, 2.78, 3.48))

#Diagrama de dispersión para ver si las variables están
#relacionadas
plot(datos$X1, datos$X2)

#Centramos los datos
datos_centrados <- data.frame(X1 = datos$X1 - mean(datos$X1),
                              X2 = datos$X2 - mean(datos$X2))

#Calculamos los autovalores y autovectores de la matriz
#de covarianzas

matriz_cov <- cov(datos_centrados)
matriz_corr <- cor(datos_centrados)

auto <- eigen(matriz_cov)
autovalores <- auto$values
autovectores <- auto$vectors

t_autovectores <- t(autovectores)
t_datos_centrados <- t(datos_centrados)

pc_scores <- t(t_autovectores %*% t_datos_centrados)
plot(pc_scores[,1], pc_scores[,2])

colnames(pc_scores) <- c("PC1", "PC2")

datos_recuperados <- t(autovectores %*% t(pc_scores))
datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(datos$X1)
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(datos$X2)

#---Ejemplo PCA -----------

#Leer la base de datos "proteinas"
proteinas <- read_delim("C:/Users/Diego/Documents/Clases - Apuntes - Scripts R/Componentes principales/Proteinas.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)

#Observamos la base de datos
View(proteinas)

#Convertimos en un dataframe
proteinas <- as.data.frame(proteinas)

#Renombro las filas con los nombres de los países
rownames(proteinas) <- proteinas$Country
proteinas <- proteinas[-1]

#Calculamos y observamos las correlaciones entre las variables
corr <- cor(proteinas)
ggcorrplot(corr)

#Calculamos las componente principales a partir de la matriz
#de covarianzas
pc_prot <- prcomp(proteinas, center = TRUE)

#Analizamos los resultados
summary(pc_prot)

#Comparamos las suma de las varianzas de las variables 
#originales con la suma de los autovalores. 
covarianzas <- cov(proteinas)
tr(covarianzas)
sum((pc_prot$sdev)^2)

#Ahora calculamos las componente principales a partir de la matriz
#de covarianzas
pc_prot <- prcomp(proteinas, center = TRUE, scale. = TRUE)

#Analizamos los resultados
summary(pc_prot)

#Observamos las cargas (loadings) de las variables en cada 
#componente
pc_prot$rotation

#Observamos como se ubican los países en los dos componentes
#principales extraídos. 
biplot(pc_prot, scale = 0, cex = 0.6, col = c("blue4", "brown3")) 
  
proteinas <- cbind(proteinas, pc_prot$x)

ggplot(proteinas, aes(x = PC1, y = PC2)) + 
  geom_point(color = "blue", size = 3) +
  geom_text(label=row.names(proteinas), 
            nudge_x =0.55, nudge_y = 0.55, 
            check_overlap = T) +
  theme_bw()

