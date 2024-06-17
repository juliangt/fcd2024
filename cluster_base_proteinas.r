library(readr)
library(psych)
library(ggplot2)

#Leer la base de datos "proteinas"

proteinas <- read_delim("C:/Users/Diego/Documents/Clases - Apuntes - Scripts R/Componentes principales/Proteinas.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)

proteinas <- as.data.frame(proteinas)
rownames(proteinas) <- proteinas$Country
proteinas <- proteinas[-1]

pc_prot <- prcomp(proteinas, center = TRUE, scale. = TRUE)
summary(pc_prot)
pc_prot$x

proteinas <- cbind(proteinas, pc_prot$x[,c(1,2)])

ggplot(proteinas, aes(x = PC1, y = PC2)) + 
        geom_point(color = "blue", size = 3) +
        geom_text(label=row.names(proteinas), 
        nudge_x =0.55, nudge_y = 0.55, 
        check_overlap = T) +
        theme_bw()

#Cluster
      
clu <- kmeans(proteinas[c(10, 11)], 4)
clu$centers
clu$totss
clu$size
clu$iter

proteinas <- cbind(proteinas, cluster = as.factor(clu$cluster))

ggplot(proteinas, aes(PC1, PC2, fill = cluster)) +
       geom_point(shape = 21, col = "black") +
       geom_text(label=row.names(proteinas), 
                 nudge_x =0.15, nudge_y = 0.15, 
                 check_overlap = T)

#Calculamos los promedios para comparar los grupos

promedios <- proteinas %>% group_by(cluster) %>% 
             summarise_at(vars(colnames(proteinas)[1:9]), mean)

lista <- list()
clusters <- c("cluster 1", "cluster 2", "cluster 3", "cluster 4")

for (i in 1:4) {
  
  lista[[clusters[i]]][[1]] <- row.names(subset(proteinas, cluster == i))
  lista[[clusters[i]]][[2]] <- promedios[i,]
    
}


#Hacemos lo mismo pero con todas las variables

clu <- kmeans(proteinas[2:10], 4)

prot_clu <- cbind(proteinas, cluster2 = as.factor(clu$cluster))

ggplot(prot_clu, aes(PC1, PC2, fill = cluster2)) +
  stat_ellipse(geon= "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

promedios2 <- prot_clu %>% group_by(cluster2) %>% 
              summarise_at(vars(colnames(proteinas)[1:9]), mean)

#Usamos cluster jerárquico


d <- dist(proteinas[2:10], method = "euclidean") # distance matrix
cluh <- hclust(d, method="complete") 
plot(cluh) # display dendrogram
groups <- cutree(cluh, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the clusters 
rect.hclust(cluh, k=4, border="red")

#Cluster jerárquico para variables

grupos_var <- hclustvar(proteinas[, 1:8]) 
plot(grupos_var)




