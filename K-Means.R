## CLUSTERING NO SUPERVISADO ##

# 1 Borrar datos

rm(list=ls())

# 2 Set working directory y Leer datos 

setwd('...')
datos <- read.table('p3_train.txt',header=TRUE,sep='\t')
datos2 <- datos[,1:562]             # creamos nueva variable datos2 la cual no contiene la variable respuesta

# 3 Primer analisis de los datos con el metodo kmeans

km0 <- kmeans(datos2,centers=6)     # queremos n clusters --> centers = n
km0$cluster                         # indica la asignacion a los clusteres, como se reparten los elementos en los clusters
km0$centers                         # coordenadas de los centros de gravedad, podemos comparar las carateristicas de cada clusters
km0$totss                           # Inercia total
km0$withinss                        # Inercia intra para cada cluster
km0$tot.withinss                    # Inercia intra (global)
km0$betweenss                       # Inercia entre
km0$size                            # Tamaño de los clusteres
km0$iter                            # Iteraciones para converger

# 4 Calculo de la variabilidad explicada
with(km0,betweenss/totss) # V.explicada = 0.6423 con 4 clusteres  0.703997 con 6 clusteres

# 5 Hacemos un plot --> regla del codo

VE <- c()
for (k in 2:10){
  km <- kmeans(datos2,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")

# 6 Ncluster para saber numero de clusteres optimo

set.seed(12345)
ncluster <- NbClust(datos2[1:1000,], min.nc=2, max.nc=6, method="kmeans")
ncluster
barplot(table(ncluster$Best.n[1,]))
heatmap(scale(ncluster$All.index),Rowv=NA,Colv = NA)

# 7 Aplicamos kmeans

library(biganalytics)
library(bigmemory)

km1 <- kmeans(datos2,centers=6,nstart=10)
pairs(datos2,pch=19,cex=0.8,col=km1[[1]])

# 8 Reducción de dimensionalidad + plot de resultados clusteres

pr.comp <- princomp(datos2)
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km1$cluster)

#reduccio de dimensionalitat
#parteixes de 562 dimensions (variables)
#conservar la variabilitat
#components en la primera dimensio
#components en la segona dimensio
#se aprecian dos clusteres grandes, que se podrian llegar a diferenciar según el estado del sujeto (reposo/activo)

# 9 Randex index

t <- table(datos$activity,km1$cluster)
randIndex(t)

# 0.3304653 con 2 clusters
# 0.19 con 6 clusteres
# 0.24 con 4 clusteres
