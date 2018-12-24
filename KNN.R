## KNN ##

# 1 Borrar datos

rm(list=ls())

# 2 Set working directory y Leer datos 

setwd('...')
datos <- read.table('p3_train.txt',header=TRUE,sep='\t')
datos2 <- datos[,543:563] 

# 3 Cargar librerias 

library(deldir)
library(kknn)
library(class)

# 4 División muestra entrenamiento y muestra test

p <- 0.7                 # Proporcion en muestra de entrenamiento, proporcion del 70% que sea de prueba
n <- dim(datos2)[1]           # numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p)) # selecci?n aleatoria de datos a test o prueba
train <- datos2[train.sel,] #tenemos muestra de prueba
test <- datos2[!train.sel,] #tenemos la de test

# 5

knn1 <- knn(train[,-ncol(datos2)], test=test[,-ncol(datos2)], cl=train$activity, k = 1)

t <- table(knn1,test$activity)

sum(diag(t))/sum(t)   # 0.9701592

# 6 mezclar datos y volver a ejecutar codigo

knn2 <- knn.cv(datos2[,-ncol(datos2)], cl=datos2$activity, k = 1)
t2 <- table(knn2,datos2$activity)
sum(diag(t2))/sum(t2)     # 0.9752 

# 7 opcion naive

max(prop.table(table(test$activity)))

# 8 numero de grupos

p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn <- knn(train[,-ncol(datos2)], test=test[,-ncol(datos2)], cl=train$activity, k = k)
  t <- table(knn,test$activity)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}

plot(K,p,pch=19,type='b')

cbind(K,p)













