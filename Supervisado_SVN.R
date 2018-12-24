#-----------------------------------------------------------
#
# SVM
#
#-----------------------------------------------------------
library(e1071)
library(snowfall)
library(snow)
library(parallel)
# 1 Borrar datos y setwd

rm(list=ls())

setwd("C:/Users/Admin/Desktop/Master/Estadistica/Practica Final")
datos <- read.table('p3_train.txt',header=TRUE,sep='\t') 

#En vez de hacer partición, se cogen directamente todos los datos.

detectCores(all.tests = FALSE, logical = TRUE)
set.seed(12345)
n0 <- 5000
datos <- datos[,-562] 

#AJUSTE MODELO
set.seed(12345)
mod.svm <- svm(activity~.,data = datos,cost=1)
pr <- predict(mod.svm,datos)
t <- table(pr,datos$activity)
t
sum(diag(t))/sum(t)


mod.tune <- tune(svm,activity~.,data=datos,kernel=c('linear','polynomial','radial','sigmoid'),ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune)
mod.tune$best.parameters
mod.tune$best.model

mod.tune <- tune(svm,activity~.,data=train,kernel=c('linear','polynomial','radial','sigmoid'),ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100))
                 ,tunecontrol = tune.control(sampling = "boot"))
summary(mod.tune)
mod.tune$best.parameters
mod.tune$best.model

#Generamos fichero p3 para shiny
datosTest <- read.table('p3_test.txt',header=TRUE,sep='\t')
preds <- predict(mod.tune$best.model, newdata = datosTest)

volcado <- data.frame(activity=preds) 
write.table(volcado, 'p3.txt', row.names = FALSE, col.names = TRUE,sep='\t', quote = FALSE)

#98.45 a shiny