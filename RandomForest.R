setwd("C:/Users/Admin/Desktop/Master/Estadistica/Practica Final")
datos <- read.table('p3_train.txt',header=TRUE,sep='\t') 

library(randomForest)
library(party)
library(e1071)

View(datos)
datos <- datos[,-562]                            # Eliminamos identificador

#No dividimos la muestra, en random forest no es necesario dividir

#-----------------------------------------------------------
#
# Random forest
#
#-----------------------------------------------------------
set.seed(12345)
rf.mod <- randomForest(activity~.,datos,importance=TRUE,ntree=50,do.trace=TRUE) 
#importance = TRUE para saber la importancia de las variables, el do.trace es para que vaya pintando los resultados
#para cada clase nos da el error de prediccion y el error de prediccion global (00B)
#la clase 1 nos da sobre un 0% tiene excelente prediccion
#la clase 2 y 3 da un 6%, precide muy bien. Para el resto la tasa de error son muy bajas del 1% al 3%
#en globlal acertamos un 96,6% <- 100 - columna 00B
rf.mod

############################################################
# Necesitamos mas arboles?
############################################################
plot(rf.mod, type="l")
#tipo "l" de lineas
#la linea negra, es el error de predicción a medida q vamos añadiendo mas arboles, llega hasta 50 pq son los que hemos hecho
#baja cuanto mas arboles
#el resto de lineas son la predicción de error para cada clase
#arriba tenemos las clases 2 y 3 que eran las peores de predecir
#si estas lineas salen planas, no necesitamos mas arboles, o que puedan bajar, la negra es la mas importante
#Tenemos que realizar mas arboles, solo tenemos una laying, que predice con 100% exactitud
#una opción es incrementar el numero de arboles, por ejemplo pasar hacer 100
############################################################
# Importancia de las variables --> Interpretabilidad
############################################################
varImpPlot(rf.mod)
#nos ordena las variables entre capacidad predictiva, la variable mas importane es la caracteristica 34
#segun el criterio de Gini es la variable 87, coinciden en la 373 y 414 que estan entre las tres primeras en los dos
#tenemos ordenadas las variables segun su importancia
#si el objetivo es predecir lo logico es coger el de la izquierda
#hay articulos que indican que Gini es mejor para predecir, el profe dice que el de la izquierda
v.imp0 <- importance(rf.mod)

##-- Importancia Global
ord <- order(v.imp0[,'MeanDecreaseAccuracy'],decreasing=TRUE)
#variables mas importantes al principio
v.imp0[ord,c('MeanDecreaseAccuracy','MeanDecreaseGini')]
#matriz que los ordena por el primer indicador, podemos ordenar por una clase en concreto

##-- Importancia para una clase concreta
ord <- order(v.imp0[,'laying'],decreasing=TRUE)
v.imp0[ord,c('laying')]
#para la varible uno, la caracteristica mas importante es la 548 y la menos la 419

ord <- order(v.imp0[,'sitting'],decreasing=TRUE)
v.imp0[ord,c('sitting')]
#para la varible uno, la caracteristica mas importante es la 414 y la menos la 326

ord <- order(v.imp0[,'standing'],decreasing=TRUE)
v.imp0[ord,c('standing')]
#para la varible uno, la caracteristica mas importante es la 373 y la menos la 514

ord <- order(v.imp0[,'walk'],decreasing=TRUE)
v.imp0[ord,c('walk')]
#para la varible uno, la caracteristica mas importante es la 229 y la menos la 296

ord <- order(v.imp0[,'walkdown'],decreasing=TRUE)
v.imp0[ord,c('walkdown')]
#para la varible uno, la caracteristica mas importante es la 119 y la menos la 516

ord <- order(v.imp0[,'walkup'],decreasing=TRUE)
v.imp0[ord,c('walkup')]
#para la varible uno, la caracteristica mas importante es la 193 y la menos la 205

############################################################
# "Tunear" el parametro mtry
############################################################
mtry.par <- tuneRF(datos[,1:561],datos$activity)
#le pasamos todas las variables predictoras y la variable respuesta
#nos va a dar el error de prediccion y nos quedaremos con aquel mas peque�o
#ha probado 23 arboles y una probabilidad de error de 3.48%, sigue probando...
#prueba con 12 y le da 3.52% de error de prediccion
#con 46 nos da 3.62% de error de prediccion
set.seed(12345)
rf.mod1 <- randomForest(activity~.,datos,importance=TRUE,ntree=50,do.trace=TRUE,mtry=23)
#probamos con 50 arboles, nos mejora la probabilidad de error a 3.24 % aprox, tenemos un 96.76% de acierto con 50 arboles
#tan solo hemos mejorado un 1%

#Vamos a mejorar el algoritmo poniendo 500 arboles
set.seed(12345)
rf.mod1 <- randomForest(activity~.,datos,importance=TRUE,ntree=500,do.trace=TRUE)
#probamos con 500 arboles, nos mejora la probabilidad de error a 2.76 % aprox, tenemos un 97.24% de acierto con 500 arboles
#tan solo hemos mejorado un 0.5%

set.seed(12345)
rf.mod1 <- randomForest(activity~.,datos,importance=TRUE,ntree=573,do.trace=TRUE)
#probamos con 573 arboles, nos mejora la probabilidad de error a 2.62 % aprox, tenemos un 97.38% de acierto con 573 arboles
#tan solo hemos mejorado un 0.1%

library(snow)
library(snowfall)

sfInit(parallel=TRUE, cpus=8)
#Le indicamos el numero de nucleos que queremos usar

sfLibrary(party)
sfLibrary(randomForest)
#los paquetes que queramos usar los tenemos que cargar con sfLibrary

set.seed(12345)
rf.mod1 <- sfLapply(randomForest(activity~.,datos,importance=TRUE,ntree=1000,do.trace=TRUE,mtry=10))
#probamos con 1000 arboles, y mtry a 10, nos mejora la probabilidad de error a 2.44 % aprox, tenemos un 97.56% de acierto
#tan solo hemos mejorado un 0.18%

set.seed(12345)
rf.mod1 <- randomForest(activity~.,datos,importance=TRUE,ntree=10000,do.trace=TRUE,mtry=10)
#probamos con 2000 arboles, y mtry a 10, nos mejora la probabilidad de error a 2.28 % aprox, tenemos un 97.72% de acierto
#tan solo hemos mejorado un 0.16%

set.seed(12345)
rf.mod1 <- tune.randomForest(activity~.,data=datos,importance=TRUE,ntree=1000,do.trace=TRUE,mtry=10)

sfStop()

#Vamos a realizar una validacion visual de las clases para ver si estan estabilizadas
plot(rf.mod1, type="l")



#Generamos fichero p3 para shiny
datosTest <- read.table('p3_test.txt',header=TRUE,sep='\t')
preds <- predict(rf.mod1, newdata = datosTest)

volcado <- data.frame(activity=preds) # preds: predicciones
write.table(volcado, 'p3.txt', row.names = FALSE, col.names = TRUE,sep='\t', quote = FALSE)
