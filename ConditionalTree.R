#####
## Arboles condicionales
#####

rm(list=ls())

# install.packages('randomForest')
# install.packages('party')
# install.packages('e1071')
library(randomForest)
library(party)
library(e1071)

setwd("C:/Users/Admin/Desktop/Master/Estadistica/Practica Final")
datos <- read.table('p3_train.txt',header=TRUE,sep='\t') 

View(datos)
d <- datos[,-562]                            # Eliminamos identificador
View(d)
p <- 0.7                                   # Proporcion en muestra de entrenamiento

n <- nrow(d)                               # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]


############################################################
# Visualizacion
############################################################
##-- Construirlo
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=3)) # Poco profundo para poder graficarlo
#profundidad del arbol maxima de 3
##-- Visualizarlo
windows()
plot(ct.mod,type='extended')
#ha tenido que evaluar todas las variables y ha decidido que lo optimo es dividir por la caracteristica 532 y el valor 0.59
#si es menor o igual a 0,59 ha encontrado que es la caracteristica 532 y asi sucesivamente, hasta que llega a la profundidad 3
#el nodo hoja mas a la derecha, con una probabilidad mas de 0,9 va a pertenecer a subir escaleras, clasifica 21 individuos
#el problema es que hay nodos hojas que pertenecen demasiados individuos, por ejemplo el nodo5 tiene 1239 y el ultimo solo 21
windows()
plot(ct.mod,type='simple')
#nos enseña las probabilidades de perteneces a cada grupo en cada nodo hoja


############################################################
# Evaluar capacidad predictiva
############################################################
#vamos a ponerle profunidad 0 y que haga hasta que crea
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=0)) # Profundidad maxima
#nos puede servir para ver si es bueno prediciendo, pero el grafico sera muy grande...
pred <- predict(ct.mod,test,type="response")                          # prediccion de la respuesta
(t <- table(pred,test$activity))                                        # tabla de predicciones vs respuesta real
#Matriz de confusion, nos interesa los valores en la diagonal.
#miramos la columna de laying(tumbado), vemos que la mayor esta en laying(tumbado) de fila con 296, mas miembros que en otros, tiene 1 en otros.

sum(diag(t))/sum(t)                                                   
#capacidad predictiva de un 0,888 -> 89%

#intentamos mejorar
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(mincriterion=0.90,minsplit=20,maxdepth=0))
pred <- predict(ct.mod,test,type="response") 
(t <- table(pred,test$activity)) 
sum(diag(t))/sum(t)
#0,888

#intentamos mejorar
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(mincriterion=0.95,minsplit=20,maxdepth=0,mtry=300))
pred <- predict(ct.mod,test,type="response") 
(t <- table(pred,test$activity)) 
sum(diag(t))/sum(t)
#0,901 indicando mtry=300, limitamos el numero de variables a escoger aleatoriamente, mejoramos en el modelo, pero
#dificilmente mejoraremos con datos Test.

#Generamos fichero p3 para shiny
datosTest <- read.table('p3_test.txt',header=TRUE,sep='\t')
preds <- predict(ct.mod, newdata = datosTest)

volcado <- data.frame(activity=preds) # preds: predicciones
View(volcado)
write.table(volcado, 'p3.txt', row.names = FALSE, col.names = TRUE,sep='\t', quote = FALSE)
