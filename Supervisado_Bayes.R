#############################################################Clustering supervisado

############################################################
#
# Naive Bayes
#
############################################################

############################################################
# Cargar paquetes
############################################################
# install.packages('e1071')
# install.packages('ineq')
library(e1071)
library(ineq)

#Leemos datos
setwd("C:/Users/Admin/Desktop/Master/Estadistica/Practica Final")
datos <- read.table('p3_train.txt',header=TRUE,sep='\t') 
View(datos)
#Inspeccionamos datos
dim(datos)                                 # Dimension
summary(datos)                             # Descriptiva
table(apply(apply(datos,2,is.na),2,sum))   # Tabla con numero de missings
#Obtenemos 0 missings y 563 varibales, perfecto

#####
# Validamos premisa de Bayes
# Las caracteristicas que determinan la presencia o ausencia de una determinada clase son independientes entre si
#####
#hemos de quitar la columna subject y activity, la penultima y ultima
cor.matrix <- cor(datos[,-c(length(datos)-1,length(datos))]) 
#Obtenemos todas las correlaciones en un unico vector
cor.num <- as.numeric(cor.matrix)

#vemos la tabla de correlaciones, entre -1 y -0,9 tenmos 518, entre -0.9 y -0.8 tenemos 1681, 
#como cuesta de ver nos hacemos un barplot
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2
t.cor

#Estan bastante relacionadas, todas las variables se mueven en correlaciones altras, estan entre 0 y 1
barplot(t.cor)

##############
# Dividir la muestra, seleccionaremos 2500 elementos de la muestra, total tenemos 5000
#############
d <- datos[,-c(length(datos)-1)]                            # Eliminamos identificador subject que es el penultima columna
p <- 0.7                                   # Proporcion en muestra de entrenamiento, hemos dicho que la mitad 2500 de 5000
n <- nrow(d)                               # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]


##################
# Aplicar bayes
##################

#aplicamos algoritmo con datos de entrenamiento Vs variable respuesta "activity"
nb <- naiveBayes(activity ~ ., train)
nb$levels
#Tenemos 6 niveles de clasificacion, laying, sitting, standing, walk, walkdown y walkup
nb$apriori
#Obtenemos la frecuencia de cada una de las clases, tenemos los individuos bien distribuidos, si tubieramos muchos indiviudos
#en una clase tendriamos problemas
nb$tables
#nos fijamos la caracteristica 557
#la media de la 557 en laying (tumbado) es de 0,88
#la media de la 557 en sitting (sentado) es de 0,89
#vemos que las medias son muy parecidas
#el ,2 es la desviacion tipo
#queremos que las medias sean diferentes entre ellas para que sean lo mas significativas posible

#######################
# Capacidad predictiva
#######################

##-- Global
preds <- predict(nb, newdata = test)
t <- table(preds, test$activity)
t
#vemos que la respuesta que mejor discrimina es laying, con un 287 por 10 en el resto, de pie tambien discrimina bastante bien
#tenemos 261 contra 13 en otros, igualmente nos interesa la diagonal

p.acierto <- sum(diag(t))/sum(t)
p.acierto
#obtenemos 80%, es un buen porcentaje para tener 6 clases, podemos intentar mejorarlo

##-- Se pueden pedir las probabilidades de cada clase para inspeccion visual
preds2 <- predict(nb, newdata = test,type = "raw") # el raw es la probabilidad respecto a los otros clusters
head(preds2)
#para cada individuo de la muestra test, la probabilidad de pertenecer a cada clase
#la primera tiene una probabilidad de 0% de pertenecer a laying y asi etc, cuanto mas se acerca a 1 mas opciones tiene,
#por ejemplo el 1 en sitting, nos da un 100% y un 0% en el resto

round(head(preds2),3) #redondeamos los porcentajes, se ven mejor
round(head(preds2,20),3) #primeros 20 individuos
#la calidad de la prediccion del algoritmo mejora con la probabilidad de los elementos a cada cluster
heatmap(scale(preds2[1:50,]),Rowv=NA,Colv=NA,col = cm.colors(256))
#abajo se ve que sitting mas probabilidades con el 1 y duda en el 16 pq pinta uno y otro casi
##-- Proporcion de acierto por clase
barplot(diag(prop.table(t,2)))
#porcentaje de acierto por cada variable respuesta, laying y standing las acierta bien
#tenemos subiendo y bajando escaleras y, andando, que las acierta en menor grado
#sentado tiene un porcetaje de 0,5, muy baja probabilidad de acierto

###############################
# Importancia de las variables, vamos a ver la importancia de las variables que tenemos, si podemos discriminar alguna
###############################

# Medir la importancia de las variables en funcion del indice Gini
graphics.off()                  # Cerrar ventanas que podamos tener, necesitamos generar muchas
nbt <- nb$tables                # Tablas con medias
var.imp <- c()                  # Gini

##-- Calcular indice y graficar distribucion
for (i in 1:(ncol(d)-1)){
  ##-- Crear ventana grafica cada 9 graficos
  if((i-1) %% 9==0){
    windows()
    par(mfrow=c(3,3))
  }
  
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])  # grafico
  var.imp[i] <- ineq(x)        # GINI calcula el indice GINI de desigualdad, en los mas predictoras los enseÃ±a     
}
#para cada una de las variables la media para cada clase, por ejemplo la variable 557
#con toda seguridad puede estar tumbado, sentado o de pie con valores altos.
#Esto es un problema por que las variables estan muy correlacionadas entre si y no consigue acertar para cada variable
#en que clase tendria mas posibilidades de estar.

##-- Los mas predictores
sel.pr <- order(var.imp, decreasing=TRUE)[1:9]
windows()
par(mfrow=c(3,3))
for (i in sel.pr){
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])
}
#nos dice la mas predictora para la variable respuesta
#obtenemos que solo 3 variables nos indican buenos valores de discriminación de prediccion

##-- Los menos predictores
sel.pr <- order(var.imp, decreasing=FALSE)[1:9]
windows()
par(mfrow=c(3,3))
for (i in sel.pr){
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])
}
#lo mismo pero la menos predictora para la variable respuesta
#obtenemos valores similares a las mas predictoras, estas validaciones visuales nos ayudan a confirmar que 
#la correlacion entre variables es alta y no podemos aplicar Naive Bayes para este caso.
graphics.off()
############################################################
# Intento de mejora: Quitar variables correlacionadas, premisa de independencia
############################################################
#Vamos a intentar quitar las variables que tienen mas correlación e intentaremos aplicar Bayes nuevamente

##-- Sistema 1: Correlaciones grandes
high.corr <- which(cor.matrix>0.8,arr.ind = TRUE) #maximo 1, quitamos > 0,8
t.high.corr <- sort(table(as.numeric(high.corr))/2,decreasing=TRUE)
t.high.corr
sel.rm <- names(t.high.corr)[t.high.corr>=2] 
train2 <- train[,-which(names(train) %in% paste0('feat',sel.rm))]

##-- Aplicar bayes nuevamente
nb2 <- naiveBayes(activity ~ ., train2, type="class")
preds <- predict(nb2, newdata = test)
t <- table(preds, test$activity)
p.acierto2 <- sum(diag(t))/sum(t)
p.acierto2

#Una vez retiradas variables mas correlacionadas obtenemos una predicción del 0,718


############################################################
# Premisa de independencia, validamos para ver si damos por bueno el 0,72 de la predicción
############################################################
#calculamos la correlación de todas las variables quitando el id y la ultima
#View(train2)
cor.matrix <- cor(train2[,-c(length(train2))])              # Matriz de correlaciones
cor.num <- as.numeric(cor.matrix)                           # Todas las correlaciones en un vector numerico
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2 # Categorazión de las correlaciones en intervalos de 0.1
t.cor
#vemos la tabla de correlaciones, entre -0.1 y 0,2 tenemos un gran numero de variables, hemos conseguido que las variables se muevan en 
#correlaciones bajas, damos por bueno el valor predictivo de 0,72
barplot(t.cor)



#######
# Predicciones
#######

datosTest <- read.table('p3_test.txt',header=TRUE,sep='\t')

preds <- predict(nb2, newdata = datosTest)

volcado <- data.frame(activity=preds) # preds: predicciones
View(volcado)
write.table(volcado, 'p3.txt', row.names = FALSE, col.names = TRUE,sep='\t', quote = FALSE)



