#1: Borrar todos los objetos 

rm(list = ls())

#2: Set working directory y leer los datos

setwd('C:/Users/jaume/Desktop/PRACTICA1')
datostrain <- read.table('p1_train.csv',header=TRUE,sep=';')

#3: Ver los datos y hacer una descriptiva con summary

View(datostrain)
head(datostrain)
nrow(datostrain)
dim(datostrain)
summary(datostrain)

#eliminar columna del ID

datostrain <- datostrain[ ,!colnames(datostrain) =="id"]

#4:transformacion a factor de ciertas variables

datostrain$year <- factor(datostrain$year)
datostrain$season <- factor(datostrain$season) 
datostrain$holiday <- factor(datostrain$holiday) 
datostrain$workingday <- factor(datostrain$workingday) 
datostrain$weather <- factor(datostrain$weather) 

#5 analizar comportamiento de count en funcion de hour, realizar intervalos segun comportamiento bajo criterio propio

plot(count~hour,datostrain)
axis(1,at=c(0:23),cex.axis=1)
with(datostrain,lines(lowess(count~hour),col=2))

dim(datostrain$hour)
boxplot(datostrain$count~datostrain$hour)
 
intervalos <- c(0,7,9,17,20,24)

datostrain$hour_interval <- cut(datostrain$hour,breaks = intervalos,right = FALSE)
boxplot(datostrain$count~datostrain$hour_interval)
with(datostrain,lines(lowess(count~hour_interval),col=2))

#6: descriptiva pairs variables numericas

pairs(datostrain[,c(2,7,8,9,10,11)])

#se observa que la variable atemp i temp presentan el mismo comportamiento para predecir la variable respuesta
#dichas variables tienen una evidente correlaci?n, en el siguiente apartado trataremos de cuantificar dicha corr.

#7: eliminaci?n de variables i #8 descriptiva bivariante

#construir dos modelos con la funcion lm, de las dos variables en funcion de la variable respuesta

mod.temp <- lm(count~temp,datostrain)
mod.atemp <- lm(count~atemp,datostrain)

summary(mod.temp)  #con una R^2= 0.1578
summary(mod.atemp) #con una R^2= 0.1537

# aqui se aprecia graficamente que ambas variables predicen de forma muy similar la variable respuesta,
#nos quedamos con la variable con mayor R2, ergo la variable temp
par(mfrow=c(1,2))
plot(count~temp,datostrain)
with(datostrain,lines(lowess(count~temp),col=2)) #suavizado de la linia
abline(mod.temp,col="red")
plot(count~atemp,datostrain)
with(datostrain,lines(lowess(count~atemp),col=2)) #suavizado de la linia 
abline(mod.atemp,col="red")

par(mfrow=c(1,2))
boxplot(count~temp,datostrain)
abline(mod.temp,col="red")
boxplot(count~atemp,datostrain)
abline(mod.atemp,col="red")

datostrain2 <- datostrain[ ,!colnames(datostrain) =="atemp"]

#boxplots

par(mfrow=c(1,5))

boxplot(datostrain$count~datostrain$year,main = 'Nº bicis vs años',xlab = 'year',ylab = 'count')
boxplot(datostrain$count~datostrain$season,main = 'Nº bicis vs estaciones',xlab = 'season',ylab = 'count')
boxplot(datostrain$count~datostrain$holiday,main = 'Nº bicis vs dias festivos',xlab = 'holiday',ylab = 'count')
boxplot(datostrain$count~datostrain$workingday,main = 'N bicis vs dias laborables',xlab = 'workingday',ylab = 'count')
boxplot(datostrain$count~datostrain$weather,main = 'Nº bicis vs condicion del tiempo',xlab = 'weather',ylab = 'count')

#9: ajustado del modelo lineal con todas las variables e interpretacion de los resultados

mod.var2 <- lm(count~.,datostrain2)   # Hacemos una ajuste lineal con todas las variables
mod.var2                         # Vemos los coeficientes de las variables
summary(mod.var2)
datostrain4 <- subset(datostrain2, select = -c(2))
mod.var5 <- lm(count~.,datostrain4)
summary(mod.var5)
#10 seleccion variables

mod.var3 <- step(mod.var2)
summary(mod.var3) #R2 = 0,6374
mod.var4 <- step(mod.var2, k = log(nrow(datostrain2)))
summary(mod.var4)

par(mfrow=c(2,2))                          # ventana para 4 gr�ficos
plot(mod.var3)                            #grafico para la validacion de las premisas 

install.packages("car")
library(car)
residualPlots(mod.var5)
residualPlots(mod.var8)


#11 colinealidad del paquete vif 

vif(mod.var5)

#12 validacion independencia
par(mfrow=c(1,1))
plot(residuals(mod.var5)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')
#13

par(mfrow=c(1,1))
countBC0 <- boxCox(mod.var5)
countBC0
countBC0$x[which.max(countBC0$y)]
lamb <- countBC0$x[which.max(countBC0$y)]  
datostrain4$countBC <- datostrain4$count^lamb

datostrain4$countlog <- log(datostrain4$count)

mod.var6 <- lm(countBC~year + season + weather + temp + humidity + hour_interval, datostrain4)
summary(mod.var6) #R2 = 0,7455
mod.var7 <- lm(countlog~year + season + weather + temp + humidity + hour_interval, datostrain4)
summary(mod.var7) #R2 = 0,7213

#15
par(mfrow=c(2,2)) 
plot(mod.var6)
par(mfrow=c(1,1))
plot(residuals(mod.var6)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')
#para la independencia no se observa ningun patron

#elimino las variables workingday y holiday
par(mfrow=c(1,2))
plot(count~holiday,datostrain4)
plot(count~workingday,datostrain4)

#con este grafico se observa que las variables holiday= si el dia era festivo es 1 si es laborable
#es 0 y workingday= si el dia era laborable 1 sino 0. Tienen el comportamiento inversamente proporcional
#es por eso que cuando ejecutamos el AIC el modelo no coje dichas variables

mod.var8 <- lm(countBC~year + season + weather +
                poly(temp,2) + poly(humidity,2) +
                hour_interval, datostrain4)
summary(mod.var8)

#R2 de 0,7467 amb polinomi a humidity i
#R2 de 0,748 amb polinomi a temp i humidity

influenceIndexPlot(mod.var8)           # Las observaciones 81, 147 tienen mucha influencia a posteriori. La 248 est� muy mal explicada por el modelo. 
                                      #La 65 y 81 tienen mucha influencia a priori.
par(mfrow=c(1,1))
influencePlot(mod.var8)  # Eliminar� las 2 m�s influyentes a posteriori (81 y 147) y la peor explicada (248)
?influencePlot
obs.rm <- c(6076,7677)
View(datostrain4[obs.rm,])
col.points <- rep(rgb(0,0,0,0.1),nrow(datostrain4))        # Vector de colores
col.points[obs.rm] <- 2:4                            # Colores distintos para las observaciones influyentes
pairs(datostrain4[,-10],col=col.points,pch=19,cex=0.8)     # Dibujo por pares de las observaciones influyentes
datos.rm <- datostrain4[-obs.rm ,]                         # Nos creamos un nuevo data.frame sin estas observaciones

#21

datostest <- read.table('p1_test.csv',header=TRUE,sep=';')
View(datostest)

datostest$year <- factor(datostest$year) 
datostest$season <- factor(datostest$season) 
datostest$weather <- factor(datostest$weather) 

# Creamos los mismos intervalos para las franjas horarias

intervals <- c(0,7,9,17,20,24)
datostest$hour_intervals <- cut(datostest$hour,breaks = intervals,right = FALSE)

# Eliminamos las columnas que no necesitamos para contruir el modelo y nos quedamos con las mismas columnas del modelo de entrenamiemnto 
# mas la columns id.

datostest <- subset(datostest, select = -c(3,5,6,9,11))

#predict

datostest$countBC_pred <- predict(mod.var8,datostest)

datostest$countBC_pred <- predict(mod.final,datos_test)   # Predicciones para los nuevos valores
datos_test$count_pred <- datos_test$countBC_pred^(1/lamb)  # Hacemos la inversa de la lambda para obtener el Nº de bicicletas alquiladas count_pred
datos_test$count_pred_redondeado <- round(datos_test$count_pred,digits = 0)  # Redondeamos el valor


