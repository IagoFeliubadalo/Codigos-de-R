#### PARTE 1: CONSTRUCCION DEL MODELO ####

#1 Borrar datos memoria

rm(list=ls())

#2 Leer datos 

setwd('D:/MBD/ESTADISTICA/PRACTICA 2')                            
datos <- read.table('p2_train.csv',header=TRUE,sep=';') 

#3 visualizacion de datos, descriptiva y eliminación columna "id"

View(datos)
summary(datos)
datos <- datos[ ,!colnames(datos) =="id"]

#4 Eliminación variable "pdays" tiene missings codificados con 999

summary(datos$pdays)
datos <- datos[ ,!colnames(datos) =="pdays"]

#5 Descriptiva bivariante v. categóricas

sapply(datos,class)
var.cat <- which(sapply(datos,class)=="factor" & names(datos)!="y")

#6 Mosaic - plot
windows()
par(mfrow=c(4,3))
for(vc in var.cat)  mosaicplot(datos[,vc]~datos$y,main=names(datos)[vc],col=2:3,las=1)
plot(datos$day_of_week)
summary(datos$day_of_week)

#7 Crear variable job 2

datos$job2 <- ifelse(datos$job == 'admin.' | datos$job == 'blue-collar' | datos$job == 'technician'| datos$job == 'management'
                     | datos$job == 'entrepreneur'| datos$job == 'self-employed' | datos$job == 'services' | datos$job == 'housemaid',1,0)
datos$job2 <- factor(datos$job2)
datos <- datos[ ,!colnames(datos) =="job"]

#8 Crear variable month2

datos$month2 <- ifelse(datos$month == 'apr' | datos$month == 'aug' | datos$month == 'jul' | datos$month == 'jun' | datos$month == 'may' 
                       | datos$month == 'nov',1,0)
datos$month2 <- factor(datos$month2)
datos <- datos[ ,!colnames(datos) =="month"]

#9 Crear variable education2

datos$education2 <- ifelse(datos$education == 'professional.course' | datos$education == 'university.degree', "categoria", as.character(datos$education))
datos$education2 <- ifelse(datos$education == 'professional.course' | datos$education == 'university.degree' ,1,0)
datos$education2 <- factor(datos$education2)
datos <- datos[ ,!colnames(datos) =="education"]

mod.glm0 <- glm(datos$y~datos$education,datos,family=binomial)   # estimacion del modelo
options(scipen=5)
summary(mod.glm0)

mod.glm00 <- glm(datos$y~datos$education2,datos,family=binomial)   # estimacion del modelo
options(scipen=5)
summary(mod.glm00)

#10 Crear variable default2

datos$default2 <- ifelse(datos$default == 'unknown'| datos$default == 'yes',1,0)
datos$default2 <- factor(datos$default2)
datos <- datos[ ,!colnames(datos) =="default"]

#retocar variable week day

datos$day_of_week2 <- ifelse(datos$day_of_week == 'mon',1,0)
datos$day_of_week2 <- factor(datos$day_of_week2)
datos <- datos[ ,!colnames(datos) =="day_of_week"]

#marital

datos$marital2 <- ifelse(datos$marital == 'married' ,1,0)
datos$marital2 <- factor(datos$marital2)
datos <- datos[ ,!colnames(datos) =="marital"]

#loan

datos$loan2 <- ifelse(datos$loan == 'no' ,1,0)
datos$loan2 <- factor(datos$loan2)
datos <- datos[ ,!colnames(datos) =="loan"]

#11 Descriptiva bivariante v.numéricas

var.num <- which(sapply(datos,class) %in% c("numeric","integer"))

#12 Desnsidades
windows()
par(mfrow=c(4,3))
for(vn in var.num) cdplot(datos$y~datos[,vn],main=names(datos)[vn],n=512)

#13 Modelo todas las variables

mod.glm1 <- glm(y~.,datos,family=binomial)    
options(scipen=5)
summary(mod.glm1) 

#14 Selección variables automatica

apply(apply(datos,2,is.na),2,sum)  
mod.glm2 <- step(mod.glm1)
summary(mod.glm2)

#16 VALIDACIO amb test de hosmer and lemeshow 

#install.packages('ResourceSelection')
library(ResourceSelection)
hoslem.test(mod.glm2$y, fitted(mod.glm2))

#com em dona un p-valor menor a 0.05 hi ha evidencia de que el model no s'ajusta bé

# VALIDACIO GRAFICA x inspeccion visual

#17 División valores predichos

br <- quantile(fitted(mod.glm2),seq(0,1,0.1))                                # Se crean los puntos de corte para los intervalos (br) de las probabilidades predichas

#18 Predichos vs esperados de cada cuantil

int <- cut(fitted(mod.glm2),br)                                              # Se crea una variable con el intervalo al que pertenece cada individuo
obs <- tapply(mod.glm2$y,int,sum)                                            # Los pagos observados en cada intervalo
exp <- tapply(fitted(mod.glm2),int,sum)                                      # Los pagos esperados en cada intervalo  
plot(1:10+0.05,exp,type='h',xlab="Intervalos",ylab="Frecuencias",lwd=2)      # Grafico de los pagos esperados
lines(1:10-0.05,obs,type='h',col=2,lwd=2)                                    # Se añade los pagos observados
legend("topleft",c("Compran - esperados", "Compran - observados"),lwd=2,col=1:2) # Se añade una leyenda


#19 Calcula el OR de contactar al telefono (contact) fijo respecto a hacerlo por movil

exp(mod.glm2$coef["contacttelephone"])   # 0.3900591
exp(-mod.glm2$coef["contacttelephone"])  # 2.563714

#20 Calcula el OR de una campaña adicional (campaign) y interpretalo

exp(10*mod.glm2$coef["campaign"]) #hacer una campaña adicional aumenta un 0,6083305 respecto no hacer campaña adicional

#21 Calcula los intervalos de confianza del 95% para todos los ORs
# Cual es la variable mas relevante a la hora de tener exito en la venta?
# Pista: confint (ten paciencia--> puede tardar)
?confint
IC <- confint(mod.glm2,level = 0.95)             # Intervalos de  confianza para los coeficientes
round(exp(IC),2)                    # Intervalos de confianza para los ORs redondeados a 2 decimales

#22 Estima las probabilidades predichas por el modelo para todos los individuos

pr <- predict(mod.glm2,datos,type="response")
pr

#23 Probabilidad maxima y minima

pos.max <- which.max(pr)        # posicion del individuo con mayor probabilidad de pagar = 73
pr[pos.max]                     # probabilidad de dicho individuo pr = 0.8624621
datos$y[pos.max]                # pago? resouesta = yes

pos.min <- which.min(pr)        # posicion del individuo con menor probabilidad de pagar = 5500
pr[pos.min]                     # probabilidad de dicho individuo pr = 0.00365891
datos$y[pos.min]                # pago? respuesta = no

boxplot(pr~y,datos)

#24 Escoge un individuo al azar y mira la probabilidad predicha y si realmente escogio el producto 
# Pista: Igual que el anterior, pero en vez de fijar la posici???n del individuo con which.max o which.min, escoge una al azar

pos.random <- 18500
pr[pos.random]                  # pr = 0.089
datos$y[pos.random]             # respuesta = NO 

#25

#install.packages('AUC')
library(AUC)

# 26

roc.curve <- roc(pr,datos$y)
plot(roc.curve)
auc(roc.curve)        # AUC = 0.7848216

#27A partir de ahora la empresa decide unicamente llamar a aquellos que tengan una probabilidad
#mayor de 0.2 de adquirir el producto. Que porcentaje se espera de las llamadas que adquieran el producto?
#Que porcentaje de los que no llamamos hubiesen adquirido el producto?

##-- Sensibilidad y especificidad para un punto de corte concreto
s <- sensitivity(pr,datos$y)
e <- specificity(pr,datos$y)
a <- accuracy(pr,datos$y)
df <- data.frame(cutpoints=s$cutoffs,sens=s$measure,esp=e$measure,acc=a$measure)
View(round(df,3))

##-- Escoger un punto de corte --> Matriz de confusion

datos$llamar <- ifelse(pr>0.2,'si','no')  # Llamo a aquellos con un probabilidad predicha de adquirir el producto superior a 0.2
with(datos,table(llamar,y))
with(datos,round(100*prop.table(table(llamar,y),1),1))

library(PresenceAbsence)
df.calibra <- data.frame(plotID=1:nrow(datos), Observed = as.numeric(datos$y)-1  , Predicted1 = pr)
calibration.plot(df.calibra, N.bins = 10,ylab='Observed probabilities')

#28 

datostest <- read.table('p2_test.csv',header=TRUE,sep=';') 
View(datostest)

#29 

datostest <- datostest[ ,!colnames(datostest) =="pdays"]

datostest$job2 <- ifelse(datostest$job == 'admin.' | datostest$job == 'blue-collar' | datostest$job == 'technician'| datostest$job == 'management'
                         | datostest$job == 'entrepreneur'| datostest$job == 'self-employed' | datostest$job == 'services' | datostest$job == 'housemaid',1,0)
datostest$job2 <- factor(datostest$job2)
datostest <- datostest[ ,!colnames(datostest) =="job"]

datostest$month2 <- ifelse(datostest$month == 'apr' | datostest$month == 'aug' | datostest$month == 'jul' | datostest$month == 'jun' | datostest$month == 'may' 
                           | datostest$month == 'nov',1,0)
datostest$month2 <- factor(datostest$month2)
datostest <- datostest[ ,!colnames(datostest) =="month"]

datostest$education2 <- ifelse(datostest$education == 'professional.course' | datostest$education == 'university.degree' ,1,0)
datostest$education2 <- factor(datostest$education2)
datostest <- datostest[ ,!colnames(datostest) =="education"]

datostest$default2 <- ifelse(datostest$default == 'unknown'| datostest$default == 'yes',1,0)
datostest$default2 <- factor(datostest$default2)
datostest <- datostest[ ,!colnames(datostest) =="default"]

datostest$day_of_week2 <- ifelse(datostest$day_of_week == 'mon',1,0)
datostest$day_of_week2 <- factor(datostest$day_of_week2)
datostest <- datostest[ ,!colnames(datostest) =="day_of_week"]

datos$marital2 <- ifelse(datos$marital == 'married' ,1,0)
datos$marital2 <- factor(datos$marital2)
datos <- datos[ ,!colnames(datos) =="marital"]

datos$loan2 <- ifelse(datos$loan == 'no' ,1,0)
datos$loan2 <- factor(datos$loan2)
datos <- datos[ ,!colnames(datos) =="loan"]

#30

datostest$y <- predict(mod.glm2,datostest)
datosfinal <- subset(datostest,select = -c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
write.table(datosfinal,"p2.txt",quote=FALSE,row.names = FALSE,col.names = FALSE,sep=",")
