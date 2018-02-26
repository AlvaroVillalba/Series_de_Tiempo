################# Taller 1 #################
######### AGREGADOS MONETARIOS #############

library(rugarch); 
library(forecast); 
library(urca)
library(readxl)
library(tseries)
library(forecast)

setwd("D:/Documents/MEA/ECONOMETRIA/TALLER 1")
###datos mensuales de los agregados monetaros M1, M2 y M3 desde enero de 1984 hasta julio de 2017###

Agregados<-read_xlsx("Agregados monetarios.xlsx")


############## 1.	(10 puntos) Importe los datos a R y convierta el data frame a ts########
##########################################################################################

#Cambiar objeto data frame a series de tiempo
Agregados.ts<-ts(Agregados, frequency=12, start=c(1984, 1))
View(Agregados.ts)
class(Agregados.ts)

############# 2.	(10 puntos) Grafique las serie M1, M2 y M3, en un panel de gráficos ######## 
############# Interprete brevemente los resultados.###########################################
##############################################################################################
par(mfrow=c(1, 3))
plot.ts(Agregados.ts[,2], main="M1 mensual",
        ylab="M1", xlab="Fecha", col="blue", las=2) 
plot.ts(Agregados.ts[,3], main="M2 mensual",
        ylab="M1", xlab="Fecha", col="blue", las=2) 
plot.ts(Agregados.ts[,4], main="M3 mensual",
        ylab="M1", xlab="Fecha", col="blue", las=2) 

#### Antes de continuar observemos como con la recomendación del TalLer propuesto de la 
#### primera diferencia del logaritmo natural de la serie M1 pierde la tendencia y 
#### se estabiliza un poco, es decir se elimina la tendencia y la vuelve estacionaria 
plot(log(Agregados.ts[,2]))
plot(diff(log(Agregados.ts[,2])))

###################### 3.	(40 puntos) Para la serie mensual de M1:#########################
###########################################################################################

### 3a.Calcule las pruebas de raíz unitaria Dickey-Fuller Aumentada, Phllips-Perron y KPSS 

# Dickey fuller aumentada
adf.test(Agregados.ts[,2], k=12) 

# perron
pp.test(Agregados.ts[,2])

# kpss
kpss.test(Agregados.ts[,2], null = "Trend", lshort = T)


### 3b.	Costruya los correlogramas de la ACF y PACF para la serie M1 en niveles.
par(mfrow=c(1, 2))
acf(Agregados.ts[,2], main="ACF M1", lag.max=100)
pacf(Agregados.ts[,2], main="PACF M1", lag.max=100)

### Podemos analizar cuantas veces tendremos que diferenciar la Serie por medio de los Test
### Dickey-Fuller, Phillips-Perron y KPSS, observemos 
ndiffs(Agregados.ts, test = c("kpss"))
ndiffs(Agregados.ts, test = c("adf"))
ndiffs(Agregados.ts, test = c("pp"))
### Como observamos se recomienda diferenciar la Serie una vez 
### La conclusión anterior tambien la podemos ver en los Correlogramas obteniods de la serie en niveles

### 3c.	Costruya los correlogramas de la ACF y PACF para la primera 
### diferencia del logaritmo natural del M1. Calcule las pruebas  

par(mfrow=c(1, 2))
acf(na.exclude(diff(log(Agregados.ts[,2]))), main="ACF LogdiffM1", lag.max=100)
pacf(na.exclude(diff(log(Agregados.ts[,2]))), main="PACF LogdiffM1", lag.max=100)

### d.	Con los correlogramas de la primera diferencia del logaritmo naturaldel M1, identifique tres 
###     potenciales modelos y realice las estimaciones de cada uno de estos.

### e.	Realice el diagnóstico para los residuos de los tres modelos planteados 
###     en el numeral anterior. Utilice estos resultados para seleccionar el "mejor modelo"

##################
#####Modelo 1#####
##################

modelo1<-arima(log(Agregados.ts[, 2]), order = c(5,1,0), season= c(1,0,0))
summary(modelo1)
residuo1<-modelo1$res
plot.ts(residuo1)
par(mfrow=c(1, 2))
acf(residuo1, main="ACF Residuo1 M1", lag.max=55)
pacf(residuo1, main="PACF Residuo1 M1", lag.max=55)
### Test de Ljung-Box
Box.test(residuo1, lag=100, type = "Ljung-Box")

### Modelo 2
modelo2<-arima(log(Agregados.ts[, 2]), order = c(5, 1, 0), season= c(1, 0, 1 ))
summary(modelo2)
residuo2<-modelo2$res
plot.ts(residuo2)
par(mfrow=c(1, 2))
acf(residuo2, main="ACF Residuo2 M1", lag.max=100)
pacf(residuo2, main="PACF Residuo2 M1", lag.max=100)
### Test de Ljung-Box
Box.test(residuo2, lag=100, type = "Ljung-Box")

### Modelo 3
modelo3<-arima(log(Agregados.ts[, 2]), order = c(5, 1, 5), season= c(1, 0, 1))
summary(modelo3)
residuo3<-modelo3$res
plot.ts(residuo3)
par(mfrow=c(1, 2))
acf(residuo3, main="ACF Residuo3 M1", lag.max=100)
pacf(residuo3, main="PACF Residuo3 M1", lag.max=100)
### Test de Ljung-Box
Box.test(residuo3, lag=100, type = "Ljung-Box")

### 3f.	Con la función auto.arima del paquete forecast, estime el modelo para la primera 
### diferencia del logaritmo natural del M1, realice el diagnóstico de los residuos 
### del modelo. Compare los resultados

modelo4<-auto.arima(log(Agregados.ts[, 2]), d=1)
summary(modelo4)
residuo4<-modelo4$res

plot.ts(residuo4)
par(mfrow=c(1, 2))
acf(residuo4, main="ACF Residuo4 M1", lag.max=100)
pacf(residuo4, main="PACF Residuo4 M1", lag.max=100)
### Test de Ljung-Box
Box.test(residuo4, lag=100, type = "Ljung-Box")

######### 4.	(40 puntos) De la serie mensual de M1 haga un subconjunto que incluya ###########
######### los datos desde enero del 2000 hasta julio de 2017, con este subconjunto:############
###############################################################################################

Agregados2<-Agregados[-c(1:192),]
plot.ts(Agregados2[,2], main="M1 mensual",
        ylab="M1", xlab="Fecha", col="blue", las=2)

#Cambiar objeto data frame a series de tiempo
Agregados2.ts<-ts(Agregados2, frequency=12, start=c(2000, 1))

plot.ts(Agregados2.ts[,2], main="M1 mensual",
        ylab="M1", xlab="Fecha", col="blue", las=2)

#a.	Calcule las pruebas de raíz unitaria Dickey-Fuller Aumentada, Phllips-Perron y KPSS 
###(agregue tendencia como parámetro), utilizando el paquete tseries.


##4a. raíz unitaria Dickey-Fuller Aumentada, Phllips-Perron y KPSS 
####Prueba de Raiz Unitaria 
### Dickey fuller aumentada
adf.test(Agregados2.ts[,2], k=12) 

### Phillips-perron
pp.test(Agregados2.ts[,2])

## kpss
kpss.test(Agregados2.ts[,2], null = "Trend", lshort = T)  

### 4b.	Costruya los correlogramas de la ACF y PACF para la serie M1 en niveles.

par(mfrow=c(1, 2))
acf(Agregados2.ts[,2], main="ACF M1", lag.max=53)
pacf(Agregados2.ts[,2], main="PACF M1", lag.max=53)

#4c.	Costruya los correlogramas de la ACF y PACF para la 
#primera diferencia del logaritmo natural del M1.

par(mfrow=c(1, 2))
acf(na.exclude(diff(log(Agregados2.ts[,2]))), main="ACF LogdiffM1", lag.max=53)
pacf(na.exclude(diff(log(Agregados2.ts[,2]))), main="PACF LogdiffM1", lag.max=53)

modelo1<-arima(log(Agregados2.ts[, 2]), order = c(1,1,0), season= c(1,0,0))
summary(modelo1)
residuo1<-modelo1$res
plot.ts(residuo1)
par(mfrow=c(1, 2))
acf(residuo1, main="ACF Residuo1 M1", lag.max=55)
pacf(residuo1, main="PACF Residuo1 M1", lag.max=55)
### Test de Ljung-Box
Box.test(residuo1, lag=53, type = "Ljung-Box")

modelo2<-arima(log(Agregados2.ts[, 2]), order = c(3,1,1), season= c(1,0,1))
 summary(modelo2)
residuo2<-modelo2$res
plot.ts(residuo2)
par(mfrow=c(1, 2))
acf(residuo2, main="ACF Residuo2 M1", lag.max=55)
pacf(residuo2, main="PACF Residuo2 M1", lag.max=55)
### Test de Ljung-Box
Box.test(residuo2, lag=53, type = "Ljung-Box")

modelo3<-arima(log(Agregados2.ts[, 2]), order = c(6,1,1), season= c(1,0,1))
summary(modelo3)
residuo3<-modelo3$res
plot.ts(residuo3)
par(mfrow=c(1, 2))
acf(residuo3, main="ACF Residuo3 M1", lag.max=55)
pacf(residuo3, main="PACF Residuo3 M1", lag.max=55)
### Test de Ljung-Box
Box.test(residuo3, lag=53, type = "Ljung-Box")

#4f.	Con la función auto.arima del paquete forecast, estime el modelo para la 
#primera diferencia del logaritmo natural del M1, realice el diagnóstico de los 
#residuos del modelo. Compare los resultados del modelo obtenido con la 
#función auto.arima con los del modelo seleccionado en el punto e.

modelo4<-auto.arima(log(Agregados2.ts[, 2]), d=1)
summary(modelo4)
residuo4<-modelo4$res
plot.ts(residuo4)
par(mfrow=c(1, 2))
acf(residuo4, main="ACF Residuo4 M1", lag.max=53)
pacf(residuo4, main="PACF Residuo4 M1", lag.max=53)
### Test de Ljung-Box
Box.test(residuo4, lag=53, type = "Ljung-Box")