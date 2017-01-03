#FRANCISCO PÉREZ HERNÁNDEZ 20076629K

require(ISLR)
require(MASS)
Boston
fix(Boston)
help(Boston)
attach(Boston)

#Búsqueda de posibles relaciones
temp <- Boston
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""), ylab=names(temp)[y])
}
par(mfrow=c(3,4))
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2]) 
par(mfrow=c(1,1))

#Si afinamos más
par(mfrow=c(3,3))
x <- sapply(c(1, 5, 6, 7, 8, 10, 11, 12, 13), plotY, dim(temp)[2]) 
par(mfrow=c(1,1))
#Donde se ve que las candidatas son rm y lstat

#Modelo lineal para lstat
fit1=lm(medv~lstat)
fit1

#Modelo para rm
fit2=lm(medv~rm)
fit2

#Visualización
summary(fit1)
par(mfrow=c(2,1)) 
plot(medv~lstat,Boston) 
abline(fit1,col="red") 
confint(fit1)
summary(fit2)
plot(medv~rm,Boston) 
abline(fit2,col="blue") 
par(mfrow=c(1,1)) 
confint(fit2)

#Cálculo manual de la raíz del ECM (RMSE)
sqrt(sum(fit1$residuals^2)/length(fit1$residuals))

#Predicción de nuevos datos
predict(fit1,data.frame(lstat=c(5,10,15)))
#Cálculo manual de la raiz del ECM (RMSE) para el conjunto de test
yprime=predict(fit1,data.frame(lstat=Boston$lstat))
#o directamente #yprime=predict(fit1,Boston)
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))

#Modelos lineales múltiples
fit3=lm(medv~lstat+age,data=Boston)
summary(fit3)
#Visualización de pares de variables por escala de grises
temp <- Boston 
plot(temp[,-dim(temp)[2]],pch=16,col=gray(1-(temp[,dim(temp)[2]]/max(temp[,dim(temp)[2]]))))

fit4=lm(medv~lstat+rm,data=Boston)
summary(fit4)

fit5=lm(medv~.,data=Boston)
summary(fit5)

fit6=lm(medv~.-age-indus,data=Boston)
summary(fit6)

fit7=lm(medv~.-age-indus-chas-crim,data=Boston)
summary(fit7)

fit8=lm(medv~lstat*rm,Boston) 
summary(fit8)
plot(medv~lstat) 
points(lstat,fitted(fit8),col="green",pch=20)

fit9=lm(medv~I(lstat^2),Boston)
summary(fit9)
plot(medv~lstat)
points(lstat,fitted(fit9),col="red",pch=20)

fit9=lm(medv~lstat +I(lstat^2),Boston)
summary(fit9)
plot(medv~lstat)
points(lstat,fitted(fit9),col="red",pch=20)

fit10=lm(medv~poly(lstat,18))
summary(fit10)
points(lstat,fitted(fit10),col="blue",pch=20)

fit11=lm(medv~poly(lstat,5))
summary(fit11)
points(lstat,fitted(fit11),col="green",pch=20)

fitprueba=lm(medv~lstat +rm +I(lstat * rm) +I(lstat^2) +I(lstat^2 * rm),Boston) 
summary(fitprueba)
plot(medv~lstat)
points(lstat,fitted(fitprueba),col="red",pch=20)

#Predicción sobre nuevos datos
yprime=predict(fit8,Boston) 
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))



#################################################
#################################################
################## CALIFORNIA ###################
#################################################
#################################################

xtra <- read.csv("california.dat", comment.char = "@")
#Asignación manual
names(xtra) <- c("Longitude", "Latitude", "HousingMedianAge",
                 "TotalRooms", "TotalBedrooms", "Population", "Households", "MedianIncome", "MedianHouseValue")
#Asignación automática, facilita el acceso a los campos 
n <- length(names(xtra)) - 1
names(xtra)[1:n] <- paste ("X", 1:n, sep="") 
names(xtra)[n+1] <- "Y"

help(xtra)
??xtra
#Como no hay mucha información sobre las variables y el tipo 
#de problema voy a realizar la previsualización de todas las 
#variables entre si
temp <- xtra
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""), ylab=names(temp)[y])
}
par(mfrow=c(2,4))
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2]) 
par(mfrow=c(1,1))
#Por lo que podemos ver en las gráficas, la variable que podría 
#tener interés para nosotros podría ser X6, y quizás X2 y X8

#Modelo lineal para X6
fit1X6=lm(xtra$Y~xtra$X6)
fit1X6

#Modelo para X2
fit2X2=lm(xtra$Y~xtra$X2)
fit2X2

#Modelo para X8
fit3X8=lm(xtra$Y~xtra$X8)
fit3X8

#Visualización
summary(fit1X6) #0.0005582
par(mfrow=c(3,1)) 
plot(xtra$Y~xtra$X6) 
abline(fit1X6,col="red") 
confint(fit1X6)
summary(fit2X2) #0.02074
plot(xtra$Y~xtra$X2) 
abline(fit2X2,col="blue") 
confint(fit2X2)
summary(fit3X8) #0.4734
plot(xtra$Y~xtra$X8)
abline(fit3X8,col="green")
confint(fit3X8)
par(mfrow=c(1,1)) 
#Como hemos visto ha resultado que todas las variables son
#representativas pero la que más nos ayuda es X8 ya que tenemos
#un R^2 de 0.4734

#Cálculo manual de la raíz del ECM (RMSE)
sqrt(sum(fit1X6$residuals^2)/length(fit1X6$residuals))

#Modelos lineales múltiples
fit4X6X2=lm(xtra$Y~xtra$X6+xtra$X2)
summary(fit4X6X2) #0.02234

fit5X6X8=lm(xtra$Y~xtra$X6+xtra$X8)
summary(fit5X6X8) #0.4742

fit6X2X8=lm(xtra$Y~xtra$X2+xtra$X8)
summary(fit6X2X8) #0.4814

fit7X2X6X8=lm(xtra$Y~xtra$X2+xtra$X6+xtra$X8)
summary(fit7X2X6X8) #0.4828
#Por lo que vemos que de momento el modelo que mejor se ajusta es
#la unión de X2,X6 y X8

fit8Todos=lm(Y~.,data=xtra)
summary(fit8Todos) #0.637
#Podemos ver como con todas las variables se consigue el mejor
#ajuste hasta el momento y no tenemos indicio de quitar ninguna
#variable

#CONCLUSIONES:
#El mejor modelo actual es con todas las variables, con lo que obtenemos que 
#podemos explicar un 63,7% del problema con este modelo 

