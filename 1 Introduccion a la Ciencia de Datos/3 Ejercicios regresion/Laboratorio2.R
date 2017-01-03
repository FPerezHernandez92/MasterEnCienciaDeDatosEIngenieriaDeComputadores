#FRANCISCO PÉREZ HERNÁNDEZ 20076629K
setwd("~/Dropbox/zMaster/zRStudio")
#Dudas: T9 da fallo las dos últimas

require(MASS)
attach(Boston) 
require(kknn)

#4 K-NN Uso
fitknn1 <- kknn(medv ~ ., Boston, Boston)
# Por defecto k = 7, distance = 2, kernel = "optimal“ 
# y scale=TRUE
names(fitknn1)

#5 Visualización de los resultados, Cálculo manual del error
plot(medv~lstat)
points(lstat,fitknn1$fitted.values,col="blue",pch=20)
yprime = fitknn1$fitted.values
# o también yprime=predict(fitknn1,Boston)
sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE

#6 Influencia de las variables
fitknn2 <- kknn(medv ~ lstat*rm+I(lstat^2)+age+crim+dis, Boston, Boston)
yprime = fitknn2$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE 
#[1] 2.421144
fitknn3 <- kknn(medv ~ lstat*rm+I(lstat^2)+age+crim+dis+black+nox, Boston, Boston) 
yprime = fitknn3$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE 
#[1] 2.259452
fitknn4 <- kknn(medv ~ . + lstat*rm+I(lstat^2) - chas, Boston, Boston)
yprime = fitknn4$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE 
#[1] 2.029163

fitknn5 <- kknn(medv ~ . - chas, Boston, Boston)
yprime = fitknn5$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE 
#[1] 2.009211
fitknn6 <- kknn(medv ~ . - chas - ptratio -zn, Boston, Boston)
yprime = fitknn6$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE 
#[1] 1.947602
summary(fitknn6)
plot(medv~lstat) 
points(lstat,fitknn1$fitted.values,col="blue",pch=20) 
points(lstat,fitknn5$fitted.values,col="red",pch=20) 
points(lstat,fitknn6$fitted.values,col="green",pch=20)

#8 K-fold cross-validation. Obtención de las medidas del error sobre las mismas particiones
nombre <- "ICD/california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  } 
  else { 
    test <- x_tst
  }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train")) 
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#9
nombre <- "ICD/california"
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  } 
  else { 
    test <- x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train")) 
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))

#10 Como afrontar un problema dado
California <- read.csv("ICD/california.dat", comment.char="@") 
n<-length(names(California)) -1;
names(California)[1:n] <- paste ("X", 1:n, sep=""); names(California)[n+1] <- "Y" 
temp <- California
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=names(temp)[x], ylab=names(temp)[y])
}
par(mfrow=c(2,4)); x<-sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2]); par(mfrow=c(1,1)) 
fitX8 <- lm(Y~X8, California)
fitX7 <- lm(Y~X7, California)
fitX4 <- lm(Y~X4, California)
#.... etc

#11
fit1=lm(Y~X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6)) +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)),California)
#Modelo que usa el logaritmo por ser Y=ln(median house value) #recomendado en el website que describe el problema
#Haciendo pruebas y analizando los datos se plantean posibles #modelos hasta llegar a uno satisfactorio
fit2=lm(Y~., California)
fit3=lm(Y~.+X4*X7*X8, California) 
fit4=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4) +X7*X8*X4*X5*X6, California)
summary(fit1)$adj.r.squared 
summary(fit2)$adj.r.squared 
summary(fit3)$adj.r.squared 
summary(fit4)$adj.r.squared

#12
nombre <- "ICD/california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@") 
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); 
  names(x_tra)[In+1] <- "Y" 
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst } 
  fitMulti=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3) +I(X8^4)+X7*X8*X4*X5*X6,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train")) 
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#14 Comparativa general entre distintos algoritmos
#leemos la tabla con los errores medios de test
resultados <- read.csv("ICD/regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatst) <- resultados[,1]
#leemos la tabla con los errores medios de test
resultados <- read.csv("ICD/regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatra) <- resultados[,1]

#16
##lm (other) vs knn (ref)
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

#17
test_friedman <- friedman.test(as.matrix(tablatst)) 
test_friedman
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)


##################################
# EJERCICIO 2
##################################
xtra <- read.csv("ICD/california.dat", comment.char = "@")
#Asignación manual
names(xtra) <- c("Longitude", "Latitude", "HousingMedianAge",
                 "TotalRooms", "TotalBedrooms", "Population", "Households", "MedianIncome", "MedianHouseValue")
#Asignación automática, facilita el acceso a los campos 
n <- length(names(xtra)) - 1
names(xtra)[1:n] <- paste ("X", 1:n, sep="") 
names(xtra)[n+1] <- "Y"
attach(xtra)

#4 K-NN Uso
fitknn1 <- kknn(Y ~ ., xtra,xtra)
# Por defecto k = 7, distance = 2, kernel = "optimal“ 
# y scale=TRUE
names(fitknn1)

#5 Visualización de los resultados, Cálculo manual del error
plot(Y~X8)
points(X8,fitknn1$fitted.values,col="blue",pch=20)
yprime = fitknn1$fitted.values
# o también yprime=predict(fitknn1,xtra)
sqrt(sum((xtra$Y-yprime)^2)/length(yprime)) #RMSE
#39132,79

#6 Influencia de las variables
medv<-Y
lstat<-X8
rm<-X2
age<-X1
crim<-X3
dis<-X4
black<-X5
nox<-X6
chas <- X7

fitknn2 <- kknn(Y ~ X8*X2+I(X8^2)+X1+X3+X4, xtra, xtra)
yprime = fitknn2$fitted.values; sqrt(sum((xtra$Y-yprime)^2)/length(yprime)) #RMSE 
#[1] 43702.06
fitknn3 <- kknn(Y ~ X8*X2+I(X8^2)+X1+X3+X4+X5+X6, xtra, xtra) 
yprime = fitknn3$fitted.values; sqrt(sum((xtra$Y-yprime)^2)/length(yprime)) #RMSE 
#[1] 39160.5
fitknn4 <- kknn(Y ~ . + X8*X2+I(X8^2) - X7, xtra, xtra)
yprime = fitknn4$fitted.values; sqrt(sum((xtra$Y-yprime)^2)/length(yprime)) #RMSE 
#[1] 39160.5
fitknn5 <- kknn(Y ~ . - X7, xtra, xtra)
yprime = fitknn5$fitted.values; sqrt(sum((xtra$Y-yprime)^2)/length(yprime)) #RMSE 
#[1] 39445.74
plot(Y~X8) 
points(lstat,fitknn1$fitted.values,col="blue",pch=20) 
points(lstat,fitknn4$fitted.values,col="red",pch=20) 
points(lstat,fitknn5$fitted.values,col="green",pch=20)

#8 K-fold cross-validation. Obtención de las medidas del error sobre las mismas particiones
#Modelo de todas las variables, Regresión múltiple
nombre <- "ICD/california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  } 
  else { 
    test <- x_tst
  }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train")) 
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#9
#Modelo con todas las variables, Knn
nombre <- "ICD/california"
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  } 
  else { 
    test <- x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train")) 
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
lmMSEtest
knnMSEtest
#El ajuste Knn con todas las variables en el modelo, presenta
#un MSE menor que en Regresión Múltiple

#10 Como afrontar un problema dado
California <- read.csv("ICD/california.dat", comment.char="@") 
n<-length(names(California)) -1;
names(California)[1:n] <- paste ("X", 1:n, sep=""); names(California)[n+1] <- "Y" 
temp <- California
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=names(temp)[x], ylab=names(temp)[y])
}
par(mfrow=c(2,4)); x<-sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2]); par(mfrow=c(1,1)) 

#Modelos Lineales
fitX8 <- lm(Y~X8, California)
fitX7 <- lm(Y~X7, California)
fitX4 <- lm(Y~X4, California)
summary(fitX8) #RSquared: 0.4734
summary(fitX7) #RSquared: 0.004294
summary(fitX4) #RSquared: 0.01796


#Modelos lineales múltiples
fit1=lm(Y~X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6)) +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)),California)
#Modelo que usa el logaritmo por ser Y=ln(median house value) 
#recomendado en el website que describe el problema
#Haciendo pruebas y analizando los datos se plantean posibles 
#modelos hasta llegar a uno satisfactorio
fit2=lm(Y~., California)
fit3=lm(Y~.+X4*X7*X8, California) 
fit4=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4) +X7*X8*X4*X5*X6, California)
summary(fit1)$adj.r.squared 
summary(fit2)$adj.r.squared 
summary(fit3)$adj.r.squared 
summary(fit4)$adj.r.squared
#Vemos como el que mejor R2 presenta es el fit4 con un 68,62% de explicación del problema

#12
nombre <- "ICD/california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@") 
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); 
  names(x_tra)[In+1] <- "Y" 
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst } 
  fitMulti=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3) +I(X8^4)+X7*X8*X4*X5*X6,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lm2MSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train")) 
lm2MSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#Agregamos ademas el mismo modelo con Knn para comparar:
nombre <- "ICD/california"
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@") 
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); 
  names(x_tra)[In+1] <- "Y" 
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst } 
  fitMulti=kknn(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3) +I(X8^4)+X7*X8*X4*X5*X6,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knn2MSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train")) 
knn2MSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))

lm2MSEtest
knn2MSEtest
#De nuevo, como pasaba con el modelo con todas las variables, esté nos proporciona que
#knn tiene un MSE menor que Regresión múltiple.

#14 Comparativa general entre distintos algoritmos
#leemos la tabla con los errores medios de test
resultados <- read.csv("ICD/regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatst) <- resultados[,1]
#leemos la tabla con los errores medios de test
resultados <- read.csv("ICD/regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatra) <- resultados[,1]

#16
#Comparativa por pares de LM y KNN (Wilcoxon´s test)
##lm (other) vs knn (ref)
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

#Se aplica el test Wilcoxon para interpretar los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
#Al ser p-valor mayor a 0.05 no existen diferencias significativas entre ambos

#17
#Comparativas múltiples con Friedman y post-hoc Holm
test_friedman <- friedman.test(as.matrix(tablatst)) 
test_friedman
#Existen diferencias significativas al menos entre un par de algoritmos

tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
#Existen diferencias significativas a favor de M5´
#(3vs1 0.081 y 3vs2, con aprox. 90% confianza)

#############
# Que pasa con train?
#############

#Comparativa por pares de LM y KNN (Wilcoxon’s test)
##lm (other) vs knn (ref)
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2)

#Se aplica el test Wilcoxon para interpretar los resultados
LMvsKNNtra <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtra$statistic
pvalue <- LMvsKNNtra$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtra$statistic
Rmas
Rmenos
pvalue
#Existen diferencias significativas entre ambos

#Comparativas Múltiples. Usaremos Friedman y como post-hoc Holm 
test_friedman <- friedman.test(as.matrix(tablatra))
test_friedman
#Existen diferencias significativas al menos entre un par de algoritmos

#Se aplica post-hoc Holm
tam <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
#Existen diferencias significativas a favor de los 3 algoritmos
