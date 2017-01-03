#Nos movemos al directorio de trabajo
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/1 Introduccion a la Ciencia de Datos/5 Trabajo Final")

#APARTADO A: DESCRIPCIÓN DEL TIPO DE DATOS DE ENTRADA
#REGRESION
baseball <- read.csv("baseball/baseball.dat", comment.char = "@", header = FALSE)
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", "Doubles", "Triples", 
                 "HomeRuns", "Runs_batted_in", "Walks", "Strike-Outs", "Stolen_bases", "Errors", 
                 "Free_agency_eligibility", "Free_agent", "Arbitration_eligibility", "Arbitration", "Salary")
is.data.frame(baseball)
#Podemos ver como es un data frame la estructura de datos
ncol(baseball)
nrow(baseball)
#Con 17 columnas y 337 filas
#Tenemos que las variables "Batting_average" y "On-base_percentage" son numéricas, y el resto son int

#APARTADO A1: Media, desviación estandar...
summary(baseball)
#Obtenemos las medias de las variables o con summary o con apply
medias_baseball <- apply(baseball,2,mean)
medias_baseball
#Para la desviación estandar usamos apply
desviacion_estandar_baseball <- apply(baseball,2,sd)
desviacion_estandar_baseball
#Varianza
varianza_baseball <- apply(baseball,2,var)
varianza_baseball
#Desviación absoluta de la mediana
desviacion_absoluta_de_la_mediana_baseball <- apply(baseball,2,mad)
desviacion_absoluta_de_la_mediana_baseball
#Rango intercuartil
rango_intercuartil_baseball <- apply(baseball,2,IQR)
rango_intercuartil_baseball

#APARTADO A2: Gráficos
plot(baseball)
#Directamente con plot, podemos ver las graficas de cada variable con el resto
par(mfrow=c(3,4))
nombres_baseball = names(baseball)
plot_con_respecto_a_y <- function(x,y){
  plot(baseball[,y]~baseball[,x],xlab=nombres_baseball[x],ylab="Salary")
}
sapply(1:12,plot_con_respecto_a_y,17)
par(mfrow=c(2,2))
sapply(13:16,plot_con_respecto_a_y,17)

#APARTADO A3: Descripción
#Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=76#sub1
correlacion_baseball <- cor(baseball)
correlacion_baseball <- cor(baseball)[,17]
correlacion_baseball[cor(baseball)[,17] > 0.5 | cor(baseball)[,17] < -0.5]

###############################################################
###############################################################
###############################################################
######################### REGRESIÓN ###########################
###############################################################
###############################################################

######## BASEBALL
attach(baseball)

#Modelo lineal para Runs
fitRuns=lm(Salary~Runs)
#fitRuns
#Modelo lineal para Hits
fitHits=lm(Salary~Hits)
#fitHits
#Modelo lineal para Dobules
fitDoubles=lm(Salary~Doubles)
#fitDoubles
#Modelo lineal para HomeRuns
fitHomeRuns=lm(Salary~HomeRuns)
#fitHomeRuns
#Modelo lineal para Runs_batted_in
fitRunsBattedIn=lm(Salary~Runs_batted_in)
#fitRunsBattedIn

mat_r_squ <- matrix(c(summary(fitRuns)$adj.r.squared,
                             summary(fitHits)$adj.r.squared,
                             summary(fitDoubles)$adj.r.squared,
                             summary(fitHomeRuns)$adj.r.squared,
                             summary(fitRunsBattedIn)$adj.r.squared))
rownames(mat_r_squ) <- c("fitRuns","fitHits","fitDoubles",
                                "fitHomeRuns","fitRunsBattedIn")
colnames(mat_r_squ) <- c("adj.r.squared")

mat_p_val <- matrix(c(summary(fitRuns)$coefficients["Runs","Pr(>|t|)"],
                      summary(fitHits)$coefficients["Hits","Pr(>|t|)"],
                      summary(fitDoubles)$coefficients["Doubles","Pr(>|t|)"],
                      summary(fitHomeRuns)$coefficients["HomeRuns","Pr(>|t|)"],
                      summary(fitRunsBattedIn)$coefficients["Runs_batted_in","Pr(>|t|)"]))
colnames(mat_p_val) <- c("p-value")
regresion_simple <- cbind(mat_r_squ,mat_p_val)

mat_rmse <- matrix(c(sqrt(sum(fitRuns$residuals^2)/(length(fitRuns$residuals)-2)),
                     sqrt(sum(fitHits$residuals^2)/(length(fitHits$residuals)-2)),
                     sqrt(sum(fitDoubles$residuals^2)/(length(fitDoubles$residuals)-2)),
                     sqrt(sum(fitHomeRuns$residuals^2)/(length(fitHomeRuns$residuals)-2)),
                     sqrt(sum(fitRunsBattedIn$residuals^2)/(length(fitRunsBattedIn$residuals)-2))))
colnames(mat_rmse) <- c("RMSE")
regresion_simple <- cbind(regresion_simple,mat_rmse)
regresion_simple

par(mfrow=c(3,2))
plot(Runs,Salary)
abline(fitRuns,col="red")
#confint(fitRuns)
plot(Hits,Salary)
abline(fitHits,col="green")
#confint(fitHits)
plot(Doubles,Salary)
abline(fitDoubles,col="blue")
#confint(fitDoubles)
plot(HomeRuns,Salary)
abline(fitHomeRuns,col="orange")
#confint(fitHomeRuns)
plot(Runs_batted_in,Salary)
abline(fitRunsBattedIn,col="brown")
#confint(fitRunsBattedIn)
par(mfrow=c(1,1))

fitM1=lm(Salary~.,data=baseball)
fitM1
summary(fitM1)
fitM2=lm(Salary~Runs+Hits)
fitM2
summary(fitM2)
fitM3=lm(Salary~Runs+Doubles)
fitM3
summary(fitM3)
fitM4=lm(Salary~Runs+HomeRuns)
fitM4
summary(fitM4)
fitM5=lm(Salary~Runs+Runs_batted_in)
fitM5
summary(fitM5)
fitM2=lm(Salary~Runs+Hits+Doubles+HomeRuns+Runs_batted_in)
fitM2
summary(fitM2)













nombre <- "baseball/baseball"
run_lm_fold <- function(i, x,modelo, tt = "test") {
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
  fitMulti=lm(modelo,x_tra)
  yprime=predict(fitMulti,test)
  mse <- sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
  resumen <- summary(fitMulti)
  #Error estandar residual
  a <- 100*(resumen$sigma/(mean(x_tra$Y)))
  a <- format(a,digits=3)
  #Coeficiente de determinacion R2
  b <- resumen$r.squared
  b <- format(b,digits=3)
  #Coeficiente de determinacion R2 ajustado
  c <- resumen$adj.r.squared
  c <- format(c,digits=3)
  #RMSE
  d <- sqrt(sum(fitMulti$residuals^2)/(length(fitMulti$residuals)-2))
  d <- format(d,digits = 3)
  salida <- list("EER" = a, "R2" = b, "R2_Adj" = c, "MSE" = mse, "RMSE" = d) 
  return (salida)
}


Analisis_lm_fold <- function(modelo, nombre){
  lmMSEtrain<-(sapply(1:5,run_lm_fold,nombre,modelo,"train")) 
  lmMSEtest<-(sapply(1:5,run_lm_fold,nombre,modelo,"test"))
  
  #Error Estandar Residual
  a <- mean(as.numeric(lmMSEtest[1,]))
  at <- ifelse(a<10,"Si","No")
  a <- format(a,digits=3)
  # Coeficiente de determinacion R2
  b <- mean(as.numeric(lmMSEtest[2,]))
  bt <- ifelse(b>0.8,"Si", "No")
  # Coeficiente de determinacion R2 ajustado
  c <- mean(as.numeric(lmMSEtest[3,]))
  ct <- ifelse(abs(b-c)<0.01,"Si","No")
  b <- format(b,digits = 3)
  c <- format(c,digits = 3)
  d <- mean(as.numeric(lmMSEtest[5,]))
  salida <- list ( "EER"     = a, "T1" = at,
                   "R2"      = b, "T2" = bt,
                   "R2_Adj"  = c, "T3" = ct,
                   "MSEtrain" = mean(as.numeric(lmMSEtrain[4,])),
                   "MSEtest" = mean(as.numeric(lmMSEtest[4,])),
                   "RMSE" = d) 
  return (salida)
}
#Runs 3, Hits 4, Doubles 5, HomeRuns 7, Runs_batted_in 8
modelo1 <- Y~X3
modelo2 <- Y~X4
modelo3 <- Y~X5
modelo4 <- Y~X7
modelo5 <- Y~X8
modelo6 <- Y~.
a <- Analisis_lm_fold(modelo1,nombre)
b <- Analisis_lm_fold(modelo2,nombre)
c <- Analisis_lm_fold(modelo3,nombre)
d <- Analisis_lm_fold(modelo4,nombre)
e <- Analisis_lm_fold(modelo5,nombre)
f <- Analisis_lm_fold(modelo6,nombre)
df <- data.frame(rbind(model1=a,model2=b,model3=c,model4=d,model5=e,model6=f))
df


fitM1=lm(Salary~.,data=baseball)
fitM1
summary(fitM1)
fitM2=lm(Salary~Runs+Hits)
fitM2
summary(fitM2)
fitM3=lm(Salary~Runs+Doubles)
fitM3
summary(fitM3)
fitM4=lm(Salary~Runs+HomeRuns)
fitM4
summary(fitM4)
fitM5=lm(Salary~Runs+Runs_batted_in)
fitM5
summary(fitM5)
fitM2=lm(Salary~Runs+Hits+Doubles+HomeRuns+Runs_batted_in)
fitM2
summary(fitM2)


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

















#CLASIFICACIÓN
australian <- read.csv("australian/australian.dat", comment.char = "@", header = FALSE)
is.data.frame(australian)
#Podemos ver como es un data frame la estructura de datos
ncol(australian)
nrow(australian)
#Con 15 columnas y 690 filas
#Tenemos que las variables 2,3 y 7 son numéricas, y el resto son int

#APARTADO A1: Media, desviación estandar...
summary(australian)
#Obtenemos las medias de las variables o con summary o con apply
medias_australian <- apply(australian,2,mean)
medias_australian
#Para la desviación estandar usamos apply
desviacion_estandar_australian <- apply(australian,2,sd)
desviacion_estandar_australian
#Varianza
varianza_australian <- apply(australian,2,var)
varianza_australian
#Desviación absoluta de la mediana
desviacion_absoluta_de_la_mediana_australian <- apply(australian,2,mad)
desviacion_absoluta_de_la_mediana_australian
#Rango intercuartil
rango_intercuartil_australian <- apply(australian,2,IQR)
rango_intercuartil_australian

#APARTADO A2: Gráficos
plot(australian)
#Directamente con plot, podemos ver las graficas de cada variable con el resto
par(mfrow=c(3,3))
nombres_australian = names(australian)
plot_con_respecto_a_y <- function(x,y){
  plot(australian[,y]~australian[,x],xlab=nombres_australian[x],ylab="15")
}
sapply(1:9,plot_con_respecto_a_y,15)
par(mfrow=c(3,2))
sapply(10:14,plot_con_respecto_a_y,15)

plot(australian)
#Directamente con plot, podemos ver las graficas de cada variable con el resto

#APARTADO A3: Descripción
#Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=53#sub1
correlacion_australian <- cor(australian)
correlacion_australian <- cor(australian)[,15]
correlacion_australian[cor(australian)[,15] > 0.5 | cor(australian)[,15] < -0.5]










#########################################
#Dividiendo en train y test

baseball_train = baseball[1:250,]
baseball_test = baseball[251:337,]
#Modelo lineal para Runs
fitRuns=lm(Salary~Runs,data=baseball_train)
fitRuns
#Modelo lineal para Hits
fitHits=lm(Salary~Hits,data=baseball_train)
fitHits
#Modelo lineal para Dobules
fitDoubles=lm(Salary~Doubles,data=baseball_train)
fitDoubles
#Modelo lineal para HomeRuns
fitHomeRuns=lm(Salary~HomeRuns,data=baseball_train)
fitHomeRuns
#Modelo lineal para Runs_batted_in
fitRunsBattedIn=lm(Salary~Runs_batted_in,data=baseball_train)
fitRunsBattedIn

mat_r_squ <- matrix(c(summary(fitRuns)$adj.r.squared,
                      summary(fitHits)$adj.r.squared,
                      summary(fitDoubles)$adj.r.squared,
                      summary(fitHomeRuns)$adj.r.squared,
                      summary(fitRunsBattedIn)$adj.r.squared))
rownames(mat_r_squ) <- c("fitRuns","fitHits","fitDoubles",
                         "fitHomeRuns","fitRunsBattedIn")
colnames(mat_r_squ) <- c("adj.r.squared")

mat_p_val <- matrix(c(summary(fitRuns)$coefficients["Runs","Pr(>|t|)"],
                      summary(fitHits)$coefficients["Hits","Pr(>|t|)"],
                      summary(fitDoubles)$coefficients["Doubles","Pr(>|t|)"],
                      summary(fitHomeRuns)$coefficients["HomeRuns","Pr(>|t|)"],
                      summary(fitRunsBattedIn)$coefficients["Runs_batted_in","Pr(>|t|)"]))
colnames(mat_p_val) <- c("p-value")
regresion_simple <- cbind(mat_r_squ,mat_p_val)

mat_rmse <- matrix(c(sqrt(sum(fitRuns$residuals^2)/(length(fitRuns$residuals)-2)),
                     sqrt(sum(fitHits$residuals^2)/(length(fitHits$residuals)-2)),
                     sqrt(sum(fitDoubles$residuals^2)/(length(fitDoubles$residuals)-2)),
                     sqrt(sum(fitHomeRuns$residuals^2)/(length(fitHomeRuns$residuals)-2)),
                     sqrt(sum(fitRunsBattedIn$residuals^2)/(length(fitRunsBattedIn$residuals)-2))))
colnames(mat_rmse) <- c("RMSE")
regresion_simple <- cbind(regresion_simple,mat_rmse)
regresion_simple

yprime=predict(fitRuns,baseball_test)
sqrt(sum(abs(baseball_test$Salary-yprime)^2)/length(yprime))
