##Nos movemos al directorio de trabajo
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/1 Introduccion a la Ciencia de Datos/5 Trabajo Final")

###############################################################
###############################################################
###############################################################
############### A) DESCRIPCIÓN DEL PROBLEMA ###################
###############################################################
###############################################################

##REGRESIÓN
baseball <- read.csv("baseball/baseball.dat", comment.char = "@", header = FALSE)
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", "Doubles", "Triples", 
                 "HomeRuns", "Runs_batted_in", "Walks", "Strike-Outs", "Stolen_bases", "Errors", 
                 "Free_agency_eligibility", "Free_agent", "Arbitration_eligibility", "Arbitration", "Salary")
is.data.frame(baseball)
##Podemos ver como es un data frame la estructura de datos
ncol(baseball)
nrow(baseball)
##Con 17 columnas y 337 filas
##Tenemos que las variables "Batting_average" y "On-base_percentage" son numéricas, y el resto son int

##APARTADO A1: Media, desviación estandar...
summary(baseball)
##Obtenemos las medias de las variables o con summary o con apply
medias_baseball <- apply(baseball,2,mean)
medias_baseball
##Para la desviación estandar usamos apply
desviacion_estandar_baseball <- apply(baseball,2,sd)
desviacion_estandar_baseball
##Varianza
varianza_baseball <- apply(baseball,2,var)
varianza_baseball
##Desviación absoluta de la mediana
desviacion_absoluta_de_la_mediana_baseball <- apply(baseball,2,mad)
desviacion_absoluta_de_la_mediana_baseball
##Rango intercuartil
rango_intercuartil_baseball <- apply(baseball,2,IQR)
rango_intercuartil_baseball

##APARTADO A2: Gráficos
plot(baseball)
##Directamente con plot, podemos ver las gráficas de cada variable con el resto
par(mfrow=c(3,4))
nombres_baseball = names(baseball)
plot_con_respecto_a_y <- function(x,y){
  plot(baseball[,y]~baseball[,x],xlab=nombres_baseball[x],ylab="Salary")
}
sapply(1:12,plot_con_respecto_a_y,17)
par(mfrow=c(2,2))
sapply(13:16,plot_con_respecto_a_y,17)

##APARTADO A3: Descripción
##Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=76#sub1
correlacion_baseball <- cor(baseball)
correlacion_baseball <- cor(baseball)[,17]
correlacion_baseball[cor(baseball)[,17] > 0.5 | cor(baseball)[,17] < -0.5]



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

#APARTADO A3: Descripción
#Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=53#sub1
correlacion_australian <- cor(australian)
correlacion_australian <- cor(australian)[,15]
correlacion_australian[cor(australian)[,15] > 0.5 | cor(australian)[,15] < -0.5]


###############################################################
###############################################################
###############################################################
###################### B) REGRESIÓN ###########################
###############################################################
###############################################################

library(kknn)
rm(list=ls())
baseball <- read.csv("baseball/baseball.dat", comment.char = "@", header = FALSE)
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", "Doubles", "Triples", 
                     "HomeRuns", "Runs_batted_in", "Walks", "Strike-Outs", "Stolen_bases", "Errors", 
                     "Free_agency_eligibility", "Free_agent", "Arbitration_eligibility", "Arbitration", "Salary")
attach(baseball)

###############################################################
##################### REGRESIÓN SIMPLE ########################
###############################################################

##Modelo lineal para Runs
fitRuns=lm(Salary~Runs)
#fitRuns
##Modelo lineal para Hits
fitHits=lm(Salary~Hits)
#fitHits
##Modelo lineal para Dobules
fitDoubles=lm(Salary~Doubles)
#fitDoubles
##Modelo lineal para HomeRuns
fitHomeRuns=lm(Salary~HomeRuns)
#fitHomeRuns
##Modelo lineal para Runs_batted_in
fitRunsBattedIn=lm(Salary~Runs_batted_in)
#fitRunsBattedIn

##Creo la matriz para obtener el r cuadrado ajustado
mat_r_squ <- matrix(c(summary(fitRuns)$adj.r.squared,
                             summary(fitHits)$adj.r.squared,
                             summary(fitDoubles)$adj.r.squared,
                             summary(fitHomeRuns)$adj.r.squared,
                             summary(fitRunsBattedIn)$adj.r.squared))
rownames(mat_r_squ) <- c("fitRuns","fitHits","fitDoubles",
                                "fitHomeRuns","fitRunsBattedIn")
colnames(mat_r_squ) <- c("adj.r.squared")

##Creo la matriz para obtener el p valor
mat_p_val <- matrix(c(summary(fitRuns)$coefficients["Runs","Pr(>|t|)"],
                      summary(fitHits)$coefficients["Hits","Pr(>|t|)"],
                      summary(fitDoubles)$coefficients["Doubles","Pr(>|t|)"],
                      summary(fitHomeRuns)$coefficients["HomeRuns","Pr(>|t|)"],
                      summary(fitRunsBattedIn)$coefficients["Runs_batted_in","Pr(>|t|)"]))
colnames(mat_p_val) <- c("p-value")
regresion_simple <- cbind(mat_r_squ,mat_p_val)

##Creo la matriz para obtener el rmse
mat_rmse <- matrix(c(sqrt(sum(fitRuns$residuals^2)/(length(fitRuns$residuals)-2)),
                     sqrt(sum(fitHits$residuals^2)/(length(fitHits$residuals)-2)),
                     sqrt(sum(fitDoubles$residuals^2)/(length(fitDoubles$residuals)-2)),
                     sqrt(sum(fitHomeRuns$residuals^2)/(length(fitHomeRuns$residuals)-2)),
                     sqrt(sum(fitRunsBattedIn$residuals^2)/(length(fitRunsBattedIn$residuals)-2))))
colnames(mat_rmse) <- c("RMSE")
##Uno los matrices y la muestro
regresion_simple <- cbind(regresion_simple,mat_rmse)
regresion_simple

##Creo las gráficas de los modelos realizados
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

##Como la siguiente parte es realizar los modelos de
##regresión múltiple, voy a realizar las funciones que se vieron
##para realizar cross-validation pero modificadas para obtener los
##datos de una manera más clara

##Función para realizar cross-validation de los modelos lm
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

##Función para realizar el análisis de cross-validation
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

nombre <- "baseball/baseball"
#Número de las variables:Runs 3, Hits 4, Doubles 5, HomeRuns 7, Runs_batted_in 8
modelo1 <- Y~X3
modelo2 <- Y~X4
modelo3 <- Y~X5
modelo4 <- Y~X7
modelo5 <- Y~X8
modelo6 <- Y~.
lm1 <- Analisis_lm_fold(modelo1,nombre)
lm2 <- Analisis_lm_fold(modelo2,nombre)
lm3 <- Analisis_lm_fold(modelo3,nombre)
lm4 <- Analisis_lm_fold(modelo4,nombre)
lm5 <- Analisis_lm_fold(modelo5,nombre)
lm6 <- Analisis_lm_fold(modelo6,nombre)
regresion_simple_cv <- data.frame(rbind(model1=lm1,model2=lm2,model3=lm3,model4=lm4,model5=lm5,model6=lm6))
regresion_simple_cv
##Podemos ver como de momento el mejor modelo sería el modelo6, con todas las variables,
##ya que obtenemos un R2 ajustado de 0,68, muy superior al resto. Además el MSE de test medio
##es el más bajo de todos los modelos. Pero para los modelos simples, tenemos que el que mejor
##R2 ajustado sería el modelo5, el cual tiene también el MSEtest más pequeño. 



###############################################################
################### REGRESIÓN MÚLTIPLE ########################
###############################################################

##Vamos a pasar ahora a buscar el mejor modelo de regresión múltiple
fitM1=lm(Salary~.,data=baseball)
summary(fitM1) #R2adj 0.686
##Voy a quitar algunas de las que parece que tienen menos influencia como:
##Doubles, Hits, Triples
fitM2=lm(Salary~. -Doubles-Hits-Triples,data=baseball)
summary(fitM2) #R2adj 0.688 hemos mejorado
modelo7 <- Y~. -X4-X5-X6
lm7 <- Analisis_lm_fold(modelo7,nombre)
#lm7
##Sigo quitando, por ejemplo: Runs, Batting_average
fitM3=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average,data=baseball)
summary(fitM3) #R2adj 0.6881
modelo8 <- Y~. -X4-X5-X6-X3-X1
lm8 <- Analisis_lm_fold(modelo8,nombre)
#lm8
##Pruebo a quitar: `On-base_percentage`
fitM4=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average-`On-base_percentage`,data=baseball)
summary(fitM4) #R2adj 0.688 
modelo9 <- Y~. -X4-X5-X6-X3-X1-X2
lm9 <- Analisis_lm_fold(modelo9,nombre)
#lm9
##Pruebo a quitar: Errors
fitM5=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average-`On-base_percentage`-Errors,data=baseball)
summary(fitM5) #R2adj 0.6876 
modelo10 <- Y~. -X4-X5-X6-X3-X1-X2-X12
lm10 <- Analisis_lm_fold(modelo10,nombre)
#lm10
##Pruebo a quitar: Walks y Arbitration
fitM6=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average-`On-base_percentage`-Errors-Walks-Arbitration,data=baseball)
summary(fitM6) #R2adj 0.685
modelo11 <- Y~. -X4-X5-X6-X3-X1-X2-X12-X9-X16
lm11 <- Analisis_lm_fold(modelo11,nombre)
#lm11

##Llegados a este punto las variables que nos quedan que parecen más relevantes son:
##X7:HomeRuns , X8:Runs_batted_in ,X10:`Strike-Outs` ,X11:Stolen_bases ,
##X13:Free_agency_eligibility ,X14:Free_agent ,X15:Arbitration_eligibility 
fitM7=lm(Salary~HomeRuns*Runs_batted_in*`Strike-Outs`*Stolen_bases*Free_agency_eligibility*Free_agent*Arbitration_eligibility,data=baseball)
summary(fitM7) #R2adj 0.7932 el mejor hasta el momento
modelo12 <- Y~X7*X8*X10*X11*X13*X14*X15
lm12 <- Analisis_lm_fold(modelo12,nombre)
#lm12
##Las variables que interfieren (cuando p-value es pequeño) son: Free_agent, Runs_batted_in, 
##Free_agency_eligibility, Stolen_bases, `Strike-Outs`,HomeRuns
##Por lo que voy a probar a eliminar la variable Arbitration_eligibility
fitM8=lm(Salary~HomeRuns*Runs_batted_in*`Strike-Outs`*Stolen_bases*Free_agency_eligibility*Free_agent,data=baseball)
summary(fitM8) #R2adj 0.7195 
modelo13 <- Y~X7*X8*X10*X11*X13*X14
lm13 <- Analisis_lm_fold(modelo13,nombre)
#lm13
#Sigo probando modelos
fitM9=lm(Salary~(HomeRuns^2)*(Runs_batted_in^2)*(`Strike-Outs`^2)*Stolen_bases*Free_agency_eligibility*Free_agent*Arbitration_eligibility,data=baseball)
summary(fitM9) #R2adj 0.7932 igual que el mejor 
modelo14 <- Y~(X7^2)*(X8^2)*(X10^2)*X11*X13*X14*X15
lm14 <- Analisis_lm_fold(modelo14,nombre)
#lm14

fitM10=lm(Salary~(HomeRuns^2)*(Runs_batted_in^2)*(`Strike-Outs`^2)*(Stolen_bases^2)*(Free_agency_eligibility^2)*(Free_agent^2)*(Arbitration_eligibility^2),data=baseball)
summary(fitM10) #R2adj 0.7932 igual que el mejor 
modelo15 <- Y~(X7^2)*(X8^2)*(X10^2)*(X11^2)*(X13^2)*(X14^2)*(X15^2)
lm15 <- Analisis_lm_fold(modelo15,nombre)
#lm15

fitM11=lm(Salary~I(HomeRuns^2)*I(Runs_batted_in^2)*I(`Strike-Outs`^2)*(Stolen_bases^2)*(Free_agency_eligibility^2)*(Free_agent^2)*(Arbitration_eligibility^2),data=baseball)
summary(fitM11) #R2adj 0.7885 
modelo16 <- Y~I(X7^2)*I(X8^2)*I(X10^2)*(X11^2)*(X13^2)*(X14^2)*(X15^2)
lm16 <- Analisis_lm_fold(modelo16,nombre)
#lm16

fitM12=lm(Salary~I(HomeRuns^2)*I(Runs_batted_in^2)*I(`Strike-Outs`^2)*I(Stolen_bases^2)*I(Free_agency_eligibility^2)*I(Free_agent^2)*I(Arbitration_eligibility^2),data=baseball)
summary(fitM12) #R2adj 0.7854
modelo17 <- Y~I(X7^2)*I(X8^2)*I(X10^2)*I(X11^2)*I(X13^2)*I(X14^2)*I(X15^2)
lm17 <- Analisis_lm_fold(modelo17,nombre)
#lm17

fitM13=lm(Salary~I(HomeRuns^2)+I(Runs_batted_in^2)+I(`Strike-Outs`^2)+I(Stolen_bases^2)+I(Free_agency_eligibility^2)+I(Free_agent^2)+I(Arbitration_eligibility^2),data=baseball)
summary(fitM13) #R2adj 0.6914
modelo18 <- Y~I(X7^2)+I(X8^2)+I(X10^2)+I(X11^2)+I(X13^2)+I(X14^2)+(X15^2)
lm18 <- Analisis_lm_fold(modelo18,nombre)
#lm18
##X7:HomeRuns , X8:Runs_batted_in ,X10:`Strike-Outs` ,X11:Stolen_bases ,
##X13:Free_agency_eligibility ,X14:Free_agent ,X15:Arbitration_eligibility 
fitM14=lm(Salary~I(Runs_batted_in*HomeRuns^2)+I(`Strike-Outs`*Stolen_bases^2)+I(Free_agency_eligibility*Free_agent^2)+I(Arbitration_eligibility^2),data=baseball)
summary(fitM14) #R2adj 0.3087
modelo19 <- Y~I(X8*X7^2)+I(X10*X11^2)+I(X13*X14^2)+I(X15^2)
lm19 <- Analisis_lm_fold(modelo19,nombre)
#lm19

fitM15=lm(Salary~I(Runs_batted_in*Free_agency_eligibility^2)+I(Runs_batted_in*Arbitration_eligibility^2)+I(Stolen_bases*Free_agency_eligibility^2)+I(Stolen_bases*Arbitration_eligibility^2)+I(HomeRuns*`Strike-Outs`*Free_agent^2),data=baseball)
summary(fitM15) #R2adj 0.7363
modelo20 <- Y~I(X8*X13^2)+I(X8*X15^2)+I(X11*X13^2)+I(X11*X15^2)+I(X7*X10*X14^2)
lm20 <- Analisis_lm_fold(modelo20,nombre)
#lm20

fitM16=lm(Salary~I(Runs_batted_in*Free_agency_eligibility^2)+I(Runs_batted_in*Arbitration_eligibility^2)+I(Stolen_bases*Free_agency_eligibility^2)+I(Stolen_bases*Arbitration_eligibility^2),data=baseball)
summary(fitM16) #R2adj 0.7363
modelo21 <- Y~I(X8*X13^2)+I(X8*X15^2)+I(X11*X13^2)+I(X11*X15^2)
lm21 <- Analisis_lm_fold(modelo21,nombre)
#lm21

fitM17=lm(Salary~I(Runs_batted_in*Free_agency_eligibility^2)+I(Runs_batted_in*Arbitration_eligibility^2)+I(Stolen_bases*Free_agency_eligibility^2),data=baseball)
summary(fitM17) #R2adj 0.7303
modelo22 <- Y~I(X8*X13^2)+I(X8*X15^2)+I(X11*X13^2)
lm22 <- Analisis_lm_fold(modelo22,nombre)
#lm22

fitM18=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility,data=baseball)
summary(fitM18) #R2adj 0.7479
modelo23 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15
lm23 <- Analisis_lm_fold(modelo23,nombre)
#lm23

fitM19=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agency_eligibility,data=baseball)
summary(fitM19) #R2adj 0.7549
modelo24 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X13
lm24 <- Analisis_lm_fold(modelo24,nombre)
#lm24

fitM20=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agency_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agent,data=baseball)
summary(fitM20) #R2adj 0.7704
modelo25 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X13+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X14
lm25 <- Analisis_lm_fold(modelo25,nombre)
#lm25

fitM21=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agency_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agent+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Arbitration,data=baseball)
summary(fitM21) #R2adj 0.7795
modelo26 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X13+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X14+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X16
lm26 <- Analisis_lm_fold(modelo26,nombre)
#lm26

dflm <- data.frame(rbind(model1=lm1,model2=lm2,model3=lm3,model4=lm4,model5=lm5,model6=lm6,
                       model7=lm7,model8=lm8,model9=lm9,model10=lm10,model11=lm11,model12=lm12,model13=lm13
                       ,model14=lm14,model15=lm15,model16=lm16,model17=lm17,model18=lm18,model19=lm19,model20=lm20
                       ,model21=lm21,model22=lm22,model23=lm23,model24=lm24,model25=lm25,model26=lm26))
dflm
dflm[which.min(as.numeric(dflm[,8])),]
##El modelo que ha obtenido un menor MSE en test ha sido el modelo 20
##Vemos que su R2 de 0,747 y un R2adj de 0,742 por lo que se explica un 74% del problema
dflm[which.min(as.numeric(dflm[,7])),]
##El modelo 16 tiene el menor MSE en train
##Este modelo tiene muy buen R2

dflm[which.max(as.numeric(dflm[,7])),]
##El modelo con un mayor MSE en training es el modelo 19
dflm[which.max(as.numeric(dflm[,8])),]
##El modelo 17 obtiene el mayor MSE en test, por lo que indica sobre-ajuste del modelo. 
dflm[which.max(as.numeric(dflm[,3])),]
##El modelo 16 ha obtenido el mayor R2
dflm[which.max(as.numeric(dflm[,5])),]
##Siendo el mismo que ha obtenido el R2adj mayor. 

dflm[c(16,20),]
##Por lo tanto tenemos para elegir entre dos modelos el 16 o el 20.
##Como mejor modelo para esta parte eligiría el modelo 20 al tener un menor MSEtest.

modelo20


###############################################################
################### KNN PARA REGRESIÓN ########################
###############################################################
set.seed(1234)
##Si realizamos un análisis como el de las transparencias, antes de aplicar knn-fold:
##Modelo para Runs
fitRunsknn=kknn(Salary~Runs,baseball,baseball)
##Modelo para Hits
fitHitsknn=kknn(Salary~Hits,baseball,baseball)
##Modelo para Dobules
fitDoublesknn=kknn(Salary~Doubles,baseball,baseball)
##Modelo para HomeRuns
fitHomeRunsknn=kknn(Salary~HomeRuns,baseball,baseball)
##Modelo para Runs_batted_in
fitRunsBattedInknn=kknn(Salary~Runs_batted_in,baseball,baseball)

par(mfrow=c(3,2))
plot(Salary~Runs)
points(Runs,fitRunsknn$fitted.values,col="red",pch=20)
plot(Salary~Hits)
points(Hits,fitHitsknn$fitted.values,col="green",pch=20)
plot(Salary~Doubles)
points(Doubles,fitDoublesknn$fitted.values,col="blue",pch=20)
plot(Salary~HomeRuns)
points(HomeRuns,fitHomeRunsknn$fitted.values,col="orange",pch=20)
plot(Salary~Runs_batted_in)
points(Runs_batted_in,fitRunsBattedInknn$fitted.values,col="brown",pch=20)
par(mfrow=c(1,1))

yprime=fitRunsknn$fitted.values
rmse_runs <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitHitsknn$fitted.values
rmse_hits <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitDoublesknn$fitted.values
rmse_doubles <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitHomeRunsknn$fitted.values
rmse_home_runs <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitRunsBattedInknn$fitted.values
rmse_runs_batted_in <- sqrt(sum((Salary-yprime)^2)/length(yprime))


mat_rmse_knn <- matrix(c(rmse_runs,rmse_hits,rmse_doubles,rmse_home_runs,rmse_runs_batted_in))
colnames(mat_rmse_knn) <- c("RMSEknn")
##Uno los matrices y la muestro
regresion_simple_knn <- cbind(regresion_simple,mat_rmse_knn)
regresion_simple_knn
#Viendo que el RMSEknn es menor que el de lm

##Función para realizar cross-validation de los modelos knn
run_knn_fold <- function(i, x,modelo, tt = "test") {
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
  fitMulti=kknn(modelo,x_tra,test)
  yprime=fitMulti$fitted.values
  rmse <- sqrt(sum((test$Y-yprime)^2)/length(yprime))
  mse <- sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
  salida <- list("MSE"=mse, "RMSE"=rmse)
  return (salida)
}

##Función para realizar el análisis de cross-validation
Analisis_knn_fold <- function(modelo, nombre){
  knnMSEtrain<-(sapply(1:5,run_knn_fold,nombre,modelo,"train")) 
  knnMSEtest<-(sapply(1:5,run_knn_fold,nombre,modelo,"test"))
  a <- mean(as.numeric(knnMSEtest[2,]))
  salida <- list ( "MSEtrain" = mean(as.numeric(knnMSEtrain[1,])),
                   "MSEtest" = mean(as.numeric(knnMSEtest[1,])),
                   "RMSE"=a) 
  return (salida)
}
set.seed(1234)
knn1 <- Analisis_knn_fold(modelo1,nombre)
knn2 <- Analisis_knn_fold(modelo2,nombre)
knn3 <- Analisis_knn_fold(modelo3,nombre)
knn4 <- Analisis_knn_fold(modelo4,nombre)
knn5 <- Analisis_knn_fold(modelo5,nombre)
knn6 <- Analisis_knn_fold(modelo6,nombre)
knn7 <- Analisis_knn_fold(modelo7,nombre)
knn8 <- Analisis_knn_fold(modelo8,nombre)
knn9 <- Analisis_knn_fold(modelo9,nombre)
knn10 <- Analisis_knn_fold(modelo10,nombre)
knn11 <- Analisis_knn_fold(modelo11,nombre)
knn12 <- Analisis_knn_fold(modelo12,nombre)
knn13 <- Analisis_knn_fold(modelo13,nombre)
knn14 <- Analisis_knn_fold(modelo14,nombre)
knn15 <- Analisis_knn_fold(modelo15,nombre)
knn16 <- Analisis_knn_fold(modelo16,nombre)
knn17 <- Analisis_knn_fold(modelo17,nombre)
knn18 <- Analisis_knn_fold(modelo18,nombre)
knn19 <- Analisis_knn_fold(modelo19,nombre)
knn20 <- Analisis_knn_fold(modelo20,nombre)
knn21 <- Analisis_knn_fold(modelo21,nombre)
knn22 <- Analisis_knn_fold(modelo22,nombre)
knn23 <- Analisis_knn_fold(modelo23,nombre)
knn24 <- Analisis_knn_fold(modelo24,nombre)
knn25 <- Analisis_knn_fold(modelo25,nombre)
knn26 <- Analisis_knn_fold(modelo26,nombre)

dfknn <- data.frame(rbind(model1=knn1,model2=knn2,model3=knn3,model4=knn4,model5=knn5,model6=knn6,
                       model7=knn7,model8=knn8,model9=knn9,model10=knn10,model11=knn11,model12=knn12,model13=knn13
                       ,model14=knn14,model15=knn15,model16=knn16,model17=knn17,model18=knn18,model19=knn19,model20=knn20
                       ,model21=knn21,model22=knn22,model23=knn23,model24=knn24,model25=knn25,model26=knn26))
dfknn
##Buscamos el que menor MSE en test tenga:
dfknn[which.min(as.numeric(dfknn[,2])),]
##Que es el modelo 22
##El que menor MSE en train tiene es:
dfknn[which.min(as.numeric(dfknn[,1])),]
##El modelo 25
##El que mayor MSE en test presenta es:
dfknn[which.max(as.numeric(dfknn[,2])),]
##El modelo 3
##Y el que mayor MSE en train tiene es:
dfknn[which.max(as.numeric(dfknn[,1])),]
##También el modelo 3

dfknn[c(22,25),]
##Por lo tanto tenemos que los modelos 22 y 25 presentan MSE bajos para test o train,
##y como no coincide con ninguno de los mejores modelo que dieron en lm, para esta parte
##creo que el mejormodelo sería el modelo 22 al tener un MSE de test menor
modelo22

##Si queremos quedarnos con el mejor modelo de entre knn y lm tenemos que:
dflm[20,]
dfknn[22,]
##Si comparamos por MSEtest tenemos que:
dflm[20,8]
dfknn[22,2]
##El modelo 20 tiene un menor valor de MSEtest
##Si observamos el de train:
dflm[20,7]
dfknn[22,1]
##El modelo 22 tiene un menor valor de MSEtrain

##Para la comparativa final me decantaré final me quedaré con el modelo 20 por tener
##un valor de MSEtest más pequeño


###############################################################
############### COMPARACIÓN DE ALGORITMOS #####################
###############################################################

##Comparativa general entre distintos algoritmos
##leemos la tabla con los errores medios de test
resultados <- read.csv("regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatst) <- resultados[,1]
##leemos la tabla con los errores medios de test
resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatra) <- resultados[,1]

tablatst["baseball",1] <- as.numeric(dflm[20,8])
tablatst["baseball",2] <- as.numeric(dfknn[20,2])
tablatst["baseball",]

tablatra["baseball",1] <- as.numeric(dflm[20,7])
tablatra["baseball",2] <- as.numeric(dfknn[20,1])
tablatra["baseball",]

##Comparativa por pares de LM y KNN para test (Wilcoxon´s test)
##lm (other) vs knn (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
##Al ser p-valor mayor a 0.05 no existen diferencias significativas entre ambos
##Solo un 13,49% de confianza de que existan diferencias ((1-pvalue)*100)
##Como el p-value es 0.865, mayor que el nivel de signifiación 0.05, no somos capaces
##a rechazar la hipótesis nula y concluir que no hay suficiente evidencia en los datos.
##Por lo tanto se acepta la hipotesis nula: las dos distribuciones son iguales

##Comparativa por pares de LM y KNN para train (Wilcoxon´s test)
##lm (other) vs knn (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
##Por lo contrario, los reusltados obtenidos en el training el test de Wilcoxon muestra
##un p-value menor que el nivel de significación 0.05, por lo que el p-value es significativo.
##Entonces rechazamos la hipotesis nula y decimos que las distribuciones son distintas. 
##Esta interpretación tiene menor peso que la realizada anteriormente para test, ya que en el 
##training los resultados son con datos ya conocidos por lo que se puede decir que hay 
##sobre-ajuste para cada resultado de las bases de datos y que sean distintas las distribuciones
##entre los ajustes lm y knn.


##Comparativas múltiples con Friedman y post-hoc Holm
test_friedman <- friedman.test(as.matrix(tablatst)) 
test_friedman
##Existen diferencias significativas al menos entre un par de algoritmos ya que p-value es menor a 0.05

##Aplicamos post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
##Existen diferencias significativas a favor de M5´
##(3vs1 0.081 y 3vs2, con aprox. 90% confianza)
##lm vs knn, pueden ser considerados equivalentes

##Comparativas múltiples con Friedman y post-hoc Holm para train
test_friedman <- friedman.test(as.matrix(tablatra)) 
test_friedman
##Existen diferencias significativas al menos entre un par de algoritmos ya que p-value es menor a 0.05

##Aplicamos post-hoc Holm
tam <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
##Existen diferencias significativas a favor en todos los casos, lo que sugiere que cada ajuste se ha
##adaptado a la base de datos, por lo que puede significar un sobre-ajuste.




###############################################################
###############################################################
###############################################################
##################### C) CLASIFICACIÓN ########################
###############################################################
###############################################################

library(class)
library(kknn)
library(caret)
library(MASS)
rm(list=ls())
australian <- read.csv("australian/australian.dat", comment.char = "@", header = FALSE)
attach(australian)
###############################################################
############################ KNN ##############################
###############################################################

##Examinamos la estructura del dataset
str(australian)
##Vemos el número de variables para cada clase
table(australian$V15)
##Pasamos la V15 a factor
australian$V15 <- factor(australian$V15,levels = c(0,1), label = c(0,1))
##Vemos el porcentaje de cada clase para el dataset
round(prop.table(table(australian$V15))*100,digits=1)
##Normalizamos toda la base de datos
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
australian_n <- as.data.frame(lapply(australian[1:14],normalize))
##Visualizamos variables
plot(australian[,1:14])
plot(australian_n[,1:14],col=australian[,15])
##Calculamos la correlación entre variables
cor(australian[,1:14])
##Vemos que puede haber correlación entre las variables V10 y V9
##Creamos el conjunto de training y de test y sus etiquetas
tamanio_tt <- nrow(australian)*0.8
australian_train <- australian_n[1:tamanio_tt,-15]
australian_test <- australian_n[(tamanio_tt+1):nrow(australian),-15]
australian_train_labels <- australian[1:tamanio_tt,15]
australian_test_labels <- australian[(tamanio_tt+1):nrow(australian),15]
##Entrenamos un modelo de los datos
set.seed(1234)
australian_test_pred <- knn(train=australian_train, test=australian_test,cl=australian_train_labels,k=21)
##Evaluamos el modelo
table(australian_test_pred,australian_test_labels)
mean(australian_test_pred==australian_test_labels)
##Por lo que obtenemos un modelo que explica el 85,5% del problema
set.seed(1234)
australian_test_pred <- knn(train=australian_train, test=australian_test,cl=australian_train_labels,k=7)
##Evaluamos el modelo
table(australian_test_pred,australian_test_labels)
mean(australian_test_pred==australian_test_labels)
##Por lo que obtenemos un modelo que explica el 84,7% del problema

##Con caret podemos también hacer un modelo knn
require(caret)
set.seed(1234)
knnModel <- train(x = australian[1:tamanio_tt,-15], y = australian[1:tamanio_tt,15],method = "knn", preProc = c("center","scale"))
knnModel
##Siendo el mejor modelo k=9
knnPred <- predict(knnModel,newdata = australian[(tamanio_tt+1):nrow(australian),-15])
testData = australian[(tamanio_tt+1):nrow(australian),15]
postResample(pred=knnPred, obs = testData)
##Si aplicamos el conjunto de test sobre el modelo creado tenemos un modelo con el 84,05% de explicación
##Si lo hacemos con los datos normalizados como debería ser
set.seed(1234)
knnFit <- train(australian_train, australian_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
knnFit
##Si le decimos los valores de k que debe tomar, vemos como con k = 15 conseguimos el mejor modelo
knnPred <- predict(knnFit, newdata = australian_test)
postResample(pred=knnPred, obs = australian_test_labels)
##Consiguiendo un model con el 82,6% de explicación del problema

##Ahora si aplico estas funciones a los conjuntos de train y test facilitados podemos tener:
##Primero creo las dos funciones para hacer todo el proceso de forma automática
aplicar_metodo_fold <- function(train,train_labels,test,test_labels,metodo="knn"){
  if (metodo == "knn"){
    knnFit <- train(train, train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
    knnPred <- predict(knnFit, newdata = test)
    #print(postResample(pred=knnPred, obs = test_labels))
    postResample(pred=knnPred, obs = test_labels)
  }
  else if (metodo == "glm"){
    glmFit <- train(train, train_labels, method="glm", metric="Accuracy",trControl=trainControl(method="cv",number=10))
    glmPredict <- predict(glmFit, newdata = test)
    salida <- confusionMatrix(data = glmPredict, reference = test_labels)
    matriz_confusion <- table(glmPredict,test_labels)
    #print(mean(glmPredict==test_labels))
    mean(glmPredict==test_labels)
  }
  else if (metodo == "lda"){
    ldaFit <- train(train,train_labels,
                    method="lda",metric="Accuracy",preProcess=c("center","scale"),
                    tuneLength=10,trControl=trainControl(method="cv",number=10))
    ldaPredict <- predict(ldaFit, newdata = test)
    salida <- confusionMatrix(data = ldaPredict, reference = test_labels)
    matriz_confusion <- table(ldaPredict,test_labels)
    #print(mean(ldaPredict==test_labels))
    mean(ldaPredict==test_labels)
  }
  else if (metodo == "qda"){
    qdaFit <- train(train,train_labels,
                    method="qda",metric="Accuracy",preProcess=c("center","scale"),
                    tuneLength=10,trControl=trainControl(method="cv",number=10))
    qdaPredict <- predict(qdaFit, newdata = test)
    salida <- confusionMatrix(data = qdaPredict, reference = test_labels)
    matriz_confusion <- table(qdaPredict,test_labels)
    #print(mean(qdaPredict==test_labels))
    mean(qdaPredict==test_labels)
  }
}
run_metodo_fold <- function(i, x,metodo = "knn", tt="test",modelo=0){
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  x_tra <- data.frame(lapply(x_tra, normalize))
  x_tra$Y <- factor(x_tra$Y, levels = c(0, 1),labels = c("0", "1"))
  file <- paste(x, "-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  x_tst <- data.frame(lapply(x_tst, normalize))
  x_tst$Y <- factor(x_tst$Y, levels = c(0, 1), labels = c("0", "1"))
  ultimo = 15
  if (modelo == 1){
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    ultimo = 14
  }
  if (modelo == 2){
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    ultimo = 13
  }
  if (modelo == 3){
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    x_tra[,3]=NULL
    x_tst[,3]=NULL
    x_tra[,1]=NULL
    x_tst[,1]=NULL
    ultimo = 11
  }
  if (modelo == 4){
    x_tra[,12]=NULL
    x_tst[,12]=NULL
    x_tra[,11]=NULL
    x_tst[,11]=NULL
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    x_tra[,3]=NULL
    x_tst[,3]=NULL
    x_tra[,2]=NULL
    x_tst[,2]=NULL
    x_tra[,1]=NULL
    x_tst[,1]=NULL
    ultimo = 8
  }
  if (modelo == 5){
    x_tra[,12]=NULL
    x_tst[,12]=NULL
    x_tra[,11]=NULL
    x_tst[,11]=NULL
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,8] = x_tra[,8]*x_tra[,9]
    x_tra[,9] = NULL
    x_tst[,8] = x_tst[,8]*x_tst[,9]
    x_tst[,9] = NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    x_tra[,3]=NULL
    x_tst[,3]=NULL
    x_tra[,2]=NULL
    x_tst[,2]=NULL
    x_tra[,1]=NULL
    x_tst[,1]=NULL
    ultimo = 7
  }
  if (tt == "test"){
    test_labels <- x_tst[,ultimo]
    test <- x_tst[,-ultimo]
  }
  else {
    test_labels <- x_tra[,ultimo]
    test <- x_tra[,-ultimo]
  }
  train_labels <- x_tra[,ultimo]
  aplicar_metodo_fold(x_tra[,-ultimo],train_labels,test,test_labels,metodo)
}
##Una vez creadas las funciones, realizo los cálculos
set.seed(1234)
nombre <- "australian/australian"
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train"))
max_accuracy_train <- max(accuracy_train[1,])
max_accuracy_train #0.8806
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #0.8737

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test"))
max_accuracy_test <- max(accuracy_test[1,])
max_accuracy_test #0.9411
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #0.8558

mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn")


###############################################################
############################ GLM ##############################
###############################################################
set.seed(1234)
##Vamos a dividir el conjunto en train y test sin normalizar
total_train_glm <- length(australian$V15)*0.8
australian_train <- australian[1:total_train_glm,-15]
australian_train_labels <- as.factor(australian[1:total_train_glm,]$V15)
australian_test <- australian[(total_train_glm+1):length(australian$V15),-15]
australian_test_labels <- as.factor(australian[(total_train_glm+1):length(australian$V15),]$V15)
##Con caret directamente, hacemos el modelo de glm
glmFit <- train(australian_train, australian_train_labels, method="glm",metric="Accuracy",trControl=trainControl(method="cv",number=10))
#glmFit
##Añadimos los elementos de test y vemos el acierto
glmPredict <- predict(glmFit, newdata = australian_test)
confusionMatrix(data = glmPredict, reference = australian_test_labels)
matriz_confusion <- table(glmPredict,australian_test_labels)
accuracy <- mean(glmPredict==australian_test_labels)
accuracy
##Y hemos obtenido un modelo con un 87,68% de explicación del problema

##Realizo los cálculos sobre los conjuntos facilitados
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train"))
max_accuracy_train <- max(accuracy_train)
max_accuracy_train #0.8870
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #0.8791

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test"))
max_accuracy_test <- max(accuracy_test)
max_accuracy_test #0.8529
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #0.8176

mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm")

###############################################################
############################ LDA ##############################
###############################################################
set.seed(1234)
##Directamente realizo el modelo lda con caret:
ldaFit <- train(australian_train,australian_train_labels,
                method="lda",metric="Accuracy",preProcess=c("center","scale"),
                tuneLength=10,trControl=trainControl(method="cv",number=10))
summary(ldaFit)
ldaPredict <- predict(ldaFit, newdata = australian_test)
confusionMatrix(data = ldaPredict, reference = australian_test_labels)
##Obteniendo un accuracy del 0.8768 para este conjunto de train y test

##Realizo los cálculos para las particiones de train y test
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train"))
max_accuracy_train <- max(accuracy_train)
max_accuracy_train #0.8661
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #0.8603

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test"))
max_accuracy_test <- max(accuracy_test)
max_accuracy_test #0.9117
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #0.8514

mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda")

###############################################################
############################ QDA ##############################
###############################################################
set.seed(1234)
qdaFit <- train(australian_train,australian_train_labels,
                method="qda",metric="Accuracy",preProcess=c("center","scale"),
                tuneLength=10,trControl=trainControl(method="cv",number=10))
qdaPredict <- predict(qdaFit, newdata = australian_test)
confusionMatrix(data = qdaPredict, reference = australian_test_labels)
##Obteniendo un accuracy del 0.7826 para este conjunto de train y test

##Realizo los cálculos
set.seed(1234)
nombre <- "australian/australian"
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train"))
max_accuracy_train <- max(accuracy_train)
max_accuracy_train #0.8112
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #0.8064

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test"))
max_accuracy_test <- max(accuracy_test)
max_accuracy_test #0.8235
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#0.7676

mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda")

resultados_clasificacion0 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion0


###############################################################
################## Probando otros modelos #####################
###############################################################
##Como V10 y V9 vimos que estaban correladas por lo que voy a juntarlas, este es el modelo 1
australian_new =australian
australian_new[,9] = australian_new[,9]*australian_new[,10]
australian_new[,10]=NULL
summary(australian_new)
##Realizo los cálculos
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",1))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",1))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn1")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",1))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",1))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm1")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",1))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",1))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda1")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",1))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",1))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda1")

resultados_clasificacion1 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1)
resultados_clasificacion
##Por lo que podemos observar que las diferencias son mínimas, siendo un poco mejor el modelo con todas las variables

##Seguimos probando modelos:
cor(australian_new[,1:13])
##Podemos ver como las variables V5 y V6 tiene algo de correlación
cor(australian_new[,1:13])[5,6]
##Por lo tanto voy a probar a unirlas, este será el modelo 2
australian_new[,5] = australian_new[,5]*australian_new[,6]
australian_new[,6]=NULL

##Realizo los cálculos
modelo_actual=2
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn2")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm2")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda2")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda2")

resultados_clasificacion2 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2)
resultados_clasificacion
##Por lo que podemos observar que las diferencias son mínimas, siendo un poco mejor el modelo con todas las variables

##Si seguimos estudiando como mejorar el modelo podemos hacer pruebas, ya que no tenemos información de que es cada variable:
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
##Donde podemos ver que V5 y V9, que son las variables que hemos fusionado son muy importantes, por lo que 
##vamos a eliminar algunas de las que su p-value es mayor a 0.5 como V1 y V3. Este es el modelo 3
australian_new$V1=NULL
australian_new$V3=NULL
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
##Veamos como se comporta:
modelo_actual=3
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn3")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm3")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda3")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda3")

resultados_clasificacion3 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2,resultados_clasificacion3)
resultados_clasificacion
##Vemos como los resultados no han mejorado, por lo que vamos a probar otro modelo

glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
##vamos a eliminar algunas de las que su p-value es mayor a 0.2 como V2, V11 y V12
australian_new$V2=NULL
australian_new$V11=NULL
australian_new$V12=NULL
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
cor(australian_new[,1:7])
##Donde veo como V7, que es la que tiene un p-value mayor, no guarda correlación con ninguna, por lo que también la eliminaré. Modelo 4
australian_new$V7=NULL

##Veamos como se comporta:
modelo_actual=4
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn4")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm4")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda4")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda4")

resultados_clasificacion4 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2,resultados_clasificacion3,resultados_clasificacion4)
resultados_clasificacion

##Veamos que tal esta el modelo actual
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
cor(australian_new[,1:6])
##Viendo las correlaciones, podemos observar la que hay entre la variable V8 y V9 (V9*V10)
cor(australian_new[,1:6])[3,4]
##Por lo que voy a probar un último modelo con ambas variables unidas V8*V9*V10. Modelo 5

##Veamos como se comporta:
modelo_actual=5
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn5")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm5")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda5")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda5")

resultados_clasificacion5 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2,resultados_clasificacion3,resultados_clasificacion4,resultados_clasificacion5)
resultados_clasificacion
##Viendo que los resultados de este último modelo son peores

##Hagamos una comparación de nuestros algoritmos para encontrar el mejor modelo
data_result_clasif <- as.data.frame(resultados_clasificacion)
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
mejor_knn_train <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[1,]
mejor_knn_test <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[2,]
##Donde vemos que el modelo con mayor Accuracy en test es el modelo 4, con knn
data_result_clasif$knn=NULL;data_result_clasif$knn1=NULL;data_result_clasif$knn2=NULL;data_result_clasif$knn3=NULL;data_result_clasif$knn4=NULL;data_result_clasif$knn5=NULL
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
mejor_lda_train <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[1,]
mejor_lda_test <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[2,]
##El segundo mejor modelo, sin knn, es el modelo 0 para lda
data_result_clasif$lda=NULL;data_result_clasif$lda1=NULL;data_result_clasif$lda2=NULL;data_result_clasif$lda3=NULL;data_result_clasif$lda4=NULL;data_result_clasif$lda5=NULL
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
##Para glm vemos como el mejor modelo es el modelo 4
data_result_clasif$glm=NULL;data_result_clasif$glm1=NULL;data_result_clasif$glm2=NULL;data_result_clasif$glm3=NULL;data_result_clasif$glm4=NULL;data_result_clasif$glm5=NULL
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
mejor_qda_train <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[1,]
mejor_qda_test <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[2,]
##Por último, para qda, el mejor modelo es el modelo 5
##Por lo que, nuestro mejor modelo ha sido el modelo 4 en el que hacimos la unión de las variables V10 con V9 y V5 con V6, y eliminabamos
##las variables V12, V11, V3, V2 y V1, usando knn


###############################################################
######################### COMPARACIÓN #########################
###############################################################
##leemos la tabla con los errores medios de test
resultados_test <- read.csv("clasif_test_alumos.csv")
tablatst <- cbind(resultados_test[,2:dim(resultados_test)[2]])
colnames(tablatst) <- names(resultados_test)[2:dim(resultados_test)[2]]
rownames(tablatst) <- resultados_test[,1]
resultados_test[2,]

##leemos la tabla con los errores medios de train
resultados_train <- read.csv("clasif_train_alumnos.csv")
tablatra <- cbind(resultados_train[,2:dim(resultados_train)[2]])
colnames(tablatra) <- names(resultados_train)[2:dim(resultados_train)[2]]
rownames(tablatra) <- resultados_train[,1]
resultados_train[2,]

##Añadir los valores resultantes en test
tablatst[2,][1] <- mejor_knn_test
tablatst[2,][2] <- mejor_lda_test
tablatst[2,][3] <- mejor_qda_test
tablatst[2,]

##Añadir los valores resultantes en train
tablatra[2,][1] <- mejor_knn_train
tablatra[2,][2] <- mejor_lda_train
tablatra[2,][3] <- mejor_qda_train
tablatra[2,]

##Comparativa por pares de LDA y QDA para test (Wilcoxon´s test)
##LDA (other) vs QDA (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,2] - tablatst[,3]) / tablatst[,2]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatst)[2], colnames(tablatst)[3])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LDAvsQDAtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LDAvsQDAtst$statistic
pvalue <- LDAvsQDAtst$p.value
LDAvsQDAtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LDAvsQDAtst$statistic
Rmas
Rmenos
pvalue
##Al ser p-valor mayor a 0.05 no existen diferencias significativas entre ambos
##Solo un 21,58% de confianza de que existan diferencias ((1-pvalue)*100)
##Como el p-value es 0.7841, mayor que el nivel de signifiación 0.05, no somos capaces
##a rechazar la hipótesis nula y concluir que no hay suficiente evidencia en los datos.
##Por lo tanto se acepta la hipotesis nula: las dos distribuciones son iguales

##Comparativa por pares de LDA y QDA para train (Wilcoxon´s test)
##LDA (other) vs QDA (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatra[,2] - tablatra[,3]) / tablatra[,2]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatra)[2], colnames(tablatra)[3])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LDAvsQDAtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LDAvsQDAtst$statistic
pvalue <- LDAvsQDAtst$p.value
LDAvsQDAtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LDAvsQDAtst$statistic
Rmas
Rmenos
pvalue
##Al igual que en test, para el train, no existen diferencias significativas al ser
##p-value = 0.1536, mayor a 0.05.


##Comparativas múltiples con Friedman y post-hoc Holm
test_friedman <- friedman.test(as.matrix(tablatst)) 
test_friedman
##p-value = 0.9512, mayor que 0.05 por lo que se acepta la hipótesis nula, no existen
##diferencias significativas entre los 3 algoritmos.

##Aplicamos post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
##No existen diferencias significativas como indicaba el test de Friedman

##Comparativas múltiples con Friedman y post-hoc Holm para train
test_friedman <- friedman.test(as.matrix(tablatra)) 
test_friedman
##No existen diferencias significativas entre los algoritmos al ser p-value = 0.522, mayor que 0.05

##Aplicamos post-hoc Holm
tam <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
##No existen diferencias significativas como indicaba el test de Friedman

